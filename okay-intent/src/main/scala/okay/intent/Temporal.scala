package okay.intent

import okay.frame.Found

/**
 * Temporal phrases to dates (specs/intent-classify.md).
 *
 * A slot typed as ISO-8601 refuses "next thursday", so today the model
 * does the conversion and the schema only checks it. That is a model
 * doing arithmetic, which is the one thing it is worst at and the one
 * thing a parser is best at.
 *
 * NOT built on `okay-lex`'s `Scan`, deliberately: that machinery earns
 * its keep by carrying lexer state across chunk boundaries and
 * relexing incrementally after an edit, and a five-word phrase has
 * neither. What it needs is to be TOTAL and DETERMINISTIC, which is a
 * function.
 *
 * Deterministic means the reference day is an ARGUMENT. "Next
 * Thursday" is not a value; it is a value relative to a day someone
 * has to name, and a parser that reads the clock cannot be tested.
 *
 * Total means `None` rather than a guess. A wrong date is acted on —
 * a meeting booked, a deadline moved — while a refused one is asked
 * about, so declining is the cheap failure and guessing is the
 * expensive one.
 */
object Temporal {

  /** a civil date; no dependency on java.time, which okay-agent's JS
   * build does not have */
  final case class Date(year: Int, month: Int, day: Int):
    def iso: String = f"$year%04d-$month%02d-$day%02d"

  /** a resolved phrase: a day, and a time within it when one was said */
  final case class When(date: Date, hour: Option[Int] = None, minute: Int = 0):
    def iso: String = hour match
      case Some(h) => f"${date.iso}T$h%02d:$minute%02d"
      case None => date.iso

  /** an interval of days, both ends inclusive — what a week, a
   * weekend and a month ARE, and what a day is not. Printed the way
   * ISO 8601 prints an interval, `from/to`. */
  final case class Period(from: Date, to: Date):
    def iso: String = s"${from.iso}/${to.iso}"
    def contains(d: Date): Boolean =
      toEpochDay(from) <= toEpochDay(d) && toEpochDay(d) <= toEpochDay(to)

  private val weekdays = Vector(
    "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

  private val months = Vector(
    "january", "february", "march", "april", "may", "june",
    "july", "august", "september", "october", "november", "december")

  /**
   * Days since 1970-01-01, and back — Howard Hinnant's civil algorithm,
   * exact for the proleptic Gregorian calendar and free of the month
   * tables and leap-year branches that a hand-rolled version gets
   * wrong at exactly the dates nobody tests.
   */
  def toEpochDay(d: Date): Long =
    val y = if d.month <= 2 then d.year - 1 else d.year
    val era = (if y >= 0 then y else y - 399) / 400
    val yoe = y - era * 400
    val mp = (d.month + 9) % 12
    val doy = (153 * mp + 2) / 5 + d.day - 1
    val doe = yoe * 365 + yoe / 4 - yoe / 100 + doy
    era.toLong * 146097 + doe - 719468

  def fromEpochDay(days: Long): Date =
    val z = days + 719468
    val era = (if z >= 0 then z else z - 146096) / 146097
    val doe = z - era * 146097
    val yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
    val y = yoe + era * 400
    val doy = doe - (365 * yoe + yoe / 4 - yoe / 100)
    val mp = (5 * doy + 2) / 153
    val d = doy - (153 * mp + 2) / 5 + 1
    val m = if mp < 10 then mp + 3 else mp - 9
    Date((if m <= 2 then y + 1 else y).toInt, m.toInt, d.toInt)

  /** 0 = Monday, matching `weekdays` */
  def dayOfWeek(d: Date): Int =
    val x = (toEpochDay(d) + 3) % 7
    (if x < 0 then x + 7 else x).toInt

  def plusDays(d: Date, n: Int): Date = fromEpochDay(toEpochDay(d) + n)

  private val digits = raw"(\d{1,4})".r
  private val isoDate = raw"(\d{4})-(\d{2})-(\d{2})".r
  private val hhmm = raw"(\d{1,2}):(\d{2})".r
  private val oClock = raw"(\d{1,2})(am|pm)".r

  /**
   * Parse a phrase relative to `today`.
   *
   * The shapes are the ones scheduling mail actually uses, and the
   * list is the scope: an explicit ISO date, `today` / `tomorrow` /
   * `the day after tomorrow` / `yesterday`, `in N days` and `N days
   * from now`, a bare or qualified weekday (`thursday`, `next
   * thursday`, `this thursday`, `last thursday`), `next week`, and a
   * month-and-day (`March 14`, `14 March`). A time may follow any of
   * them (`at 2pm`, `at 14:00`, `2pm`).
   *
   * Everything else is `None`, including phrases this could plausibly
   * guess at ("soon", "end of the month", "the 14th") — see the class
   * comment for why declining is the cheap failure.
   */
  def parse(phrase: String, today: Date): Option[When] =
    val words = phrase.toLowerCase
      .replaceAll("[.,!?;]", " ")
      .split("\\s+").filter(_.nonEmpty)
      // a POSSESSIVE is the same word wearing a genitive: "tomorrow's
      // meeting" and "Thursday's invite" carried a date this parser
      // could not see, because the token did not match (measured,
      // intent-offline-slots). `o'clock` is untouched — only a
      // trailing apostrophe-s goes.
      .map(w => if w.endsWith("'s") || w.endsWith("\u2019s") then w.dropRight(2) else w)
      .filter(_.nonEmpty).toList
    if words.isEmpty then None
    else
      val time = timeIn(words)
      dateIn(words, today).map(d => time match
        case Some((h, m)) => When(d, Some(h), m)
        case None => When(d))
      .orElse(Multilingual.parse(phrase, today))

  private def timeIn(words: List[String]): Option[(Int, Int)] =
    words.collectFirst {
      case hhmm(h, m) if h.toInt < 24 && m.toInt < 60 => (h.toInt, m.toInt)
      case oClock(h, ap) if h.toInt >= 1 && h.toInt <= 12 =>
        val base = h.toInt % 12
        ((if ap == "pm" then base + 12 else base), 0)
    }

  private def dateIn(words: List[String], today: Date): Option[Date] =
    // an explicit date wins over anything relative: it is not ambiguous
    words.collectFirst { case isoDate(y, m, d) => Date(y.toInt, m.toInt, d.toInt) }
      .orElse(monthAndDay(words, today))
      .orElse(relative(words, today))

  private def monthAndDay(words: List[String], today: Date): Option[Date] =
    def dayNear(m: Int, d: Int): Date =
      // no year said: the coming one, so "March 14" in December means
      // next March rather than a date nine months past
      val thisYear = Date(today.year, m, d)
      if toEpochDay(thisYear) >= toEpochDay(today) then thisYear
      else Date(today.year + 1, m, d)
    val idx = words.indexWhere(w => months.exists(_.startsWith(w)) && w.length >= 3)
    if idx < 0 then None
    else
      val m = months.indexWhere(_.startsWith(words(idx))) + 1
      val after = words.lift(idx + 1).collect { case digits(d) if d.toInt <= 31 => d.toInt }
      val before = words.lift(idx - 1).collect { case digits(d) if d.toInt <= 31 => d.toInt }
      after.orElse(before).map(d => dayNear(m, d))

  private def relative(words: List[String], today: Date): Option[Date] =
    val set = words.toSet
    def weekdayAt(i: Int): Option[Int] =
      val raw = words(i)
      // a PLURAL weekday is still that weekday: "Thursdays are remote
      // from now on" names Thursday (measured, intent-offline-slots).
      // Tried second, so nothing that matched before changes.
      val w = if weekdays.exists(_.startsWith(raw)) || raw.length < 4 then raw else raw.stripSuffix("s")
      val hit = weekdays.indexWhere(_.startsWith(w))
      if hit >= 0 && w.length >= 3 then Some(hit) else None

    if set.contains("tomorrow") then
      Some(plusDays(today, if set.contains("after") then 2 else 1))
    else if set.contains("yesterday") then Some(plusDays(today, -1))
    else if set.contains("today") then Some(today)
    else
      // "in 3 days" / "3 days from now"
      val nDays =
        if set.contains("days") || set.contains("day") then
          words.collectFirst { case digits(n) if n.toInt <= 366 => n.toInt }
        else None
      nDays.map(n => plusDays(today, if set.contains("ago") then -n else n)).orElse {
        val at = words.indices.collectFirst { case i if weekdayAt(i).isDefined => i }
        at.flatMap(i => weekdayAt(i).map { target =>
          val qualifier = words.lift(i - 1).getOrElse("")
          val delta = (target - dayOfWeek(today) + 7) % 7
          qualifier match
            // "last thursday" goes backwards; "next thursday" is the
            // one in the week ahead, so a same-day match still moves
            case "last" => plusDays(today, if delta == 0 then -7 else delta - 7)
            case "next" => plusDays(today, if delta == 0 then 7 else delta)
            case _ => plusDays(today, if delta == 0 then 7 else delta)
        })
      }.orElse(if set.contains("week") && set.contains("next") then Some(plusDays(today, 7)) else None)

  /**
   * The same answer as `parse`, plus the words it rests on.
   *
   * The value is `parse`'s own verdict over the WHOLE message, so this
   * cannot disagree with the parser; only the evidence is searched
   * for. That ordering is the design. Sliding a window and taking the
   * first or the longest hit would be a SECOND parser: a window can
   * read to something the whole sentence would not, and then a frame
   * filled by extraction and the same frame filled by asking would
   * hold different dates.
   *
   * The span is the SHORTEST window of words reproducing that value —
   * minimal evidence for an answer already given. It always exists,
   * since the whole message is one of the windows.
   *
   * O(n^2) parses of very short strings, on a message that is being
   * classified anyway; the classifier is the expensive thing on this
   * path, not this.
   */
  def find(message: String, today: Date): Option[Found[When]] =
    parse(message, today).map { v =>
      val toks = message.split("\\s+").filter(_.nonEmpty)
      val windows =
        for len <- 1 to toks.length; i <- 0 to toks.length - len yield (i, len)
      val span = windows
        .find((i, len) => parse(toks.slice(i, i + len).mkString(" "), today).contains(v))
        .map((i, len) => toks.slice(i, i + len).mkString(" "))
        .getOrElse(message)
      Found(span.replaceAll("^[\\p{Punct}]+|[\\p{Punct}]+$", "").trim, v)
    }

  /**
   * A PERIOD, beside the day (intent-periods-and-zl).
   *
   * `parse` refuses «later this week» on purpose: a day it would have
   * to guess, and a guessed day is acted on. But «на этой неделе» is
   * not a guess — it names Monday to Sunday exactly, and the parser
   * that answered it with a DAY would be the one guessing. So the
   * shapes that are intervals get a value that is an interval: this /
   * next / last week, the weekend (the Saturday and Sunday of the
   * week named), and a month named without a day — the coming one, as
   * `monthAndDay` takes the coming year. Everything else is `None`: a
   * weekday is a day and `parse` has it, a date is a date, «soon» is
   * nothing. `parse` is untouched; «next week» still answers a day
   * there, because a consumer that asked for a day gets one.
   *
   * Measured by a consumer over 317 live messages (okay-chat,
   * specs/meaning.md): six what's-on questions named a week or a
   * weekend, and nothing here could say so.
   */
  def period(phrase: String, today: Date): Option[Period] =
    Multilingual.period(phrase, today)

  /** `find`'s evidence rule over `period`: the value is the whole
   * message's, and the span is the shortest window of words that
   * reproduces it */
  def findPeriod(message: String, today: Date): Option[Found[Period]] =
    period(message, today).map { v =>
      val toks = message.split("\\s+").filter(_.nonEmpty)
      val windows =
        for len <- 1 to toks.length; i <- 0 to toks.length - len yield (i, len)
      val span = windows
        .find((i, len) => period(toks.slice(i, i + len).mkString(" "), today).contains(v))
        .map((i, len) => toks.slice(i, i + len).mkString(" "))
        .getOrElse(message)
      Found(span.replaceAll("^[\\p{Punct}]+|[\\p{Punct}]+$", "").trim, v)
    }

  /** the Monday of the week a day falls in */
  private def mondayOf(d: Date): Date = plusDays(d, -dayOfWeek(d))

  /** the last day of a month, from the first day of the next */
  private def lastDayOf(year: Int, month: Int): Date =
    val next = if month == 12 then Date(year + 1, 1, 1) else Date(year, month + 1, 1)
    plusDays(next, -1)

  /**
   * The other seven languages of the parallel fixture
   * (intent-temporal-multilingual). The same shapes as the English
   * parser, over a lexicon per language: weekday, month and
   * relative-day words as PREFIXES of a token, so an inflection or a
   * German compound (`Freitagvormittag`, `четвергам`, `Jutrzejsza`)
   * is the word it starts with; the qualifier (`prochain`, `nächste`,
   * `следующ…`) before or after the weekday; `N jours` / `vor N
   * Tagen` / `через N дня`; the next-week pair; `15h`, `15 Uhr`, `15時`.
   * Japanese has no spaces, so it is scanned as a string, with the
   * `M月D日` and `N日後` shapes as regexes.
   *
   * English is tried first and unchanged; a phrase reaches this only
   * when English declined. Ambiguity is decided the way the parser
   * decides everything: a weekday beats the tomorrow-word (`el viernes
   * por la mañana` is Friday; `mañana` alone is tomorrow), and what a
   * lexicon does not say is `None`, as before.
   */
  private object Multilingual:
    final case class Lexicon(
      weekdays: Vector[Seq[String]], months: Vector[Seq[String]],
      today: Seq[String], tomorrow: Seq[String], dayAfter: Seq[String], yesterday: Seq[String],
      next: Seq[String], last: Seq[String], week: Seq[String], days: Seq[String], ago: Seq[String],
      /** the weekend, as prefixes; a language that says it in several
       * words (`fin de semana`) says so in `weekendPhrase` instead */
      weekend: Seq[String] = Seq.empty,
      weekendPhrase: Seq[List[String]] = Seq.empty,
      /** the month's OWN forms, whole words, for a month named with
       * no day beside it. `months` are prefixes, and `monthAndDay` can
       * afford a prefix because a day number stands next to it; alone,
       * «майстер» starts with «май» and «лютни» with «лют», and a
       * consumer's live log read a luthier as February. Empty means
       * the prefixes are whole names already (fr, de, es) */
      monthWords: Vector[Seq[String]] = Vector.empty)

    private def is(tok: String, forms: Seq[String]): Boolean = forms.exists(f => tok.startsWith(f))
    private def has(words: List[String], forms: Seq[String]): Boolean = words.exists(w => is(w, forms))

    val fr = Lexicon(
      Vector(Seq("lundi"), Seq("mardi"), Seq("mercredi"), Seq("jeudi"), Seq("vendredi"), Seq("samedi"), Seq("dimanche")),
      Vector(Seq("janvier"), Seq("février", "fevrier"), Seq("mars"), Seq("avril"), Seq("mai"), Seq("juin"),
        Seq("juillet"), Seq("août", "aout"), Seq("septembre"), Seq("octobre"), Seq("novembre"), Seq("décembre", "decembre")),
      today = Seq("aujourd'hui", "aujourd’hui"), tomorrow = Seq("demain"), dayAfter = Seq("après-demain", "apres-demain"),
      yesterday = Seq("hier"), next = Seq("prochain"), last = Seq("dernier", "dernière"), week = Seq("semaine"),
      days = Seq("jour"), ago = Seq("il"), weekend = Seq("week-end", "weekend"))
    val de = Lexicon(
      Vector(Seq("montag"), Seq("dienstag"), Seq("mittwoch"), Seq("donnerstag"), Seq("freitag"), Seq("samstag", "sonnabend"), Seq("sonntag")),
      Vector(Seq("januar", "jänner"), Seq("februar"), Seq("märz", "maerz"), Seq("april"), Seq("mai"), Seq("juni"),
        Seq("juli"), Seq("august"), Seq("september"), Seq("oktober"), Seq("november"), Seq("dezember")),
      today = Seq("heute", "heutig"), tomorrow = Seq("morgen", "morgig"), dayAfter = Seq("übermorgen", "uebermorgen"),
      yesterday = Seq("gestern", "gestrig"), next = Seq("nächst", "naechst", "kommend"), last = Seq("letzt", "vergangen"),
      week = Seq("woche"), days = Seq("tag"), ago = Seq("vor"), weekend = Seq("wochenend"))
    val es = Lexicon(
      Vector(Seq("lunes"), Seq("martes"), Seq("miércoles", "miercoles"), Seq("jueves"), Seq("viernes"), Seq("sábado", "sabado"), Seq("domingo")),
      Vector(Seq("enero"), Seq("febrero"), Seq("marzo"), Seq("abril"), Seq("mayo"), Seq("junio"),
        Seq("julio"), Seq("agosto"), Seq("septiembre", "setiembre"), Seq("octubre"), Seq("noviembre"), Seq("diciembre")),
      today = Seq("hoy"), tomorrow = Seq("mañana", "manana"), dayAfter = Seq("pasado"), yesterday = Seq("ayer"),
      next = Seq("próxim", "proxim", "siguiente"), last = Seq("pasad"), week = Seq("semana"), days = Seq("día", "dia"), ago = Seq("hace"),
      weekend = Seq("finde"), weekendPhrase = Seq(List("fin", "de", "semana")))
    val ru = Lexicon(
      Vector(Seq("понедельник"), Seq("вторник"), Seq("сред"), Seq("четверг"), Seq("пятниц"), Seq("суббот"), Seq("воскресень")),
      Vector(Seq("январ"), Seq("феврал"), Seq("март"), Seq("апрел"), Seq("мая", "май"), Seq("июн"),
        Seq("июл"), Seq("август"), Seq("сентябр"), Seq("октябр"), Seq("ноябр"), Seq("декабр")),
      today = Seq("сегодня"), tomorrow = Seq("завтра"), dayAfter = Seq("послезавтра"), yesterday = Seq("вчера"),
      next = Seq("следующ", "будущ"), last = Seq("прошл", "прошедш"), week = Seq("недел"), days = Seq("дн", "день"), ago = Seq("назад"),
      weekend = Seq("выходн", "уикенд", "уик-энд"),
      monthWords = Vector(Seq("январь", "января", "январе"), Seq("февраль", "февраля", "феврале"), Seq("март", "марта", "марте"),
        Seq("апрель", "апреля", "апреле"), Seq("май", "мая", "мае"), Seq("июнь", "июня", "июне"), Seq("июль", "июля", "июле"),
        Seq("август", "августа", "августе"), Seq("сентябрь", "сентября", "сентябре"), Seq("октябрь", "октября", "октябре"),
        Seq("ноябрь", "ноября", "ноябре"), Seq("декабрь", "декабря", "декабре")))
    val uk = Lexicon(
      Vector(Seq("понеділ", "щопонеділ"), Seq("вівтор", "щовівтор"), Seq("серед", "щосеред"), Seq("четвер", "щочетверг"),
        Seq("п'ятниц", "п’ятниц", "щоп'ятниц", "щоп’ятниц"), Seq("субот", "щосубот"), Seq("неділ", "щонеділ")),
      Vector(Seq("січ"), Seq("лют"), Seq("берез"), Seq("квіт"), Seq("трав"), Seq("черв"),
        Seq("лип"), Seq("серп"), Seq("верес"), Seq("жовт"), Seq("листопад"), Seq("груд")),
      today = Seq("сьогодні"), tomorrow = Seq("завтра"), dayAfter = Seq("післязавтра"), yesterday = Seq("вчора"),
      next = Seq("наступн"), last = Seq("минул", "попередн"), week = Seq("тижд", "тижн"), days = Seq("дн", "день"), ago = Seq("тому"),
      weekend = Seq("вихідн", "вікенд"),
      monthWords = Vector(Seq("січень", "січня", "січні"), Seq("лютий", "лютого", "лютому"), Seq("березень", "березня", "березні"),
        Seq("квітень", "квітня", "квітні"), Seq("травень", "травня", "травні"), Seq("червень", "червня", "червні"),
        Seq("липень", "липня", "липні"), Seq("серпень", "серпня", "серпні"), Seq("вересень", "вересня", "вересні"),
        Seq("жовтень", "жовтня", "жовтні"), Seq("листопад", "листопада", "листопаді"), Seq("грудень", "грудня", "грудні")))
    val pl = Lexicon(
      Vector(Seq("poniedział"), Seq("wtor"), Seq("środ", "srod"), Seq("czwart"), Seq("piąt", "piat"), Seq("sobot"), Seq("niedziel")),
      Vector(Seq("stycz"), Seq("lut"), Seq("marz", "marc"), Seq("kwiet"), Seq("maj"), Seq("czerw"),
        Seq("lip"), Seq("sierp"), Seq("wrze"), Seq("październik", "pazdziernik"), Seq("listopad"), Seq("grud")),
      today = Seq("dziś", "dzisiaj"), tomorrow = Seq("jutr"), dayAfter = Seq("pojutrze"), yesterday = Seq("wczoraj"),
      next = Seq("przyszł", "przyszl", "następn", "nastepn"), last = Seq("zeszł", "zeszl", "ostatn", "poprzedn"),
      week = Seq("tydz", "tygod"), days = Seq("dni", "dzień", "dzien"), ago = Seq("temu"), weekend = Seq("weekend"),
      monthWords = Vector(Seq("styczeń", "stycznia", "styczniu"), Seq("luty", "lutego", "lutym"), Seq("marzec", "marca", "marcu"),
        Seq("kwiecień", "kwietnia", "kwietniu"), Seq("maj", "maja", "maju"), Seq("czerwiec", "czerwca", "czerwcu"),
        Seq("lipiec", "lipca", "lipcu"), Seq("sierpień", "sierpnia", "sierpniu"), Seq("wrzesień", "września", "wrześniu"),
        Seq("październik", "października", "październiku"), Seq("listopad", "listopada", "listopadzie"),
        Seq("grudzień", "grudnia", "grudniu")))
    val lexicons = Vector(fr, de, es, ru, uk, pl)

    /** English, for PERIODS only — the day parser has its own English
     * and this is deliberately not in `lexicons`, so nothing `parse`
     * answers changes */
    val en = Lexicon(
      weekdays.map(Seq(_)), months.map(Seq(_)),
      today = Seq("today"), tomorrow = Seq("tomorrow"), dayAfter = Seq("after"), yesterday = Seq("yesterday"),
      next = Seq("next", "coming", "following"), last = Seq("last", "past", "previous"), week = Seq("week"),
      days = Seq("day"), ago = Seq("ago"), weekend = Seq("weekend", "week-end"),
      monthWords = months.map(Seq(_)))
    private val periodLexicons = en +: lexicons

    /**
     * The interval a phrase names, if it names one (`Temporal.period`).
     *
     * The weekend is read BEFORE the week, and the reason is a prefix:
     * `wochenende`, `week-end` and `weekend` all start with the week
     * word, so a parser that read the week first would call the
     * weekend a week. A month counts only with no day beside it —
     * with one it is a date, and `parse` has it.
     */
    def period(phrase: String, today: Date): Option[Period] =
      val words = tokens(phrase)
      if words.isEmpty then None
      else periodLexicons.iterator.map(lex => periodIn(words, today, lex)).collectFirst { case Some(p) => p }

    private def periodIn(words: List[String], today: Date, lex: Lexicon): Option[Period] =
      val monday = mondayOf(today)
      // the qualifier is read from every lexicon, as `dateIn` reads it —
      // and from this one, since English is not in `anyLast`
      val shift =
        if has(words, lex.next) then 7
        else if has(words, lex.last) || has(words, anyLast) then -7
        else 0
      def week(of: Date) = Period(of, plusDays(of, 6))
      def weekend(of: Date) = Period(plusDays(of, 5), plusDays(of, 6))
      val start = plusDays(monday, shift)
      if has(words, lex.weekend) || lex.weekendPhrase.exists(words.containsSlice) then Some(weekend(start))
      else if has(words, lex.week) then Some(week(start))
      else monthAlone(words, today, lex)

    /** the English «may» is a verb far more often than a month, and a
     * bare one is the verb; the month wears one of these before it */
    private val beforeMay = Set("in", "during", "by", "until", "till", "for", "from", "of", "this", "next", "last", "since")

    /**
     * A month named with no day beside it — the coming one.
     *
     * Matched against the month's WHOLE forms, never the prefixes
     * `monthAndDay` uses: there a day number stands beside the month
     * and settles it; here nothing does, and «майстер», «лютни»,
     * «lutnia» all begin with a month. Found on a consumer's live log
     * the evening the period parser landed.
     */
    private def monthAlone(words: List[String], today: Date, lex: Lexicon): Option[Period] =
      val forms = if lex.monthWords.nonEmpty then lex.monthWords else lex.months
      val idx = words.indexWhere(w => forms.exists(_.contains(w)))
      if idx < 0 then None
      else
        // ANY number beside the month makes it somebody's date or range,
        // never the bare month: «с 12 по 40 сентября» is an impossible
        // day the day parser refuses on purpose, and «12-14 сентября» is
        // a range in one token — both read as the whole of September
        // when only 1..31 counted as a neighbour (found by okay-chat's
        // TestWhen the moment its reader asked for periods)
        def numberAt(j: Int) = words.lift(j).exists(_.exists(_.isDigit))
        val bareMay = words(idx) == "may" && !words.lift(idx - 1).exists(beforeMay)
        if numberAt(idx + 1) || numberAt(idx - 1) || bareMay then None
        else
          val m = forms.indexWhere(_.contains(words(idx))) + 1
          val year = if toEpochDay(lastDayOf(today.year, m)) >= toEpochDay(today) then today.year else today.year + 1
          Some(Period(Date(year, m, 1), lastDayOf(year, m)))
    // qualifiers are the words most alike across the Cyrillic and Slavic
    // pairs (ru "четверг" is a prefix of uk "четверга"), so a lexicon that
    // finds the weekday reads the qualifier from EVERY lexicon: "минулого"
    // is last whichever list named the day
    private val anyLast = lexicons.flatMap(_.last)
    private val anyAgo = lexicons.flatMap(_.ago)

    private val hourFr = raw"(\d{1,2})h(\d{2})?".r
    private val hourJa = raw"(\d{1,2})時(?:(\d{1,2})分)?".r
    private val monthDayJa = raw"(\d{1,2})月(\d{1,2})日".r
    private val daysAfterJa = raw"(\d{1,3})日後".r
    private val daysBeforeJa = raw"(\d{1,3})日前".r

    private def tokens(phrase: String): List[String] =
      phrase.toLowerCase
        .replaceAll("[.,!?;¿¡«»\"()]", " ")   // not the colon: 15:00 is a time
        .split("\\s+").filter(_.nonEmpty).toList
        .map(_.replaceAll("^[dl][’']", "")) // l'appel, d'hier

    def parse(phrase: String, today: Date): Option[When] =
      val words = tokens(phrase)
      val time = timeIn(words).orElse(words.collectFirst {
        case hourFr(h, m) if h.toInt < 24 => (h.toInt, Option(m).map(_.toInt).getOrElse(0))
      }).orElse {
        val i = words.indexWhere(_ == "uhr")
        if i > 0 then words(i - 1).toIntOption.filter(_ < 24).map(h => (h, 0)) else None
      }
      val date = lexicons.iterator.map(lex => dateIn(words, today, lex)).collectFirst { case Some(d) => d }
        .orElse(japanese(phrase, today))
      date.map(d => time.orElse(japaneseTime(phrase)) match
        case Some((h, m)) => When(d, Some(h), m)
        case None => When(d))

    private def dateIn(words: List[String], today: Date, lex: Lexicon): Option[Date] =
      def weekdayAt(i: Int): Option[Int] =
        val hit = lex.weekdays.indexWhere(forms => is(words(i), forms))
        if hit >= 0 then Some(hit) else None
      val weekday = words.indices.collectFirst { case i if weekdayAt(i).isDefined => i }.flatMap { i =>
        weekdayAt(i).map { target =>
          val around = List(words.lift(i - 1), words.lift(i + 1), words.lift(i - 2)).flatten
          val delta = (target - dayOfWeek(today) + 7) % 7
          if around.exists(w => is(w, anyLast)) then plusDays(today, if delta == 0 then -7 else delta - 7)
          else plusDays(today, if delta == 0 then 7 else delta)
        }
      }
      weekday.orElse(monthAndDay(words, today, lex)).orElse(relative(words, today, lex))

    private def monthAndDay(words: List[String], today: Date, lex: Lexicon): Option[Date] =
      val idx = words.indexWhere(w => lex.months.exists(forms => is(w, forms)))
      if idx < 0 then None
      else
        val m = lex.months.indexWhere(forms => is(words(idx), forms)) + 1
        def dayAt(j: Int) = words.lift(j).collect { case digits(d) if d.toInt >= 1 && d.toInt <= 31 => d.toInt }
        dayAt(idx + 1).orElse(dayAt(idx - 1)).map { d =>
          val thisYear = Date(today.year, m, d)
          if toEpochDay(thisYear) >= toEpochDay(today) then thisYear else Date(today.year + 1, m, d)
        }

    private def relative(words: List[String], today: Date, lex: Lexicon): Option[Date] =
      // "pasado mañana" is the day after tomorrow; "pasado" alone is a qualifier
      val dayAfter = words.sliding(2).exists(p => p.length == 2 && is(p(0), lex.dayAfter) && is(p(1), lex.tomorrow)) ||
        (lex.dayAfter.exists(_.length > 6) && has(words, lex.dayAfter))
      if dayAfter then Some(plusDays(today, 2))
      else if has(words, lex.tomorrow) then Some(plusDays(today, 1))
      else if has(words, lex.yesterday) then Some(plusDays(today, -1))
      else if has(words, lex.today) then Some(today)
      else
        val nDays =
          if has(words, lex.days) then words.collectFirst { case digits(n) if n.toInt <= 366 => n.toInt } else None
        nDays.map(n => plusDays(today, if has(words, anyAgo) then -n else n))
          .orElse(if has(words, lex.week) && has(words, lex.next) then Some(plusDays(today, 7))
                  else if has(words, lex.week) && has(words, anyLast) then Some(plusDays(today, -7))
                  else None)

    private val jaWeekdays = Vector("月曜", "火曜", "水曜", "木曜", "金曜", "土曜", "日曜")

    private def japanese(phrase: String, today: Date): Option[Date] =
      val s = phrase
      monthDayJa.findFirstMatchIn(s).map { m =>
        val (mo, d) = (m.group(1).toInt, m.group(2).toInt)
        val thisYear = Date(today.year, mo, d)
        if toEpochDay(thisYear) >= toEpochDay(today) then thisYear else Date(today.year + 1, mo, d)
      }.orElse {
        val hit = jaWeekdays.indexWhere(s.contains)
        if hit >= 0 then
          val delta = (hit - dayOfWeek(today) + 7) % 7
          Some(if s.contains("先週") then plusDays(today, if delta == 0 then -7 else delta - 7)
               else plusDays(today, if delta == 0 then 7 else delta))
        else if s.contains("明後日") then Some(plusDays(today, 2))
        else if s.contains("明日") then Some(plusDays(today, 1))
        else if s.contains("昨日") then Some(plusDays(today, -1))
        else if s.contains("今日") then Some(today)
        else daysAfterJa.findFirstMatchIn(s).map(m => plusDays(today, m.group(1).toInt))
          .orElse(daysBeforeJa.findFirstMatchIn(s).map(m => plusDays(today, -m.group(1).toInt)))
          .orElse(if s.contains("来週") then Some(plusDays(today, 7))
                  else if s.contains("先週") then Some(plusDays(today, -7)) else None)
      }

    private def japaneseTime(phrase: String): Option[(Int, Int)] =
      hourJa.findFirstMatchIn(phrase).map(m => (m.group(1).toInt, Option(m.group(2)).map(_.toInt).getOrElse(0)))
        .filter((h, m) => h < 24 && m < 60)
}

