package okay.intent

import okay.frame.Found

/**
 * Duration phrases to minutes (specs/intent-classify.md,
 * intent-extract-duration).
 *
 * The second slot with a parser and an extractor, after `Temporal`,
 * and built on the same two promises: TOTAL — `None` rather than a
 * guess, because a wrong length is booked while a refused one is
 * asked about — and DETERMINISTIC, a function of the phrase alone.
 *
 * The shapes are the ones a meeting is asked for in: a number and a
 * unit (`30 minutes`, `45 min`, `2 hours`, `1.5 h`, `90m`, `1h30`),
 * the spoken ones (`an hour`, `half an hour`, `a quarter of an hour`,
 * `an hour and a half`, `two hours`, `twenty minutes`). Anything else
 * — `a while`, `all day`, `a couple of hours` — is `None`. English
 * words; the other languages' number-and-unit words are a lexicon
 * each, as `Temporal`'s were, and are filed.
 */
object Duration {

  private val words = Map(
    "a" -> 1, "an" -> 1, "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6,
    "seven" -> 7, "eight" -> 8, "nine" -> 9, "ten" -> 10, "eleven" -> 11, "twelve" -> 12,
    "fifteen" -> 15, "twenty" -> 20, "thirty" -> 30, "forty" -> 40, "forty-five" -> 45, "fifty" -> 50,
    "sixty" -> 60, "ninety" -> 90)

  private val number = raw"(\d+(?:[.,]\d+)?)".r
  private val hm = raw"(\d{1,2})h(\d{2})".r            // 1h30
  private val glued = raw"(\d+(?:[.,]\d+)?)(h|hr|hrs|m|min|mins)".r  // 2h, 90m, 45min

  private def isHours(w: String): Boolean =
    w == "h" || w == "hr" || w == "hrs" || w == "hour" || w == "hours"
  private def isMinutes(w: String): Boolean =
    w == "m" || w == "min" || w == "mins" || w == "minute" || w == "minutes"

  private def tokens(phrase: String): List[String] =
    phrase.toLowerCase.replaceAll("[!?;:()]", " ")
      // a dot or comma BETWEEN digits is a decimal (1.5, 1,5); anywhere
      // else it is punctuation ("45 minutes, in room 4")
      .replaceAll("[.,](?!\\d)", " ").replaceAll("(?<!\\d)[.,]", " ")
      .replaceAll("(\\d)-(?=[a-z])", "$1 ")   // "2-hour" is a number and a unit; "forty-five" is a word
      .split("\\s+").filter(_.nonEmpty).toList

  private def quantity(w: String): Option[Double] =
    w match
      case number(n) => n.replace(',', '.').toDoubleOption
      case _ => words.get(w).map(_.toDouble)

  /** minutes, or `None` */
  def parse(phrase: String): Option[Int] =
    val ts = tokens(phrase)
    if ts.isEmpty then None
    else
      val set = ts.toSet
      // the spoken fractions first: they are whole phrases, not a
      // number beside a unit
      if ts.containsSlice(List("quarter", "of", "an", "hour")) || ts.containsSlice(List("quarter", "hour")) then Some(15)
      else if ts.containsSlice(List("half", "an", "hour")) || ts.containsSlice(List("half", "hour")) then
        // "an hour and a half" is caught below; this is "half an hour"
        Some(30)
      else
        // 1h30 / 2h / 90m / 45min as one token
        val direct = ts.collectFirst {
          case hm(h, m) if m.toInt < 60 => h.toInt * 60 + m.toInt
          case glued(n, u) => n.replace(',', '.').toDouble match
            case q if isHours(u) => math.round(q * 60).toInt
            case q => math.round(q).toInt
        }.orElse {
          // a number (or number word) followed by a unit; "and a half" adds thirty
          val i = ts.indices.collectFirst {
            case i if i + 1 < ts.length && quantity(ts(i)).isDefined && (isHours(ts(i + 1)) || isMinutes(ts(i + 1))) => i
          }
          i.flatMap { i =>
            val q = quantity(ts(i)).get
            val hours = isHours(ts(i + 1))
            val base = if hours then q * 60 else q
            val half = hours && ts.drop(i + 2).take(3) == List("and", "a", "half")
            val total = math.round(base).toInt + (if half then 30 else 0)
            if total > 0 && total <= 24 * 60 then Some(total) else None
          }
        }
        direct.orElse(
          if set.contains("hour") && ts.containsSlice(List("and", "a", "half")) then Some(90) else None)
        .orElse(Multilingual.parse(phrase))

  /**
   * The same answer as `parse`, plus the words it rests on — the
   * shortest window of words reproducing the whole message's value,
   * exactly as `Temporal.find` (read its comment for why the value is
   * the whole message's and only the evidence is searched for).
   */
  def find(message: String): Option[Found[Int]] =
    parse(message).map { v =>
      val toks = message.split("\\s+").filter(_.nonEmpty)
      val windows =
        for len <- 1 to toks.length; i <- 0 to toks.length - len yield (i, len)
      val span = windows
        .find((i, len) => parse(toks.slice(i, i + len).mkString(" ")).contains(v))
        .map((i, len) => toks.slice(i, i + len).mkString(" "))
        .getOrElse(message)
      Found(span.replaceAll("^[\\p{Punct}]+|[\\p{Punct}]+$", "").trim, v)
    }

  /**
   * The other seven languages (intent-duration-multilingual), the way
   * `Temporal.Multilingual` does it: a lexicon per language — unit
   * words matched as token PREFIXES so a declension is the word it
   * starts with (`heures`, `Stunden`, `часа`, `godziny`), number words
   * with their genders, the spoken fractions as phrases, a compound
   * number as a ten and a unit beside it (`сорок пять`, `cuarenta y
   * cinco`, `czterdzieści pięć`), and `et demie` / `y media` / `с
   * половиной` / `i pół` after the hours. Japanese is a string scan:
   * `N時間`, `N時間半`, `N時間M分`, `N分`, `半時間`. English is tried first
   * and unchanged; what no lexicon says is `None`.
   */
  private object Multilingual:
    final case class Lexicon(
      hours: Seq[String], minutes: Seq[String], numbers: Map[String, Double],
      hourAlone: Seq[String], half: Seq[List[String]], quarter: Seq[List[String]],
      hourAndHalf: Seq[List[String]], andHalf: Seq[List[String]], connectors: Set[String] = Set.empty)

    private def is(tok: String, forms: Seq[String]): Boolean = forms.exists(f => tok.startsWith(f))

    val fr = Lexicon(
      hours = Seq("heure"), minutes = Seq("minute", "min"),
      numbers = Map("un" -> 1.0, "une" -> 1.0, "deux" -> 2.0, "trois" -> 3.0, "quatre" -> 4.0, "cinq" -> 5.0, "six" -> 6.0,
        "sept" -> 7.0, "huit" -> 8.0, "neuf" -> 9.0, "dix" -> 10.0, "onze" -> 11.0, "douze" -> 12.0, "quinze" -> 15.0,
        "vingt" -> 20.0, "trente" -> 30.0, "quarante" -> 40.0, "quarante-cinq" -> 45.0, "cinquante" -> 50.0,
        "soixante" -> 60.0, "quatre-vingt-dix" -> 90.0, "demie" -> 0.5),
      hourAlone = Seq(), half = Seq(List("demi-heure"), List("une", "demi-heure"), List("demi", "heure")),
      quarter = Seq(List("quart", "d'heure"), List("quart", "d’heure"), List("quart", "heure")),
      hourAndHalf = Seq(List("heure", "et", "demie"), List("une", "heure", "et", "demie")),
      andHalf = Seq(List("et", "demie")))
    val de = Lexicon(
      hours = Seq("stunde", "std"), minutes = Seq("minute", "min"),
      numbers = Map("ein" -> 1.0, "eine" -> 1.0, "eins" -> 1.0, "einer" -> 1.0, "zwei" -> 2.0, "drei" -> 3.0, "vier" -> 4.0,
        "fünf" -> 5.0, "fuenf" -> 5.0, "sechs" -> 6.0, "sieben" -> 7.0, "acht" -> 8.0, "neun" -> 9.0, "zehn" -> 10.0,
        "elf" -> 11.0, "zwölf" -> 12.0, "zwoelf" -> 12.0, "fünfzehn" -> 15.0, "zwanzig" -> 20.0, "dreißig" -> 30.0,
        "dreissig" -> 30.0, "vierzig" -> 40.0, "fünfundvierzig" -> 45.0, "fünfzig" -> 50.0, "sechzig" -> 60.0, "neunzig" -> 90.0,
        "anderthalb" -> 1.5, "eineinhalb" -> 1.5, "zweieinhalb" -> 2.5, "dreieinhalb" -> 3.5),
      hourAlone = Seq(), half = Seq(List("halbe", "stunde"), List("eine", "halbe", "stunde")),
      quarter = Seq(List("viertelstunde"), List("eine", "viertelstunde"), List("viertel", "stunde")),
      hourAndHalf = Seq(List("anderthalb", "stunden"), List("eineinhalb", "stunden"), List("eine", "stunde", "und", "eine", "halbe")),
      andHalf = Seq(List("und", "eine", "halbe")))
    val es = Lexicon(
      hours = Seq("hora"), minutes = Seq("minuto", "min"),
      numbers = Map("un" -> 1.0, "una" -> 1.0, "uno" -> 1.0, "dos" -> 2.0, "tres" -> 3.0, "cuatro" -> 4.0, "cinco" -> 5.0,
        "seis" -> 6.0, "siete" -> 7.0, "ocho" -> 8.0, "nueve" -> 9.0, "diez" -> 10.0, "once" -> 11.0, "doce" -> 12.0,
        "quince" -> 15.0, "veinte" -> 20.0, "treinta" -> 30.0, "cuarenta" -> 40.0, "cincuenta" -> 50.0, "sesenta" -> 60.0, "noventa" -> 90.0),
      hourAlone = Seq(), half = Seq(List("media", "hora")),
      quarter = Seq(List("cuarto", "de", "hora"), List("un", "cuarto", "de", "hora")),
      hourAndHalf = Seq(List("hora", "y", "media"), List("una", "hora", "y", "media")),
      andHalf = Seq(List("y", "media")), connectors = Set("y"))
    val ru = Lexicon(
      hours = Seq("час"), minutes = Seq("минут", "мин"),
      numbers = Map("один" -> 1.0, "одна" -> 1.0, "одну" -> 1.0, "два" -> 2.0, "две" -> 2.0, "три" -> 3.0, "четыре" -> 4.0,
        "пять" -> 5.0, "шесть" -> 6.0, "семь" -> 7.0, "восемь" -> 8.0, "девять" -> 9.0, "десять" -> 10.0, "одиннадцать" -> 11.0,
        "двенадцать" -> 12.0, "пятнадцать" -> 15.0, "двадцать" -> 20.0, "тридцать" -> 30.0, "сорок" -> 40.0,
        "пятьдесят" -> 50.0, "шестьдесят" -> 60.0, "девяносто" -> 90.0, "полтора" -> 1.5),
      hourAlone = Seq("час"), half = Seq(List("полчаса"), List("пол", "часа")),
      quarter = Seq(List("четверть", "часа")),
      hourAndHalf = Seq(List("полтора", "часа")),
      andHalf = Seq(List("с", "половиной")))
    val uk = Lexicon(
      hours = Seq("годин"), minutes = Seq("хвилин", "хв"),
      numbers = Map("один" -> 1.0, "одна" -> 1.0, "одну" -> 1.0, "два" -> 2.0, "дві" -> 2.0, "три" -> 3.0, "чотири" -> 4.0,
        "п'ять" -> 5.0, "п’ять" -> 5.0, "шість" -> 6.0, "сім" -> 7.0, "вісім" -> 8.0, "дев'ять" -> 9.0, "дев’ять" -> 9.0,
        "десять" -> 10.0, "одинадцять" -> 11.0, "дванадцять" -> 12.0, "п'ятнадцять" -> 15.0, "п’ятнадцять" -> 15.0,
        "двадцять" -> 20.0, "тридцять" -> 30.0, "сорок" -> 40.0, "п'ятдесят" -> 50.0, "п’ятдесят" -> 50.0,
        "шістдесят" -> 60.0, "дев'яносто" -> 90.0, "дев’яносто" -> 90.0, "півтори" -> 1.5),
      hourAlone = Seq("годину", "година"), half = Seq(List("півгодини"), List("пів", "години")),
      quarter = Seq(List("чверть", "години")),
      hourAndHalf = Seq(List("півтори", "години")),
      andHalf = Seq(List("з", "половиною")))
    val pl = Lexicon(
      hours = Seq("godzin"), minutes = Seq("minut", "min"),
      numbers = Map("jeden" -> 1.0, "jedna" -> 1.0, "jedną" -> 1.0, "dwa" -> 2.0, "dwie" -> 2.0, "trzy" -> 3.0, "cztery" -> 4.0,
        "pięć" -> 5.0, "sześć" -> 6.0, "siedem" -> 7.0, "osiem" -> 8.0, "dziewięć" -> 9.0, "dziesięć" -> 10.0,
        "jedenaście" -> 11.0, "dwanaście" -> 12.0, "piętnaście" -> 15.0, "dwadzieścia" -> 20.0, "trzydzieści" -> 30.0,
        "czterdzieści" -> 40.0, "pięćdziesiąt" -> 50.0, "sześćdziesiąt" -> 60.0, "dziewięćdziesiąt" -> 90.0, "półtorej" -> 1.5),
      hourAlone = Seq("godzinę", "godzina"), half = Seq(List("pół", "godziny")),
      quarter = Seq(List("kwadrans")),
      hourAndHalf = Seq(List("półtorej", "godziny")),
      andHalf = Seq(List("i", "pół")))
    val lexicons = Vector(fr, de, es, ru, uk, pl)

    private val jaHours = raw"(\d+)時間(半)?(?:(\d+)分)?".r
    private val jaMinutes = raw"(\d+)分".r

    def parse(phrase: String): Option[Int] =
      val ts = tokens(phrase)
      lexicons.iterator.map(lex => inLexicon(ts, lex)).collectFirst { case Some(m) => m }
        .orElse(japanese(phrase))

    private def inLexicon(ts: List[String], lex: Lexicon): Option[Int] =
      def phrase(forms: Seq[List[String]]): Boolean = forms.exists(f => ts.containsSlice(f))
      // a count beside a unit first: "dwie i pół godziny" CONTAINS the
      // half-hour phrase and means two and a half; the phrases answer
      // for what has no count
      def phrases: Option[Int] =
        if phrase(lex.hourAndHalf) then Some(90)
        else if phrase(lex.quarter) then Some(15)
        else if phrase(lex.half) then Some(30)
        else None
      locally:
        // a quantity at i: digits, a number word, or a ten and a unit
        // (with the language's connector between: "cuarenta y cinco")
        def quantityAt(i: Int): Option[(Double, Int)] =
          ts(i) match
            case number(n) => n.replace(',', '.').toDoubleOption.map(q => (q, 1))
            case w => lex.numbers.get(w).map { n =>
              val j = if i + 1 < ts.length && lex.connectors(ts(i + 1)) then i + 2 else i + 1
              val ones = if n >= 20 && n % 10 == 0 && j < ts.length then lex.numbers.get(ts(j)).filter(o => o < 10 && o >= 1) else None
              ones.map(o => (n + o, j - i + 1)).getOrElse((n, 1))
            }
        // "и пол" / "с половиной" / "i pół" sit BEFORE the unit in the
        // Slavic languages and "et demie" / "y media" AFTER it in the
        // Romance ones; both add thirty minutes to a count of hours
        def halfAt(k: Int): Int = lex.andHalf.find(f => ts.drop(k).startsWith(f)).map(_.length).getOrElse(0)
        val hit = ts.indices.iterator.flatMap { i =>
          quantityAt(i).flatMap { (q, used) =>
            val before = halfAt(i + used)
            val u = i + used + before
            if u < ts.length && (is(ts(u), lex.hours) || is(ts(u), lex.minutes)) then
              val hours = is(ts(u), lex.hours)
              val half = hours && (before > 0 || halfAt(u + 1) > 0)
              val total = math.round(if hours then q * 60 else q).toInt + (if half then 30 else 0)
              if total > 0 && total <= 24 * 60 then Some(total) else None
            else None
          }
        }.nextOption()
        hit.orElse(phrases).orElse(
          // the unit alone, in the languages that say "for an hour" with
          // no article to count: "час", "годину", "godzinę"
          if ts.exists(lex.hourAlone.contains) && !ts.indices.exists(i => quantityAt(i).isDefined) then Some(60) else None)

    private def japanese(phrase: String): Option[Int] =
      if phrase.contains("半時間") then Some(30)
      else jaHours.findFirstMatchIn(phrase).map { m =>
        m.group(1).toInt * 60 + (if m.group(2) != null then 30 else 0) + Option(m.group(3)).map(_.toInt).getOrElse(0)
      }.orElse(jaMinutes.findFirstMatchIn(phrase).map(_.group(1).toInt)).filter(v => v > 0 && v <= 24 * 60)
}
