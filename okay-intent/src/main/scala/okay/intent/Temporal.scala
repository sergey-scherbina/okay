package okay.intent

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
      .split("\\s+").filter(_.nonEmpty).toList
    if words.isEmpty then None
    else
      val time = timeIn(words)
      dateIn(words, today).map(d => time match
        case Some((h, m)) => When(d, Some(h), m)
        case None => When(d))

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
      val w = words(i)
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
}
