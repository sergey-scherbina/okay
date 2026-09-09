package okay.sql

/**
 * The temporal text forms, without java.time — this module runs on
 * JVM, JS and Native, and java.time is JVM-only (okay-intent's
 * `Temporal` made the same choice for the same reason). The values
 * themselves are platform-neutral numbers: a timestamp is
 * microseconds since the epoch (UTC), a date is days since the
 * epoch, a time is microseconds into the day. This object renders
 * them in ISO 8601 and parses what engines actually print:
 * Postgres's `2026-09-02 06:00:00.123456+02`, H2's
 * `2026-09-02 06:00:00+00`, ISO's `2026-09-02T06:00:00Z`.
 *
 * Civil-date arithmetic is Howard Hinnant's (proleptic Gregorian,
 * exact over the whole Int range of days).
 */
object Temporal:

  private val MicrosPerSecond = 1000000L
  private val MicrosPerDay = 86400L * MicrosPerSecond

  /** days since 1970-01-01 of the civil date */
  def daysFromCivil(y0: Int, m: Int, d: Int): Int =
    val y = if m <= 2 then y0 - 1 else y0
    val era = (if y >= 0 then y else y - 399) / 400
    val yoe = y - era * 400
    val mp = (m + 9) % 12
    val doy = (153 * mp + 2) / 5 + d - 1
    val doe = yoe * 365 + yoe / 4 - yoe / 100 + doy
    era * 146097 + doe - 719468

  /** the civil date of the day count: (year, month, day) */
  def civilFromDays(z0: Int): (Int, Int, Int) =
    val z = z0 + 719468
    val era = (if z >= 0 then z else z - 146096) / 146097
    val doe = z - era * 146097
    val yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
    val y = yoe + era * 400
    val doy = doe - (365 * yoe + yoe / 4 - yoe / 100)
    val mp = (5 * doy + 2) / 153
    val d = doy - (153 * mp + 2) / 5 + 1
    val m = if mp < 10 then mp + 3 else mp - 9
    (if m <= 2 then y + 1 else y, m, d)

  private def pad(n: Int, width: Int): String =
    val s = math.abs(n).toString
    val body = if s.length >= width then s else "0" * (width - s.length) + s
    if n < 0 then "-" + body else body

  /** `YYYY-MM-DD` */
  def renderDate(days: Int): String =
    val (y, m, d) = civilFromDays(days)
    s"${pad(y, 4)}-${pad(m, 2)}-${pad(d, 2)}"

  /** `HH:MM:SS`, then the fraction only when non-zero, trailing
   * zeros dropped (`06:00:00`, `06:00:00.5`, `06:00:00.000001`) */
  def renderTime(micros: Long): String =
    val secs = micros / MicrosPerSecond
    val frac = micros % MicrosPerSecond
    val hh = pad((secs / 3600).toInt, 2)
    val mm = pad(((secs / 60) % 60).toInt, 2)
    val ss = pad((secs % 60).toInt, 2)
    if frac == 0 then s"$hh:$mm:$ss"
    else
      var f = pad(frac.toInt, 6)
      while f.endsWith("0") do f = f.dropRight(1)
      s"$hh:$mm:$ss.$f"

  /** `YYYY-MM-DDTHH:MM:SS[.ffffff]Z` — ISO 8601, UTC, the form every
   * engine and java.time's `Instant.parse` accept */
  def renderTimestamp(micros: Long): String =
    val days = Math.floorDiv(micros, MicrosPerDay)
    val ofDay = Math.floorMod(micros, MicrosPerDay)
    s"${renderDate(days.toInt)}T${renderTime(ofDay)}Z"

  private val DateRe = """^(-?\d{4,})-(\d{2})-(\d{2})$""".r
  private val TimeRe = """^(\d{2}):(\d{2})(?::(\d{2}))?(?:\.(\d{1,9}))?$""".r
  private val StampRe =
    """^(-?\d{4,})-(\d{2})-(\d{2})[T ](\d{2}):(\d{2})(?::(\d{2}))?(?:\.(\d{1,9}))?\s*(Z|z|[+-]\d{2}(?::?\d{2})?(?::?\d{2})?)?$""".r

  def parseDate(s: String): Option[Int] = s.trim match
    case DateRe(y, m, d) => civil(y.toInt, m.toInt, d.toInt)
    case _ => None

  private def civil(y: Int, m: Int, d: Int): Option[Int] =
    if m < 1 || m > 12 || d < 1 || d > 31 then None else Some(daysFromCivil(y, m, d))

  private def micros(h: Int, mi: Int, s: Int, frac: String): Option[Long] =
    if h > 23 || mi > 59 || s > 60 then None
    else
      val f = if frac == null then 0L else (frac + "000000").take(6).toLong
      Some(((h * 3600L + mi * 60L + s) * MicrosPerSecond) + f)

  def parseTime(s: String): Option[Long] = s.trim match
    case TimeRe(h, m, sec, frac) => micros(h.toInt, m.toInt, if sec == null then 0 else sec.toInt, frac)
    case _ => None

  /** the offset in seconds of `Z`, `+02`, `+0230`, `+02:30`, `+02:30:15` */
  private def offsetSeconds(z: String): Option[Long] =
    if z == null || z == "Z" || z == "z" then Some(0L)
    else
      val sign = if z.head == '-' then -1L else 1L
      val digits = z.tail.filter(_ != ':')
      if digits.length % 2 != 0 || digits.length > 6 then None
      else
        val parts = digits.grouped(2).map(_.toLong).toVector
        val h = parts(0)
        val m = if parts.length > 1 then parts(1) else 0L
        val sec = if parts.length > 2 then parts(2) else 0L
        Some(sign * (h * 3600 + m * 60 + sec))

  /** microseconds since the epoch; a missing offset means UTC (a
   * `timestamp without time zone` is read as UTC, stated) */
  def parseTimestamp(s: String): Option[Long] = s.trim match
    case StampRe(y, mo, d, h, mi, sec, frac, z) =>
      for
        days <- civil(y.toInt, mo.toInt, d.toInt)
        us <- micros(h.toInt, mi.toInt, if sec == null then 0 else sec.toInt, frac)
        off <- offsetSeconds(z)
      yield days.toLong * MicrosPerDay + us - off * MicrosPerSecond
    case _ => None
