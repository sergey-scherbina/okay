package okay2.sql

/**
 * Dates and times as numbers, with no java.time, so every platform
 * reads them alike (okay-sql's Temporal.scala): days since the epoch by
 * Howard Hinnant's civil algorithms, microseconds for times and
 * timestamps, ISO 8601 out, and pg's, H2's and ISO's forms in.
 */
object Temporal {

  private val MicrosPerSecond = 1000000L
  private val MicrosPerDay = 86400L * MicrosPerSecond

  def daysFromCivil(y0: Int, m: Int, d: Int): Int = {
    val y = if (m <= 2) y0 - 1 else y0
    val era = (if (y >= 0) y else y - 399) / 400
    val yoe = y - era * 400
    val mp = (m + 9) % 12
    val doy = (153 * mp + 2) / 5 + d - 1
    val doe = yoe * 365 + yoe / 4 - yoe / 100 + doy
    era * 146097 + doe - 719468
  }

  def civilFromDays(z0: Int): (Int, Int, Int) = {
    val z = z0 + 719468
    val era = (if (z >= 0) z else z - 146096) / 146097
    val doe = z - era * 146097
    val yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
    val y = yoe + era * 400
    val doy = doe - (365 * yoe + yoe / 4 - yoe / 100)
    val mp = (5 * doy + 2) / 153
    val d = doy - (153 * mp + 2) / 5 + 1
    val m = if (mp < 10) mp + 3 else mp - 9
    (if (m <= 2) y + 1 else y, m, d)
  }

  private def pad(n: Int, width: Int): String = {
    val s = math.abs(n).toString
    val body = if (s.length >= width) s else "0" * (width - s.length) + s
    if (n < 0) "-" + body else body
  }

  def renderDate(days: Int): String = {
    val (y, m, d) = civilFromDays(days)
    s"${pad(y, 4)}-${pad(m, 2)}-${pad(d, 2)}"
  }

  def renderTime(micros: Long): String = {
    val secs = micros / MicrosPerSecond
    val frac = micros % MicrosPerSecond
    val hh = pad((secs / 3600).toInt, 2)
    val mm = pad(((secs / 60) % 60).toInt, 2)
    val ss = pad((secs % 60).toInt, 2)
    if (frac == 0) s"$hh:$mm:$ss"
    else {
      var f = pad(frac.toInt, 6)
      while (f.endsWith("0")) f = f.dropRight(1)
      s"$hh:$mm:$ss.$f"
    }
  }

  def renderTimestamp(micros: Long): String = {
    val days = Math.floorDiv(micros, MicrosPerDay)
    val ofDay = Math.floorMod(micros, MicrosPerDay)
    s"${renderDate(days.toInt)}T${renderTime(ofDay)}Z"
  }

  private val DateRe = """^(-?\d{4,})-(\d{2})-(\d{2})$""".r
  private val TimeRe = """^(\d{2}):(\d{2})(?::(\d{2}))?(?:\.(\d{1,9}))?$""".r
  private val StampRe =
    """^(-?\d{4,})-(\d{2})-(\d{2})[T ](\d{2}):(\d{2})(?::(\d{2}))?(?:\.(\d{1,9}))?\s*(Z|z|[+-]\d{2}(?::?\d{2})?(?::?\d{2})?)?$""".r

  def parseDate(s: String): Option[Int] = s.trim match {
    case DateRe(y, m, d) => civil(y.toInt, m.toInt, d.toInt)
    case _ => None
  }

  private def civil(y: Int, m: Int, d: Int): Option[Int] =
    if (m < 1 || m > 12 || d < 1 || d > 31) None else Some(daysFromCivil(y, m, d))

  private def micros(h: Int, mi: Int, s: Int, frac: String): Option[Long] =
    if (h > 23 || mi > 59 || s > 60) None
    else {
      val f = if (frac == null) 0L else (frac + "000000").take(6).toLong
      Some(((h * 3600L + mi * 60L + s) * MicrosPerSecond) + f)
    }

  def parseTime(s: String): Option[Long] = s.trim match {
    case TimeRe(h, m, sec, frac) => micros(h.toInt, m.toInt, if (sec == null) 0 else sec.toInt, frac)
    case _ => None
  }

  private def offsetSeconds(z: String): Option[Long] =
    if (z == null || z == "Z" || z == "z") Some(0L)
    else {
      val sign = if (z.head == '-') -1L else 1L
      val digits = z.tail.filter(_ != ':')
      if (digits.length % 2 != 0 || digits.length > 6) None
      else {
        val parts = digits.grouped(2).map(_.toLong).toVector
        val h = parts(0)
        val m = if (parts.length > 1) parts(1) else 0L
        val sec = if (parts.length > 2) parts(2) else 0L
        Some(sign * (h * 3600 + m * 60 + sec))
      }
    }

  def parseTimestamp(s: String): Option[Long] = s.trim match {
    case StampRe(y, mo, d, h, mi, sec, frac, z) =>
      for {
        days <- civil(y.toInt, mo.toInt, d.toInt)
        us <- micros(h.toInt, mi.toInt, if (sec == null) 0 else sec.toInt, frac)
        off <- offsetSeconds(z)
      } yield days.toLong * MicrosPerDay + us - off * MicrosPerSecond
    case _ => None
  }
}
