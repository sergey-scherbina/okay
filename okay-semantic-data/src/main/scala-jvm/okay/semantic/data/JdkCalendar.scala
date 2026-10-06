package okay.semantic.data

import java.time.{Instant, ZoneId, DayOfWeek}
import java.time.temporal.TemporalAdjusters
import okay.semantic.{Calendar, Grain, Dimension, Kind, Value, TimeTransform}

/** Explicit civil-zone interpreter; fixed-duration windows remain in the cross core. */
object JdkCalendar extends Calendar:
  def start(micros: Long, grain: Grain, zone: String): Either[String, Long] =
    scala.util.Try {
      val instant = Instant.ofEpochSecond(Math.floorDiv(micros, 1000000L), Math.floorMod(micros, 1000000L) * 1000L)
      val date = instant.atZone(ZoneId.of(zone))
      val start = grain match
        case Grain.Hour => date.withMinute(0).withSecond(0).withNano(0)
        case Grain.Day => date.toLocalDate.atStartOfDay(date.getZone)
        case Grain.Week => date.toLocalDate.`with`(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY)).atStartOfDay(date.getZone)
        case Grain.Month => date.toLocalDate.withDayOfMonth(1).atStartOfDay(date.getZone)
        case Grain.Quarter => date.toLocalDate.withMonth((date.getMonthValue - 1) / 3 * 3 + 1).withDayOfMonth(1).atStartOfDay(date.getZone)
        case Grain.Year => date.toLocalDate.withDayOfYear(1).atStartOfDay(date.getZone)
      val result = start.toInstant
      Math.addExact(Math.multiplyExact(result.getEpochSecond, 1000000L), result.getNano / 1000L)
    }.toEither.left.map(e => s"calendar $grain ($zone): ${e.getMessage}")

  def dimension[A](id: String, description: String, read: A => Option[Long], grain: Grain,
                   zone: String): Either[String, Dimension[A]] =
    start(0L, grain, zone).map { _ =>
      Dimension(id, description, Kind.Number, a => read(a).fold[Value](Value.Null)(t =>
        Value.Number(BigDecimal(start(t, grain, zone).fold(why => throw IllegalArgumentException(why), identity)))), Some(TimeTransform.Civil(grain, zone)))
    }
