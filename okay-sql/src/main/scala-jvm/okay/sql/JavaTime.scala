package okay.sql

import okay.codec.Schema
import java.time.{Instant, LocalDate, LocalTime}
import java.time.format.DateTimeParseException

/* The java.time fields (sql-temporal-types), JVM only — the seam's
 * values are platform-neutral numbers (Temporal.scala), and these
 * givens are how a case class names an `Instant`, a `LocalDate` or a
 * `LocalTime` field on the platform that has them. In JSON/CBOR they
 * travel as ISO 8601 text; on a row they are the driver's Timestamp/
 * Date/Time values, exact to the microsecond. `import okay.sql.given` */

given instantSchema: Schema[Instant] = Schema.refine[Instant, String](
  s => try Right(Instant.parse(s)) catch { case _: DateTimeParseException => Left(s"not an instant: '$s'") },
  _.toString)

given localDateSchema: Schema[LocalDate] = Schema.refine[LocalDate, String](
  s => try Right(LocalDate.parse(s)) catch { case _: DateTimeParseException => Left(s"not a date: '$s'") },
  _.toString)

given localTimeSchema: Schema[LocalTime] = Schema.refine[LocalTime, String](
  s => try Right(LocalTime.parse(s)) catch { case _: DateTimeParseException => Left(s"not a time: '$s'") },
  _.toString)

/** the platform's typed row shapes: the given IS the identity */
private[sql] object JavaTime:
  val known: Vector[Typed.Known[?]] = Vector(
    Typed.Known(instantSchema, Typed.Shape.prim[Instant](SqlType.Timestamp,
      { case SqlValue.Timestamp(us) =>
          Instant.ofEpochSecond(Math.floorDiv(us, 1000000L), Math.floorMod(us, 1000000L) * 1000L) },
      i => SqlValue.Timestamp(i.getEpochSecond * 1000000L + i.getNano / 1000))),
    Typed.Known(localDateSchema, Typed.Shape.prim[LocalDate](SqlType.Date,
      { case SqlValue.Date(d) => LocalDate.ofEpochDay(d.toLong) },
      d => SqlValue.Date(d.toEpochDay.toInt))),
    Typed.Known(localTimeSchema, Typed.Shape.prim[LocalTime](SqlType.Time,
      { case SqlValue.Time(us) => LocalTime.ofNanoOfDay(us * 1000L) },
      t => SqlValue.Time(t.toNanoOfDay / 1000L))))
