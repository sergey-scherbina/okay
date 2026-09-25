package okay2.sql

import java.time.{Instant, LocalDate, LocalTime}
import java.time.format.DateTimeParseException
import okay2.codec.Schema

/** the java.time fields, JVM only (`import okay2.sql.javatime._`): on
 * the wire an ISO string, in a row the engine's own temporal column
 * (okay-sql's scala-jvm JavaTime). An object and not the package
 * object's parent: a package object extending a trait of its own
 * package is a cyclic reference in Scala 2 */
object javatime {
  implicit val instantSchema: Schema[Instant] = Schema.refine[Instant, String](
    s => try Right(Instant.parse(s)) catch { case _: DateTimeParseException => Left(s"not an instant: '$s'") },
    _.toString)

  implicit val localDateSchema: Schema[LocalDate] = Schema.refine[LocalDate, String](
    s => try Right(LocalDate.parse(s)) catch { case _: DateTimeParseException => Left(s"not a date: '$s'") },
    _.toString)

  implicit val localTimeSchema: Schema[LocalTime] = Schema.refine[LocalTime, String](
    s => try Right(LocalTime.parse(s)) catch { case _: DateTimeParseException => Left(s"not a time: '$s'") },
    _.toString)

  private[sql] val known: Vector[Typed.Known[_]] = Vector(
    new Typed.Known(instantSchema, Typed.Shape.prim[Instant](SqlType.Timestamp,
      { case SqlValue.Timestamp(us) => Instant.ofEpochSecond(Math.floorDiv(us, 1000000L), Math.floorMod(us, 1000000L) * 1000L) },
      i => SqlValue.Timestamp(i.getEpochSecond * 1000000L + i.getNano / 1000))),
    new Typed.Known(localDateSchema, Typed.Shape.prim[LocalDate](SqlType.Date,
      { case SqlValue.Date(d) => LocalDate.ofEpochDay(d.toLong) },
      d => SqlValue.Date(d.toEpochDay.toInt))),
    new Typed.Known(localTimeSchema, Typed.Shape.prim[LocalTime](SqlType.Time,
      { case SqlValue.Time(us) => LocalTime.ofNanoOfDay(us * 1000L) },
      t => SqlValue.Time(t.toNanoOfDay / 1000L))))
}
