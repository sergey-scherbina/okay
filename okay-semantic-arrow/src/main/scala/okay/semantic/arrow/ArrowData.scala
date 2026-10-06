package okay.semantic.arrow

import okay.semantic.{Plan, Result}
import okay.arrow.{ArrowCodec, Rows, Table}
import okay.codec.Schema
import okay.compress.Compression

object ArrowData:
  def table[A](plan: Plan[A], input: Table)(using Schema[A]): Either[Vector[String], Result] =
    Rows.rows[A](input).left.map(e => Vector(s"Arrow: $e")).flatMap(plan.run)
  def ipc[A](plan: Plan[A], bytes: Array[Byte])(using Schema[A], ArrowCodec, Compression): Either[Vector[String], Result] =
    scala.util.Try(summon[ArrowCodec].read(bytes)).toEither.left.map(e => Vector(s"Arrow IPC: ${e.getMessage}"))
      .flatMap(table(plan, _))
