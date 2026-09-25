package okay2.sql

import okay2.{HMap, Same}
import okay2.codec.Schema

/** a named, typed column: the key of a typed row, compared by identity */
final class Column[A] private (val name: String, private[sql] val schema: Schema[A])

object Column {
  implicit val same: Same[Column] = Same.byIdentity[Column]
  def apply[A](name: String)(implicit s: Schema[A]): Column[A] = new Column(name, s)
}

/**
 * A typed row with no case class declared (okay-sql's Row.scala): an
 * `HMap` keyed by `Column`, so the row's TYPE is exactly its columns —
 * reading one never set does not compile — and it encodes to named or
 * positional parameters in insertion order.
 *
 * Scala 3 walks the map's tuple with a cast per entry; here the walk is
 * an instance built by induction over the entry list, where each entry
 * is `Cons[S, V, T]` with `S <: Column[V]`: the column's schema and the
 * value meet at one type, and nothing is cast.
 */
object Row {

  def empty: HMap[Column, HMap.Nil] = HMap.empty[Column]

  /** the entries of T encoded, newest first */
  @scala.annotation.implicitNotFound("${T} is not a list of Column entries")
  trait Encode[T] { def apply(t: T): Either[String, List[(String, SqlValue)]] }

  object Encode {
    implicit val nil: Encode[HMap.Nil] = new Encode[HMap.Nil] {
      def apply(t: HMap.Nil): Either[String, List[(String, SqlValue)]] = Right(Nil)
    }
    implicit def cons[V, S <: Column[V], T](implicit rest: Encode[T]): Encode[HMap.Cons[S, V, T]] =
      new Encode[HMap.Cons[S, V, T]] {
        def apply(t: HMap.Cons[S, V, T]): Either[String, List[(String, SqlValue)]] =
          Typed.encodeOne(t.key.schema, t.value) match {
            case Left(e) => Left(s"column ${t.key.name}: $e")
            case Right(sv) => rest(t.tail).map((t.key.name, sv) :: _)
          }
      }
  }

  implicit final class RowOps[T](private val row: HMap[Column, T]) extends AnyVal {
    /** the columns as named parameters, in insertion order */
    def toParams(implicit enc: Encode[T]): Either[String, Vector[(String, SqlValue)]] =
      enc(row.toList).map(_.reverse.toVector)

    /** the same, positional */
    def values(implicit enc: Encode[T]): Either[String, Vector[SqlValue]] = toParams.map(_.map(_._2))
  }
}
