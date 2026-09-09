package okay.outbox

import okay.*
import okay.given
import okay.codec.Schema
import okay.sql.{Sql, SqlValue, Typed}

/**
 * The typed rows of a small query, collected. `Typed.rows` streams
 * chunks of `Either[Bad, A]` because a damaged row is DATA for a
 * database that is not ours; these tables are ours, so a damaged row
 * is a failure here — the schema and the table were written by the
 * same hand, and a mismatch means a migration is missing.
 */
private[outbox] object Rows:

  def all[A: Schema](db: Sql, sql: String, params: Vector[SqlValue] = Vector.empty): Vector[A] ! Async =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    def go[B](p: Chunk[Either[B, A]] ! (Produce + Async), acc: Vector[A]): Vector[A] ! Async =
      S.uncons(p).flatMap {
        case None => pure(acc)
        case Some((c, rest)) =>
          val (bad, good) = c.partitionMap(identity)
          bad.headOption match
            case Some(b) => throw IllegalStateException(s"$sql: a row this Schema cannot read: $b")
            case None => go(rest, acc ++ good)
      }
    go(Typed.rows[A](db, sql, params), Vector.empty)
