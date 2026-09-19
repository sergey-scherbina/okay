package okay.outbox

import okay.*
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
    // generic in B so the damage type is whatever `Typed.rows` says
    def go[B](p: Source[Chunk[Either[B, A]]]): Vector[A] ! Async =
      given Fold[Chunk[Either[B, A]], Vector[A]] = Fold(Vector.empty[A]) { (acc, c) =>
        val (bad, good) = c.partitionMap(identity)
        bad.headOption match
          case Some(b) => throw IllegalStateException(s"$sql: a row this Schema cannot read: $b")
          case None => acc ++ good
      }
      Writer.fold[Chunk[Either[B, A]], Vector[A], Unit, Async](p).map(_._1)
    go(Typed.rows[A](db, sql, params))
