package okay.jdbc

import okay.{!, +, Async, Chunk, Produce, Stream, async}
import okay.given
import okay.codec.Schema
import okay.persist.{Ack, Offsets}
import okay.sql.{Bad, Sql, SqlValue, Typed}

/**
 * The incremental poll (specs/jdbc.md, "Reading their data as it
 * changes") — stated non-CDC: available exactly when their schema
 * offers a monotone, commit-visible column, and honest about the
 * late-commit miss (a transaction committing late with a smaller
 * value is invisible to a watermark that already passed it; the
 * mitigation is a lag window IN THE CALLER'S SQL, never a cure).
 *
 * The watermark IS a consumer offset: stored through persist
 * `Offsets` — commit-as-record, refold-on-restart, nothing new to
 * make durable. At-least-once by construction: the watermark
 * commits after the batch is in hand, so a crash between the two
 * re-serves rows rather than losing them.
 */
final class Poll(db: Sql, offsets: Offsets, group: String, source: String,
                 start: Long = 0L):
  import Poll.*

  /** the resume point: the journaled watermark, or `start` */
  def watermark: Long = offsets.committed(group, source, 0).getOrElse(start)

  /**
   * One poll step. `sql` takes the watermark as its ONE parameter
   * (`where col > ? ... order by col`; a lag window belongs in the
   * same text, where the DBA can read it); `watermarkOf` reads the
   * monotone column back off the decoded row.
   *
   * The batch is the decoded PREFIX: a damaged row stops the
   * advance there and surfaces (the torn-tail doctrine at the
   * database seam) — the watermark never passes a row that did not
   * decode, so nothing is silently skipped.
   */
  def poll[A](sql: String)(watermarkOf: A => Long)(using Schema[A]): Batch[A] ! Async =
    val wm = watermark
    drain(Typed.rows[A](db, sql, Vector(SqlValue.I64(wm)))).flatMap { rows =>
      val prefix = rows.takeWhile(_.isRight).collect { case Right(a) => a }
      val damage = rows.drop(prefix.length).collectFirst { case Left(b) => b }
      val next = prefix.map(watermarkOf).maxOption.getOrElse(wm)
      async {
        if next > wm then offsets.commit(group, source, 0, next, Ack.Durable)
        Batch(prefix, damage, next)
      }
    }

  private def drain[A](p: Chunk[Either[Bad, A]] ! (Produce + Async))
  : Vector[Either[Bad, A]] ! Async =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    S.uncons(p).flatMap {
      case None => okay.pure(Vector.empty)
      case Some((c, rest)) => drain(rest).map(c.toVector ++ _)
    }

object Poll:
  /** what one poll answered: the rows that decoded (in column
   * order), the first damage if any stopped the batch there, and
   * the watermark now journaled */
  final case class Batch[A](rows: Vector[A], damage: Option[Bad], watermark: Long)
