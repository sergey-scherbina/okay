package okay2.jdbc

import okay2.!
import okay2.async.Async
import okay2.codec.Schema
import okay2.persist.{Ack, Offsets}
import okay2.sql.{Bad, Sql, SqlValue, Typed}
import okay2.stream.{Chunk, Source}

/**
 * The incremental poll (okay-jdbc's Poll.scala, specs/jdbc.md, "Reading
 * their data as it changes") — stated non-CDC: available exactly when
 * their schema offers a monotone, commit-visible column, and honest about
 * the late-commit miss (the mitigation is a lag window IN THE CALLER'S
 * SQL, never a cure).
 *
 * The watermark IS a consumer offset, stored through persist `Offsets`.
 * At-least-once by construction: the watermark commits after the batch
 * is in hand.
 */
final class Poll(db: Sql, offsets: Offsets, group: String, source: String, start: Long = 0L) {
  import Poll._

  /** the resume point: the journaled watermark, or `start` */
  def watermark: Long = offsets.committed(group, source, 0).getOrElse(start)

  /**
   * One poll step. `sql` takes the watermark as its ONE parameter
   * (`where col > ? ... order by col`); `watermarkOf` reads the monotone
   * column back off the decoded row. The batch is the decoded PREFIX: a
   * damaged row stops the advance there and surfaces — the watermark
   * never passes a row that did not decode.
   */
  def poll[A](sql: String)(watermarkOf: A => Long)(implicit s: Schema[A]): Batch[A] ! Async = {
    val wm = watermark
    drain(Typed.rows[A](db, sql, Vector(SqlValue.I64(wm)))).flatMap { rows =>
      val prefix = rows.takeWhile(_.isRight).collect { case Right(a) => a }
      val damage = rows.drop(prefix.length).collectFirst { case Left(b) => b }
      val next = prefix.map(watermarkOf).maxOption.getOrElse(wm)
      Async {
        if (next > wm) offsets.commit(group, source, 0, next, Ack.Durable)
        Batch(prefix, damage, next)
      }
    }
  }

  private def drain[A](p: Source[Chunk[Either[Bad, A]]]): Vector[Either[Bad, A]] ! Async = Source.concat(p)
}

object Poll {
  /** what one poll answered: the rows that decoded, the first damage if
   * any stopped the batch there, and the watermark now journaled */
  final case class Batch[A](rows: Vector[A], damage: Option[Bad], watermark: Long)
}
