package okay2.sql

import scala.collection.immutable.ArraySeq
import okay2.{!, Writer, pure}
import okay2.async.{Accepted, Async, CanBlock, Handoff}
import okay2.stream.{Chunk, Source}

/** a driver made of one described frame, and a runner that executes
 * `Async.Run` in place and FAILS on any wait: these programs never
 * wait, so they run on Scala.js too */
object Drive {

  implicit val noPark: CanBlock = new CanBlock {
    private def no: Nothing = throw new AssertionError("an Async wait where none was expected")
    def block[A](register: (A => Unit) => (() => Unit)): A = no
    def blockAccepted(register: Accepted => (() => Unit)): Boolean = no
    def handoff[A](): Handoff[A] = no
    def await(h: Handoff[_]): Unit = no
  }

  def run[A](p: A ! Async): A = !.run(Async.run[A, okay2.Pure](p))

  def rows[A](db: Sql)(implicit s: okay2.codec.Schema[A]): Vector[Either[Bad, A]] =
    run(Source.concat(Typed.rows[A](db, "q")))

  final class OneFrame(cols: Vector[Col], rows: Vector[Vector[SqlValue]]) extends Sql {
    def describe(sql: String): Vector[Col] ! Async = pure[Async, Vector[Col]](cols)
    def query(sql: String, params: Vector[SqlValue]): Source[Chunk[Vector[SqlValue]]] =
      Writer.tell[Chunk[Vector[SqlValue]]](ArraySeq.from(rows))
    def update(sql: String, params: Vector[SqlValue]): Long ! Async = pure[Async, Long](0L)
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = pure[Async, Long](0L)
    def begin(isolation: Isolation, readOnly: Boolean): Granted ! Async = pure[Async, Granted](Granted(isolation, isolation))
    def commit(): Unit ! Async = pure[Async, Unit](())
    def rollback(): Unit ! Async = pure[Async, Unit](())
    def cancel(): Unit = ()
  }
}
