package okay.cluster

import okay.{Aggregator, Pane}
import okay.codec.Schema
import okay.given
import scala.collection.mutable

/**
 * THE COMMIT WINDOW, CLOSED BY THE WRITER (specs/dataflow.md,
 * stage 9).
 *
 * Stage 8 left one window open and named it: a pane is written while
 * its epoch is being absorbed and the epoch is committed afterwards,
 * so a coordinator that dies in between wrote panes the journal does
 * not know about, and its successor writes them again. Harmless to a
 * keyed writer, not harmless to an appending one.
 *
 * The engine cannot close it — the write left the engine — so it
 * hands the writer the two moments that let the writer close it.
 * `TestResume` already shows the window is real: on the same
 * schedules, `Sink.writing`'s plain writer is offered panes twice.
 * This shows a STAGING writer offered each epoch once, under the same
 * deaths, with the difference being one `move` that records the epoch
 * beside its rows.
 */
object Staged {
  /** the rows, and the epoch each arrived in — one atomic write in a
   * real store, one `synchronized` here */
  private val rows = mutable.HashMap.empty[(Long, Int), Long]
  private var applied = 0
  private var moves = 0L
  private var repeats = 0L
  private var panes = 0L

  /**
   * The writer, and its whole trick: an epoch it has already applied
   * is DROPPED. `committed` runs before the journal records the
   * epoch, so a coordinator that dies in between asks the successor
   * to redo it — and the successor's `move` carries the same number.
   */
  def move(epoch: Int, batch: Vector[Pane[Int, Long]]): Unit = synchronized {
    moves += 1
    if epoch <= applied then repeats += 1
    else
      applied = epoch
      panes += batch.length
      for p <- batch do rows.update((p.start, p.key), p.value)
  }

  def reset(): Unit = synchronized {
    rows.clear(); applied = 0; moves = 0L; repeats = 0L; panes = 0L
  }
  def snapshot: Map[(Long, Int), Long] = synchronized(rows.toMap)
  def written: Long = synchronized(panes)
  def dropped: Long = synchronized(repeats)
  def calls: Long = synchronized(moves)
}

/** the tumbling job of `TestOnce`, staged instead of written through */
object StagedJob extends Job[Feed, Long] {
  import Feeds.*
  type A = Ev
  def name: String = "test.staged"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def flow(f: Feed, parts: Int): Flow[Ev] = Flow.slices(events(f), parts)
  def sink(f: Feed): Wire[Ev, Long] =
    Wire.tumblingStaged(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(Staged.move)
}

class TestStaged extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()
  Jobs.register(StagedJob)
  val feed: Feed = Feed(20000, Late - 1)

  val collect: Aggregator[Pane[Int, Long], Vector[((Long, Int), Long)], Vector[((Long, Int), Long)]] =
    Aggregator[Pane[Int, Long], Vector[((Long, Int), Long)], Vector[((Long, Int), Long)]](
      Vector.empty)((b, p) => b :+ ((p.start, p.key) -> p.value))((a, b) => a ++ b)(identity)

  lazy val panes: Map[(Long, Int), Long] =
    Flows.fan(StagedJob.flow(feed, 8),
      Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(collect)).runWith.value.toMap

  /** a coordinator that dies at a chosen epoch, on either side of the
   * journal's commit — `TestResume`'s, and the same two deaths */
  final class Dying(at: Int, afterSaving: Boolean) extends Checkpoint:
    val kept = Checkpoint.Memory()
    def save(epoch: Int, bytes: Array[Byte]): Unit =
      if epoch == at && !afterSaving then throw Dying.Died(epoch)
      kept.save(epoch, bytes)
      if epoch == at then throw Dying.Died(epoch)
    def latest: Option[(Int, Array[Byte])] = kept.latest

  object Dying:
    final case class Died(epoch: Int) extends RuntimeException(s"died at epoch $epoch")

  test("a quiet stream: every epoch moved once, and the rows are the batch answer") {
    Staged.reset()
    val got = Cluster.stream(StagedJob, feed, 4, Vector(Cluster.local), 512).runWith
    assertEquals(Staged.snapshot, panes)
    assertEquals(Staged.written, panes.size.toLong)
    assertEquals(Staged.dropped, 0L, "an epoch was moved twice on a quiet run")
    assertEquals(got.value, panes.size.toLong)
  }

  test("THE WINDOW IS CLOSED: a death between the write and the commit moves nothing twice") {
    // this is the schedule that makes `Sink.writing`'s plain writer
    // repeat — TestResume asserts it does — and the staged one takes
    // it without a duplicate reaching the rows
    var seen = 0L
    for at <- Vector(2, 4, 6, 8) do
      Staged.reset()
      val j = Dying(at, afterSaving = false)
      val _ = intercept[Dying.Died](
        Cluster.stream(StagedJob, feed, 4, Vector(Cluster.local), 512, j).runWith)
      assertEquals(j.latest.map(_._1), Some(at - 1), s"the run never reached epoch $at")
      val got = Cluster.stream(StagedJob, feed, 4, Vector(Cluster.local), 512, j.kept).runWith

      assertEquals(Staged.snapshot, panes, s"died before committing epoch $at")
      assertEquals(Staged.written, panes.size.toLong,
        s"died before committing epoch $at: a pane was written twice")
      assertEquals(got.value, panes.size.toLong, s"died before committing epoch $at")
      seen += Staged.dropped
    // and the mechanism was EXERCISED: the successor was asked to
    // redo an epoch the writer had already applied, and dropped it
    assert(seen > 0,
      "four deaths inside the window and the writer never saw a repeated epoch — " +
        "this test is not exercising the thing it exists for")
  }

  test("a death AFTER the commit: the successor starts past the epoch, and moves nothing again") {
    for at <- Vector(2, 4, 6) do
      Staged.reset()
      val j = Dying(at, afterSaving = true)
      val _ = intercept[Dying.Died](
        Cluster.stream(StagedJob, feed, 4, Vector(Cluster.local), 512, j).runWith)
      val got = Cluster.stream(StagedJob, feed, 4, Vector(Cluster.local), 512, j.kept).runWith
      assertEquals(Staged.snapshot, panes, s"died after committing epoch $at")
      assertEquals(Staged.written, panes.size.toLong, s"died after committing epoch $at")
      assertEquals(Staged.dropped, 0L,
        s"died after committing epoch $at: the epoch was in the journal, so nothing should repeat")
      assertEquals(got.value, panes.size.toLong)
  }

  test("what a staged sink REFUSES: a batch run, where the panes retire on the workers") {
    Staged.reset()
    val e = intercept[Exception](
      Flows.fan(StagedJob.flow(feed, 4), StagedJob.sink(feed)).runWith)
    val why = Option(e.getCause).getOrElse(e).getMessage
    assert(why.contains("staging sink"), why)
    assert(why.contains("Cluster.stream"), why)
  }
}
