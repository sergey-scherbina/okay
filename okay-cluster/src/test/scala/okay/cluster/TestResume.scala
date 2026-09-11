package okay.cluster

import okay.{Aggregator, Pane}
import okay.codec.Schema
import okay.given
import scala.collection.mutable

/**
 * A STORE OF ITS OWN, not `TestOnce`'s.
 *
 * The two suites can run in the same JVM at the same time and both
 * write panes into a map keyed by (window, key); sharing one would
 * make each suite's assertions depend on the other's timing, which is
 * the per-suite-state trap this repository has met before.
 */
object ResumeStore {
  private val rows = mutable.HashMap.empty[(Long, Int), Long]
  private var offers = 0L
  private var clashes = 0L

  def write(p: Pane[Int, Long]): Unit = synchronized {
    offers += 1
    if rows.get((p.start, p.key)).exists(_ != p.value) then clashes += 1
    rows.update((p.start, p.key), p.value)
  }

  def reset(): Unit = synchronized { rows.clear(); offers = 0L; clashes = 0L }
  def snapshot: Map[(Long, Int), Long] = synchronized(rows.toMap)
  def offered: Long = synchronized(offers)
  def clashed: Long = synchronized(clashes)
}

/** the windowed job of `TestOnce`, writing into the store above */
object ResumeWriteJob extends Job[Feed, Long] {
  import Feeds.*
  type A = Ev
  def name: String = "test.write.resume"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def flow(f: Feed, parts: Int): Flow[Ev] = Flow.slices(events(f), parts)
  def sink(f: Feed): Wire[Ev, Long] =
    Wire.tumblingTo(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(ResumeStore.write)
}

/**
 * THE COORDINATOR DIES AND ANOTHER FINISHES THE JOB
 * (specs/dataflow.md, stage 8).
 *
 * Stages 5 and 6b made a WORKER's death ordinary: a partition is a
 * recipe, so a replacement replays it. The coordinator was the one
 * party that could not be replaced, because it held the fold and
 * wrote nothing down. Now it writes down exactly what it holds — the
 * fold, the per-partition extents, the two counters and the session
 * ids — after every epoch, and a second `Cluster.stream` over the
 * same journal picks the run up.
 *
 * THE TWO DEATHS ARE BOTH TESTED, and they are not the same one:
 *
 *   - AFTER the commit: the epoch is in the journal, and the
 *     successor asks for the next one;
 *   - BEFORE it: the epoch was computed and lost, so the successor
 *     asks for it AGAIN. The workers' sessions are already there and
 *     re-answer the same partial, which is exactly the idempotency
 *     `TestStream` pins from both sides.
 *
 * Both must produce the batch answer, and the second is the one that
 * would quietly lose an epoch if `Advance` meant "next" rather than
 * naming an index.
 */
class TestResume extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()
  val feed: Feed = Feed(20000, Late - 1)

  lazy val batch: Run[((Sum, Sum), Sum)] =
    Flows.fan(FanJob.flow(feed, 8), FanJob.sink(feed)).runWith

  /**
   * A CLEAN STREAM, for the counts that a stream and a batch run do
   * not share.
   *
   * `value` and `dropped` are the same question either way and are
   * asserted against the BATCH answer, which is the definition of
   * correct. `merged` is not: a streaming partition finishes nothing
   * locally (stage 6a), so every pane reaches the coordinator as an
   * accumulator and the number is an order of magnitude larger —
   * 16 511 against the batch's 1 264 here. Comparing it to the batch
   * is comparing two different questions, which is what the first
   * version of this test did.
   */
  def streamed(parts: Int, take: Int): Run[((Sum, Sum), Sum)] =
    Cluster.stream(FanJob, feed, parts, Vector(Cluster.local), take).runWith

  /** a coordinator that dies at a chosen epoch — modelled where the
   * death actually matters, at the commit */
  final class Dying(at: Int, afterSaving: Boolean) extends Checkpoint:
    val kept = Checkpoint.Memory()
    def save(epoch: Int, bytes: Array[Byte]): Unit =
      if epoch == at && !afterSaving then throw Dying.Died(epoch)
      kept.save(epoch, bytes)
      if epoch == at then throw Dying.Died(epoch)
    def latest: Option[(Int, Array[Byte])] = kept.latest

  object Dying:
    final case class Died(epoch: Int) extends RuntimeException(s"the coordinator died at epoch $epoch")

  test("the journal fills, and the answer does not move") {
    val j = Checkpoint.Memory()
    val got = Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 1000, j).runWith
    assertEquals(got.value, batch.value)
    assertEquals(got.dropped, batch.dropped)
    assert(j.commits > 1, s"the coordinator journalled ${j.commits} times")
    assert(j.latest.isDefined)
  }

  test("a run with NO journal is unchanged — the default costs nothing") {
    val got = Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 1000).runWith
    assertEquals(got.value, batch.value)
  }

  test("THE COORDINATOR DIES AFTER A COMMIT, and a second one finishes the job") {
    val clean = streamed(4, 1000)
    for at <- Vector(1, 2, 3, 5) do
      val j = Dying(at, afterSaving = true)
      val e = intercept[Dying.Died](
        Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 1000, j).runWith)
      assertEquals(e.epoch, at)
      assertEquals(j.latest.map(_._1), Some(at), s"epoch $at was not committed")

      // a NEW coordinator, over the same journal
      val got = Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 1000, j.kept).runWith
      assertEquals(got.value, batch.value, s"died after committing epoch $at")
      assertEquals(got.dropped, batch.dropped, s"died after committing epoch $at")
      // and the same COUNT as an uninterrupted stream: the epochs
      // before the crash were folded by the predecessor and their
      // counts came back out of the journal
      assertEquals(got.merged, clean.merged, s"died after committing epoch $at")
  }

  test("THE COORDINATOR DIES BEFORE THE COMMIT, and the epoch is done again") {
    val clean = streamed(4, 1000)
    for at <- Vector(1, 2, 3, 5) do
      val j = Dying(at, afterSaving = false)
      val _ = intercept[Dying.Died](
        Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 1000, j).runWith)
      assertEquals(j.latest.map(_._1), if at == 1 then None else Some(at - 1),
        s"the journal should hold the epoch BEFORE $at")

      val got = Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 1000, j.kept).runWith
      assertEquals(got.value, batch.value, s"died before committing epoch $at")
      assertEquals(got.dropped, batch.dropped, s"died before committing epoch $at")
      // NOT more than an uninterrupted stream, and that is the whole
      // point of `Advance` naming an INDEX: the epoch that was lost is
      // asked for again, the session re-answers the same partial, and
      // it is absorbed once into the state that never saw it
      assertEquals(got.merged, clean.merged, s"died before committing epoch $at")
  }

  /** the session ids this run minted, read out of its own journal —
   * counting the global table would race with every other suite in
   * this JVM, and these are the only ids this test may speak about */
  def idsOf(j: Checkpoint, parts: Int): Vector[Long] =
    val (_, bytes) = j.latest.getOrElse(fail("the journal is empty"))
    val f = okay.codec.Codecs.cbor(summon[okay.codec.Schema[Folded]]).decode(bytes)
      .fold(why => fail(why), identity)
    Vector.tabulate(parts)(i => f.base + i)

  test("the successor inherits the SESSIONS, so a dead coordinator strands none") {
    val j = Dying(2, afterSaving = true)
    val _ = intercept[Dying.Died](
      Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 1000, j).runWith)
    val ids = idsOf(j, 4)
    assert(ids.forall(Sessions.get(_).isDefined),
      "the dead coordinator left no sessions open — there is nothing to strand")

    val got = Cluster.stream(FanJob, feed, 4, Vector(Cluster.local), 1000, j.kept).runWith
    assertEquals(got.value, batch.value)
    assert(ids.forall(Sessions.get(_).isEmpty),
      "the successor minted new session ids and left its predecessor's open for ever")
  }

  test("the resume survives the workers dying too") {
    // both parties gone: the successor opens the sessions afresh
    // under the inherited ids and replays to the epoch it needs,
    // which is stage 6b's road with nobody left who has seen it
    val j = Dying(3, afterSaving = true)
    val _ = intercept[Dying.Died](
      Cluster.stream(FanJob, feed, 8, Vector(Cluster.local), 512, j).runWith)
    val ids = idsOf(j, 8)
    assert(ids.forall(Sessions.get(_).isDefined))
    ids.foreach(Sessions.drop)                       // the machines restarted
    assert(ids.forall(Sessions.get(_).isEmpty))

    val got = Cluster.stream(FanJob, feed, 8, Vector(Cluster.local), 512, j.kept).runWith
    assertEquals(got.value, batch.value)
    assertEquals(got.dropped, batch.dropped)
  }

  // -----------------------------------------------------------------
  // 6c ACROSS RUNS — the boundary stage 6c had to draw, and where it
  // moves to now that the coordinator has a journal.
  // -----------------------------------------------------------------

  Jobs.register(ResumeWriteJob)

  val collect: Aggregator[Pane[Int, Long], Vector[((Long, Int), Long)], Vector[((Long, Int), Long)]] =
    Aggregator[Pane[Int, Long], Vector[((Long, Int), Long)], Vector[((Long, Int), Long)]](
      Vector.empty)((b, p) => b :+ ((p.start, p.key) -> p.value))((a, b) => a ++ b)(identity)

  lazy val panes: Map[(Long, Int), Long] =
    val rows = Flows.fan(ResumeWriteJob.flow(feed, 8),
      Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(collect)).runWith.value
    assertEquals(rows.length, rows.map(_._1).distinct.length, "the batch run presented a window twice")
    rows.toMap

  test("A WRITING SINK ACROSS RUNS: the store holds each window once, and nothing is offered twice") {
    // the coordinator dies AFTER committing, so every pane it wrote
    // is inside the epoch the journal holds — the successor starts
    // past them and offers each exactly once
    for at <- Vector(2, 4, 6) do
      ResumeStore.reset()
      val j = Dying(at, afterSaving = true)
      val _ = intercept[Dying.Died](
        Cluster.stream(ResumeWriteJob, feed, 4, Vector(Cluster.local), 512, j).runWith)
      // the epoch has to EXIST for the death to mean anything: with
      // ten epochs in this run, `at` past the end would make the
      // whole row vacuous
      assertEquals(j.latest.map(_._1), Some(at), s"the run never reached epoch $at")
      val got = Cluster.stream(ResumeWriteJob, feed, 4, Vector(Cluster.local), 512, j.kept).runWith
      assertEquals(ResumeStore.snapshot, panes, s"died after committing epoch $at")
      assertEquals(ResumeStore.clashed, 0L, s"died after committing epoch $at")
      assertEquals(ResumeStore.offered, panes.size.toLong,
        s"died after committing epoch $at: a pane was offered twice across the restart")
      assertEquals(got.value, panes.size.toLong, s"died after committing epoch $at")
  }

  test("and where the window IS: a death between the write and the commit re-offers that epoch") {
    // THE HONEST HALF. A pane is written while the epoch is being
    // absorbed and the epoch is committed after — so a coordinator
    // that dies in between has written panes the journal does not
    // know about, and the successor writes them again. The identity
    // is what makes that harmless, exactly as it is within one run
    // (stage 6c): same key, same value, one row.
    var repeats = 0L
    for at <- Vector(2, 4, 6, 8) do
      ResumeStore.reset()
      val j = Dying(at, afterSaving = false)
      val _ = intercept[Dying.Died](
        Cluster.stream(ResumeWriteJob, feed, 4, Vector(Cluster.local), 512, j).runWith)
      assertEquals(j.latest.map(_._1), Some(at - 1), s"the run never reached epoch $at")
      val got = Cluster.stream(ResumeWriteJob, feed, 4, Vector(Cluster.local), 512, j.kept).runWith
      assertEquals(ResumeStore.snapshot, panes, s"died before committing epoch $at")
      assertEquals(ResumeStore.clashed, 0L,
        s"died before committing epoch $at: a repeated offer carried a different value")
      assert(ResumeStore.offered >= panes.size.toLong, s"epoch $at")
      assertEquals(got.value, panes.size.toLong, s"died before committing epoch $at")
      repeats += ResumeStore.offered - panes.size
    assert(repeats > 0,
      "four deaths between a write and its commit and not one pane was rewritten — " +
        "this test is not exercising the window it exists for")
  }
}
