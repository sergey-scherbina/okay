package okay.cluster

import okay.Aggregator
import okay.codec.Schema
import okay.given

/**
 * RESCALE AT AN EPOCH BOUNDARY (specs/dataflow.md, stage 13).
 *
 * A stream stops at epoch N running `parts` ways and resumes at N+1
 * running `parts'` ways, and the answer is the batch answer. The
 * whole of what makes it possible is here already: stopping is a
 * coordinator that throws after a commit (as in TestResume), resuming
 * is the ordinary journal resume, and the fold the coordinator carries
 * is keyed by KEY (not by partition), so a re-cut source leaves it
 * untouched.
 *
 * Two things the source and sink must be, and both are checked here by
 * asserting the engine REFUSES otherwise rather than compute a wrong
 * answer:
 *
 *   - the SOURCE must be striped, not sliced. A striped source's
 *     consumed elements are a clean global prefix `[0, G)` whatever
 *     the partition count, so resuming is "skip the prefix, re-stripe
 *     the rest". A contiguous cut has no such prefix and refuses.
 *   - the SINK must be keyed or fold, not windowed. A windowed
 *     operator keeps its OPEN panes in the worker, rebuilt by replay
 *     on a same-width resume; a re-cut does not replay, so it would
 *     lose the panes open at the stop point. A keyed/fold sink keeps
 *     everything in the coordinator's fold and carries it across.
 *     (Windowed rescale — journalling the open panes — is box 2, not
 *     built.)
 *
 * The feed is IN ORDER (`jitter = 0`): a keyed sink never drops, but
 * this keeps the whole test's arithmetic simple.
 */
class TestRescale extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()
  Jobs.register(StripeKeyedJob)
  Jobs.register(StripeWindowJob)
  val feed: Feed = Feed(20000, 0)

  /** the answer, however the source is cut — the definition of correct */
  lazy val batch: Run[Sum] =
    Flows.fan(StripeKeyedJob.flow(feed, 4), StripeKeyedJob.sink(feed)).runWith

  /** a coordinator that commits epoch `at` and then stops — a death,
   * or a deposition, at a boundary; its journal is what resumes */
  final class StopAt(at: Int) extends Checkpoint:
    val kept = Checkpoint.Memory()
    def save(epoch: Int, bytes: Array[Byte]): Unit =
      kept.save(epoch, bytes)
      if epoch == at then throw StopAt.Stopped(epoch)
    def latest: Option[(Int, Array[Byte])] = kept.latest
  object StopAt:
    final case class Stopped(epoch: Int) extends RuntimeException(s"stopped at epoch $epoch")

  test("a striped keyed stream, NOT rescaled, is already the batch answer") {
    val got = Cluster.stream(StripeKeyedJob, feed, 4, Vector(Cluster.local), 500).runWith
    assertEquals(got.value, batch.value)
  }

  test("a striped stream stops at N with `parts` and resumes at N+1 with `parts'`; the answer is the batch answer") {
    for (from, to) <- Vector((4, 8), (8, 3), (2, 6), (4, 4), (6, 1), (1, 5)) do
      val j = StopAt(2)
      val stop = intercept[StopAt.Stopped](
        Cluster.stream(StripeKeyedJob, feed, from, Vector(Cluster.local), 500, j).runWith)
      assertEquals(stop.epoch, 2)
      assertEquals(j.kept.latest.map(_._1), Some(2), s"$from -> $to never reached epoch 2")

      val got = Cluster.stream(StripeKeyedJob, feed, to, Vector(Cluster.local), 500, j.kept).runWith
      assertEquals(got.value, batch.value, s"rescaled $from -> $to")
      assertEquals(got.partitions, to, s"the resumed run reports its NEW width, $from -> $to")
  }

  test("rescale and change the worker set at the same time — partitions redistribute") {
    val j = StopAt(2)
    val _ = intercept[StopAt.Stopped](
      Cluster.stream(StripeKeyedJob, feed, 4, Vector(Cluster.local), 500, j).runWith)
    val got = Cluster.stream(StripeKeyedJob, feed, 6, Vector.fill(3)(Cluster.local), 500, j.kept).runWith
    assertEquals(got.value, batch.value)
  }

  test("a SAME-width resume of a striped job is unaffected — rescale is only the width change") {
    val j = StopAt(3)
    val _ = intercept[StopAt.Stopped](
      Cluster.stream(StripeKeyedJob, feed, 4, Vector(Cluster.local), 500, j).runWith)
    val got = Cluster.stream(StripeKeyedJob, feed, 4, Vector(Cluster.local), 500, j.kept).runWith
    assertEquals(got.value, batch.value)
  }

  test("a CONTIGUOUS cut refuses to rescale — its positions do not survive a re-cut") {
    val j = StopAt(2)
    val _ = intercept[StopAt.Stopped](
      Cluster.stream(WindowJob, feed, 4, Vector(Cluster.local), 500, j).runWith)
    val e = intercept[IllegalStateException](
      Cluster.stream(WindowJob, feed, 6, Vector(Cluster.local), 500, j.kept).runWith)
    assert(e.getMessage.contains("cannot rescale"), e.getMessage)
    assert(e.getMessage.contains("contiguous cut"), e.getMessage)
  }

  test("a WINDOWED sink refuses to rescale — its open panes are not in the journal (box 2)") {
    val j = StopAt(2)
    val _ = intercept[StopAt.Stopped](
      Cluster.stream(StripeWindowJob, feed, 4, Vector(Cluster.local), 500, j).runWith)
    val e = intercept[IllegalStateException](
      Cluster.stream(StripeWindowJob, feed, 6, Vector(Cluster.local), 500, j.kept).runWith)
    assert(e.getMessage.contains("cannot rescale a WINDOWED sink"), e.getMessage)
    assert(e.getMessage.contains("box 2"), e.getMessage)
  }
}

/** a keyed (group-by) job over a STRIPED source — no window, so its
 * whole state is in the coordinator's fold and it rescales cleanly */
object StripeKeyedJob extends Job[Feed, Feeds.Sum] {
  import Feeds.*
  type A = Ev
  def name: String = "test.stripe.keyed"
  def params: Schema[Feed] = summon[Schema[Feed]]
  override def rescalable: Boolean = true
  def flow(f: Feed, parts: Int): Flow[Ev] = Flow.striped(events(f), parts)
  def sink(f: Feed): Wire[Ev, Sum] =
    Wire.keyed((e: Ev) => e.key, value)(
      Aggregator[(Int, Long), Sum, Sum](Sum(0, 0, 0))((s, kv) =>
        Sum(s.n + 1, s.total + kv._2, s.x ^ mix(kv._1 * 31 + kv._2)))((a, b) =>
        Sum(a.n + b.n, a.total + b.total, a.x ^ b.x))(identity))
}

/** a WINDOWED job that is (wrongly) marked rescalable — only so the
 * engine's box-2 refusal can be asserted; a striped source is not
 * enough when the sink keeps open panes off the journal */
object StripeWindowJob extends Job[Feed, Feeds.Sum] {
  import Feeds.*
  type A = Ev
  def name: String = "test.stripe.window"
  def params: Schema[Feed] = summon[Schema[Feed]]
  override def rescalable: Boolean = true
  def flow(f: Feed, parts: Int): Flow[Ev] = Flow.striped(events(f), parts)
  def sink(f: Feed): Wire[Ev, Sum] =
    Wire.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value, seeded = false)(paneSum)
}
