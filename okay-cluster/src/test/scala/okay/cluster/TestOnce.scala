package okay.cluster

import okay.{Aggregator, Pane}
import okay.codec.Schema
import okay.given
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

/**
 * A ROW THE ENGINE WROTE SOMEWHERE (specs/dataflow.md, stage 6c).
 *
 * Every stage since 5 rests on the same trade: a partition may be
 * COMPUTED twice, and that is correct because the coordinator keeps
 * exactly one partial per partition. Nothing had ever LEFT the engine,
 * so the trade never had to be defended. The moment a pane is written
 * out, the question stops being "is the arithmetic right" and becomes
 * "did this row land twice".
 *
 * What is asserted here is the honest version, in two halves, under
 * the failure that would break it:
 *
 *   - the store holds each (window, key) ONCE, with the batch value;
 *   - and the writer was OFFERED more than that, because a partition
 *     whose reply was lost is recomputed and writes its panes again.
 *
 * The second is not a defect the first hides. It is the mechanism:
 * exactly-once OUTCOME on a keyed writer, at-least-once execution
 * underneath, and no protocol between them.
 */
object Store {
  private val rows = mutable.HashMap.empty[(Long, Int), Long]
  private var offers = 0L
  private var clashes = 0L

  /**
   * WHY THIS IS SYNCHRONIZED and a real writer would not be: the
   * partitions run on fibres and the coordinator on another, so a
   * plain HashMap here would be racing on the test's own bookkeeping
   * rather than on anything the engine does. A database keyed by
   * (start, key) is the shape this stands in for.
   */
  def write(p: Pane[Int, Long]): Unit = synchronized {
    offers += 1
    // a REPEAT that carried a different value would be the defect —
    // an offer is only harmless because replay is deterministic
    if rows.get((p.start, p.key)).exists(_ != p.value) then clashes += 1
    rows.update((p.start, p.key), p.value)
  }

  def reset(): Unit = synchronized { rows.clear(); offers = 0L; clashes = 0L }
  def snapshot: Map[(Long, Int), Long] = synchronized(rows.toMap)
  def offered: Long = synchronized(offers)
  def clashed: Long = synchronized(clashes)
}

/** the same tumbling window as `WindowJob`, written OUT instead of
 * folded into an answer the submitter reads */
object WriteJob extends Job[Feed, Long] {
  import Feeds.*
  type A = Ev
  def name: String = "test.write"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def flow(f: Feed, parts: Int): Flow[Ev] = Flow.slices(events(f), parts)
  def sink(f: Feed): Wire[Ev, Long] =
    Wire.tumblingTo(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(Store.write)
}

class TestOnce extends munit.FunSuite {
  import Feeds.*

  Jobs.register(WriteJob)
  val feed: Feed = Feed(20000, Late - 1)

  val collect: Aggregator[Pane[Int, Long], Vector[((Long, Int), Long)], Vector[((Long, Int), Long)]] =
    Aggregator[Pane[Int, Long], Vector[((Long, Int), Long)], Vector[((Long, Int), Long)]](
      Vector.empty)((b, p) => b :+ ((p.start, p.key) -> p.value))((a, b) => a ++ b)(identity)

  /** what the panes ARE — one local run, no workers, no failures */
  lazy val expected: Map[(Long, Int), Long] =
    val rows = Flows.fan(WriteJob.flow(feed, 8),
      Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(collect)).runWith.value
    assertEquals(rows.length, rows.map(_._1).distinct.length,
      "the batch run itself presented a window twice — nothing below means anything")
    rows.toMap

  /**
   * A WORKER THAT DID THE WORK AND THEN VANISHED — the case the
   * `ask` doc names and no test had ever produced.
   *
   * `TestFailure`'s doomed worker throws INSTEAD of serving, so its
   * partition is never computed and nothing is ever written twice.
   * This one serves the request in full — every pane the completeness
   * rule let that partition finish is written — and only then loses
   * the reply. The coordinator cannot tell the two apart, which is
   * exactly why the outcome has to survive both.
   */
  def losing(base: Cluster.Serve, at: Int): Cluster.Serve =
    val n = AtomicInteger(0)
    req =>
      val out = base(req)
      req match
        case _: Req.Run if n.incrementAndGet() == at =>
          throw java.io.IOException(s"the reply to run $at never arrived")
        case _ => out

  test("batch, nothing dies: each window is written once, and the run counts them") {
    Store.reset()
    val got = Cluster.run(WriteJob, feed, 8, Vector(Cluster.local, Cluster.local)).runWith
    assertEquals(Store.snapshot, expected)
    assertEquals(Store.offered, expected.size.toLong, "a pane was offered twice on a quiet run")
    assertEquals(Store.clashed, 0L)
    assertEquals(got.value, expected.size.toLong, "the run's own count is not the panes it wrote")
    assertEquals(got.retried, 0L)
  }

  test("A REPLY IS LOST: the writer is offered more than there are panes, and the store still holds each once") {
    Store.reset()
    val workers = Vector(losing(Cluster.local, 1), Cluster.local, Cluster.local)
    val got = Cluster.run(WriteJob, feed, 8, workers).runWith

    // `lost`, not `retried`: one lost reply no longer buries anybody
    // (dataflow-reconnect), and what this line is checking is that
    // the injection fired at all
    assert(got.failed > 0, "the loss was never noticed — the injection is not working")
    // THE OUTCOME: keyed by (window, key), and therefore right
    assertEquals(Store.snapshot, expected)
    assertEquals(Store.clashed, 0L, "a repeated offer carried a DIFFERENT value — replay is not deterministic")
    // THE EXECUTION: at-least-once, and this is the number that says so
    assert(Store.offered > expected.size.toLong,
      s"${Store.offered} offers for ${expected.size} panes — the recomputed partition wrote nothing, " +
        "so this test is not exercising the repeat it exists for")
    // and the ANSWER is the panes, not the offers: the lost partial
    // took its count with it and the recomputed one replaced it
    assertEquals(got.value, expected.size.toLong)
  }

  test("seeded losses: whichever reply goes missing, the store is the batch answer") {
    var seen = 0L
    for seed <- 1L to 12L do
      Store.reset()
      val at = 1 + math.floorMod(mix(seed), 4L).toInt
      val workers = Vector(losing(Cluster.local, at), Cluster.local, losing(Cluster.local, at + 1))
      val got = Cluster.run(WriteJob, feed, 8, workers).runWith
      assertEquals(Store.snapshot, expected, s"seed $seed, losing run $at")
      assertEquals(Store.clashed, 0L, s"seed $seed")
      assertEquals(got.value, expected.size.toLong, s"seed $seed")
      seen += Store.offered - expected.size
    assert(seen > 0, "twelve schedules and not one pane was ever rewritten")
  }

  /**
   * THE STREAM WRITES ON THE COORDINATOR, and that is not a second
   * mechanism — it is the completeness rule being off. A streaming
   * partition finishes nothing locally (stage 6a), so every pane
   * reaches the coordinator as an accumulator and is written when the
   * global watermark passes it. One place, one offer.
   *
   * Which makes the batch/stream difference sayable: the batch run
   * trades an extra offer for the 1.7 million panes it never sends.
   */
  test("streaming: the panes are written once, from the coordinator, and match the batch") {
    for parts <- Vector(1, 4); take <- Vector(64, 1000) do
      Store.reset()
      val got = Cluster.stream(WriteJob, feed, parts, Vector(Cluster.local), take).runWith
      assertEquals(Store.snapshot, expected, s"$parts partitions, epochs of $take")
      assertEquals(Store.offered, expected.size.toLong, s"$parts partitions, epochs of $take")
      assertEquals(got.value, expected.size.toLong, s"$parts partitions, epochs of $take")
  }
}
