package okay.cluster

import okay.Chunks
import okay.codec.{Codecs, Schema}
import okay.given
import okay.persist.{Ack, MemoryStore, Policy, Streams, Topic}

/**
 * THE LOG IS THE SOURCE (specs/dataflow.md, stage 11, box 1).
 *
 * Every job so far DERIVED its partition from parameters, which was
 * the honest choice for a benchmark and is not a source. Here the
 * partition is a topic partition of okay-persist — the repository's
 * one primitive — read through `Streams.chunks`, and the job answers
 * what the fan over the array answers, in a batch run and in a
 * stream. Same slices, same order, same eleven-of-whatever the
 * checksum counts: not "close", equal.
 *
 * The topic is a static because the workers are in-process; a worker
 * PROCESS would open the store by name from the job's parameters,
 * which is box 5 and needs a broker.
 */
object LogSource {
  import Feeds.*
  given Schema[Ev] = Schema.derived
  val codec = Codecs.cbor(summon[Schema[Ev]])

  @volatile private var topic: Topic | Null = null

  /** partition p holds the p-th contiguous slice — the cut `Flow.slices`
   * makes, so the two plans are over the same partitions */
  def fill(events: IndexedSeq[Ev], parts: Int): Unit =
    val t = MemoryStore().topic("events", parts, Policy(compact = false))
    val n = events.length
    for p <- 0 until parts do
      val from = (n.toLong * p / parts).toInt
      val until = (n.toLong * (p + 1) / parts).toInt
      for i <- from until until do
        t.append(p, Array.emptyByteArray, codec.encode(events(i)), Ack.Durable): Unit
    topic = t

  def partition(p: Int): Chunks[Ev] =
    Chunks.map(Streams.chunks(topic.nn, p, 0L))(r =>
      codec.decode(r.value).fold(why => throw IllegalStateException(why), identity))
}

/** the tumbling job of `WindowJob`, read from the log instead */
object LogJob extends Job[Feed, Feeds.Sum] {
  import Feeds.*
  type A = Ev
  def name: String = "test.log"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def flow(f: Feed, parts: Int): Flow[Ev] =
    Flow.of(Vector.tabulate(parts)(p => () => LogSource.partition(p)))
  def sink(f: Feed): Wire[Ev, Sum] =
    Wire.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)
}

class TestSourceLog extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()
  Jobs.register(LogJob)
  val feed: Feed = Feed(20000, Late - 1)

  test("a job over topic partitions answers what the fan over the array answers") {
    for parts <- Vector(1, 4, 8) do
      LogSource.fill(events(feed), parts)
      val array = Flows.fan(Flow.slices(events(feed), parts), WindowJob.sink(feed)).runWith
      val log = Cluster.run(LogJob, feed, parts, Vector(Cluster.local)).runWith
      assertEquals(log.value, array.value, s"$parts partitions")
      assertEquals(log.dropped, array.dropped, s"$parts partitions")
      // the same slices in the same order see the same boundaries
      assertEquals(log.merged, array.merged, s"$parts partitions")
  }

  test("and streamed, epoch by epoch, from the log") {
    LogSource.fill(events(feed), 4)
    val array = Flows.fan(Flow.slices(events(feed), 4), WindowJob.sink(feed)).runWith
    for take <- Vector(64, 1000) do
      val log = Cluster.stream(LogJob, feed, 4, Vector(Cluster.local), take).runWith
      assertEquals(log.value, array.value, s"epochs of $take")
      assertEquals(log.dropped, array.dropped, s"epochs of $take")
  }

  test("a partition is a recipe: the log is read again on every run, and answers the same") {
    LogSource.fill(events(feed), 4)
    val a = Cluster.run(LogJob, feed, 4, Vector(Cluster.local)).runWith
    val b = Cluster.run(LogJob, feed, 4, Vector(Cluster.local)).runWith
    assertEquals(a.value, b.value)
  }
}
