package okay.kafka

import okay.given
import okay.cluster.{Cluster, Feed, Feeds, Flow, Flows, Job, Jobs, WindowJob, Wire}
import okay.codec.{Codecs, Schema}
import java.util.concurrent.{CountDownLatch, TimeUnit}

/** a stream whose staged epochs go to an `EpochLog` */
object EpochLogJob extends Job[Feed, Long] {
  import Feeds.*
  @volatile var log: EpochLog | Null = null
  val pane = Codecs.cbor(summon[Schema[(Long, Int, Long)]])
  type A = Ev
  def name: String = "test.epochlog"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def answer: Schema[Long] = summon[Schema[Long]]
  def flow(f: Feed, parts: Int): Flow[Ev] = Flow.slices(events(f), parts)
  def sink(f: Feed): Wire[Ev, Long] =
    Wire.tumblingStaged(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)((epoch, panes) =>
      val _ = log.nn.move(epoch, panes.iterator.map(p => pane.encode((p.start, p.key, p.value)))))
}

/**
 * AN EPOCH LARGER THAN A RECORD, EXACTLY ONCE (specs/dataflow.md,
 * staging-large-epochs).
 *
 * The framing runs without a broker. Everything else is LIVE — it needs
 * Kafka on 9092 (`docker run -p 9092:9092 apache/kafka:3.9.0`), skips in
 * milliseconds without one, and is out of the default gate.
 */
class TestEpochLog extends munit.FunSuite {
  Jobs.register(EpochLogJob)

  override def munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(600, "s")

  val bootstrap: String = sys.env.getOrElse("OKAY_KAFKA", "127.0.0.1:9092")

  def live(name: String)(body: => Any): Unit =
    test(name.tag(new munit.Tag("Live"))) {
      assume(TestKafkaSupport.reachable(bootstrap), s"no Kafka on $bootstrap")
      body
    }

  /** row `i` of `epoch`: a KiB, and which row it is written into it */
  def row(epoch: Int, i: Int): Array[Byte] =
    val b = java.nio.ByteBuffer.allocate(1024)
    val _ = b.putInt(epoch).putInt(i)
    while b.remaining() >= 8 do { val _ = b.putLong(Feeds.mix(epoch * 1000003L + i)) }
    b.array()

  test("the framing round-trips, and a row larger than a chunk rides alone") {
    val rows = Vector.tabulate(50)(i => Array.fill[Byte](i * 7)(i.toByte)) :+ Array.fill[Byte](5000)(9)
    val chunks = EpochLog.chunks(rows.iterator, 1000).toVector
    assert(chunks.length > 1, "nothing was cut")
    assert(chunks.forall(c => c.length <= 1000 || EpochLog.rows(c).length == 1), "a chunk over the limit holds more than its one row")
    assertEquals(chunks.flatMap(EpochLog.rows).map(_.toVector), rows.map(_.toVector))
  }

  live("an epoch of 100 MB, its writer killed between chunks: a reader sees it whole, once, or not at all") {
    val topic = s"okay-epochs-${System.nanoTime()}"
    val tid = s"$topic-writer"
    val n = 100 * 1024                      // rows of a KiB: 100 MiB
    val first = EpochLog(bootstrap, topic, tid)
    assert(first.move(1, Iterator.tabulate(1000)(row(1, _))))

    // the writer of epoch 2 stops half way through its rows and never
    // comes back: its transaction is open, its chunks are on the broker
    val halfway = CountDownLatch(1)
    val never = CountDownLatch(1)
    val dying = Thread(() =>
      try first.move(2, Iterator.tabulate(n) { i =>
        if i == n / 2 then { halfway.countDown(); never.await() }
        row(2, i)
      }): Unit
      catch case _: Throwable => ())
    dying.setDaemon(true)
    dying.start()
    assert(halfway.await(5, TimeUnit.MINUTES), "the writer never got half way")
    Thread.sleep(2000)                      // its sends reach the broker

    assertEquals(EpochLog.epochs(bootstrap, topic).keySet, Set(1), "a half-written epoch is visible")

    // a successor under the same transactional id fences it
    val next = EpochLog(bootstrap, topic, tid)
    assertEquals(next.committed, 1, "the successor did not learn epoch 1 from the log")
    assert(next.move(2, Iterator.tabulate(n)(row(2, _))), "the successor skipped an epoch never committed")
    assert(!next.move(2, Iterator.tabulate(n)(row(2, _))), "a committed epoch was written twice")

    val seen = EpochLog.epochs(bootstrap, topic)
    assertEquals(seen.keySet, Set(1, 2))
    assertEquals(seen(2).length, n, "epoch 2 is not whole")
    assert(seen(2).indices.forall(i => java.util.Arrays.equals(seen(2)(i), row(2, i))), "epoch 2's rows are not the rows written")

    never.countDown()                       // the dead writer wakes, fenced
    dying.join(60000)
    assertEquals(EpochLog(bootstrap, topic, tid).committed, 2, "a third writer did not learn epoch 2")
    assertEquals(EpochLog.epochs(bootstrap, topic)(2).length, n, "the fenced writer changed epoch 2")
    next.close()
  }

  live("a stream staged into an EpochLog: every pane, each epoch once") {
    val topic = s"okay-epochs-stream-${System.nanoTime()}"
    EpochLogJob.log = EpochLog(bootstrap, topic, s"$topic-writer", chunkBytes = 4096)
    val feed = Feed(20000, Feeds.Late - 1)
    val panes = Flows.fan(WindowJob.flow(feed, 4), WindowJob.sink(feed)).runWith.value.n
    val _ = Cluster.stream(EpochLogJob, feed, 4, Vector(Cluster.local), 512).runWith
    val seen = EpochLog.epochs(bootstrap, topic)
    assert(seen.size > 2, s"${seen.size} epochs")
    assertEquals(seen.values.map(_.length.toLong).sum, panes, "the topic does not hold every pane once")
  }
}
