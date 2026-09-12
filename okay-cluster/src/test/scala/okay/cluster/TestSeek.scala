package okay.cluster

import okay.{Aggregator, Chunks}
import okay.codec.{Codecs, Schema}
import okay.given
import okay.persist.{Ack, MemoryStore, Policy, Streams, Topic}
import java.util.concurrent.atomic.AtomicLong

/**
 * A RESUMED RUN OPENS AT ITS POSITION — for the sinks that can
 * (specs/dataflow.md, stage 11 box 2).
 *
 * Before this box every fresh session replayed its partition from
 * zero, discarding what the coordinator had already folded. With the
 * log as the source that is the one cost a log exists to remove: a
 * position is a number, and a session can open AT it.
 *
 * BUT NOT EVERY SINK CAN, and the test says which. A keyed sink hands
 * over a DELTA each epoch and clears, so a session opened at epoch
 * N-1's position with an empty map is exactly right. A windowed sink
 * keeps its open panes inside the partition and hands them over only
 * when they close — a fresh session at a position has none of them,
 * and their contributions from before it would reach nobody. So the
 * windowed sink still replays, and this suite asserts BOTH behaviours
 * by counting what the topic was asked for, not by trusting a flag.
 */
object SeekStore {
  import Feeds.*
  given Schema[Ev] = Schema.derived
  val codec = Codecs.cbor(summon[Schema[Ev]])

  /** a topic that counts the records it hands out */
  final class Counting(t: Topic) extends Topic:
    val records = AtomicLong(0)
    def name: String = t.name
    def partitions: Int = t.partitions
    def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long =
      t.append(partition, key, value, ack)
    def read(partition: Int, from: Long, max: Int): Topic.Read =
      val out = t.read(partition, from, max)
      out match
        case Topic.Read.Records(rs) => records.addAndGet(rs.length.toLong): Unit
        case _ => ()
      out
    def begin(partition: Int): Long = t.begin(partition)
    def end(partition: Int): Long = t.end(partition)
    def compact(partition: Int): Unit = t.compact(partition)

  @volatile private var topic: Counting | Null = null
  def counting: Counting = topic.nn

  def fill(events: IndexedSeq[Ev], parts: Int): Unit =
    val t = Counting(MemoryStore().topic("events", parts, Policy(compact = false)))
    val n = events.length
    for p <- 0 until parts do
      val from = (n.toLong * p / parts).toInt
      val until = (n.toLong * (p + 1) / parts).toInt
      for i <- from until until do
        t.append(p, Array.emptyByteArray, codec.encode(events(i)), Ack.Durable): Unit
    topic = t

  /** the partition as a recipe WITH A START: `Streams.chunks` seeks
   * to the offset, which on a non-compacted partition is the element
   * index — the contiguity this relies on is `Streams.chunks`'s own
   * `DroppedHistory` contract */
  def partition(p: Int, start: Long): Chunks[Ev] =
    Chunks.map(Streams.chunks(topic.nn, p, start))(r =>
      codec.decode(r.value).fold(why => throw IllegalStateException(why), identity))

  val keySum: Aggregator[(Int, Long), Sum, Sum] =
    Aggregator[(Int, Long), Sum, Sum](Sum(0, 0, 0))((s, kv) =>
      Sum(s.n + 1, s.total + kv._2, s.x ^ mix(kv._1 * 31 + kv._2)))((a, b) =>
      Sum(a.n + b.n, a.total + b.total, a.x ^ b.x))(identity)
}

/** keyed state over the log: hands over deltas, so it can seek */
object KeyedLogJob extends Job[Feed, Feeds.Sum] {
  import Feeds.*
  type A = Ev
  def name: String = "test.log.keyed"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def flow(f: Feed, parts: Int): Flow[Ev] =
    Flow.seekable(Vector.tabulate(parts)(p => (start: Long) => SeekStore.partition(p, start)))
  def sink(f: Feed): Wire[Ev, Sum] = Wire.keyed((e: Ev) => e.key, value)(SeekStore.keySum)
}

/** windows over the same log: open panes live in the partition, so it cannot */
object WindowLogJob extends Job[Feed, Feeds.Sum] {
  import Feeds.*
  type A = Ev
  def name: String = "test.log.window"
  def params: Schema[Feed] = summon[Schema[Feed]]
  def flow(f: Feed, parts: Int): Flow[Ev] =
    Flow.seekable(Vector.tabulate(parts)(p => (start: Long) => SeekStore.partition(p, start)))
  def sink(f: Feed): Wire[Ev, Sum] =
    Wire.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)
}

class TestSeek extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()
  Jobs.register(KeyedLogJob)
  Jobs.register(WindowLogJob)
  val feed: Feed = Feed(20000, Late - 1)
  val Parts = 4
  val Take = 512

  /** TestResume's coordinator that dies after committing epoch `at` */
  final class Dying(at: Int) extends Checkpoint:
    val kept = Checkpoint.Memory()
    def save(epoch: Int, bytes: Array[Byte]): Unit =
      kept.save(epoch, bytes)
      if epoch == at then throw Dying.Died(epoch)
    def latest: Option[(Int, Array[Byte])] = kept.latest
  object Dying:
    final case class Died(epoch: Int) extends RuntimeException(s"died at epoch $epoch")

  def total: Long = events(feed).length.toLong

  test("the sinks say whether they can seek, and `and` says both") {
    assert(KeyedLogJob.sink(feed).seekable, "a keyed sink hands over deltas and can seek")
    assert(!WindowLogJob.sink(feed).seekable, "a windowed sink keeps open panes and cannot")
    assert(!KeyedLogJob.sink(feed).and(WindowLogJob.sink(feed)).seekable)
    assert(Sink.fold(Aggregator.count[Ev]).seekable)
  }

  test("Flow.slices seeks natively: a partition opened mid-way yields only its tail") {
    val xs = events(feed)
    val whole = Chunks.foldLeft(Flows.partition(Flow.slices(xs, Parts), 1, 0L))(0L)((n, _) => n + 1)
    val tail = Chunks.foldLeft(Flows.partition(Flow.slices(xs, Parts), 1, 100L))(0L)((n, _) => n + 1)
    assertEquals(tail, whole - 100L)
    // and past the end is empty, not an error
    assertEquals(Chunks.foldLeft(Flows.partition(Flow.slices(xs, Parts), 1, whole + 5))(0L)((n, _) => n + 1), 0L)
  }

  test("Flow.of skips by reading — the replay, named: the tail is right and the reads happened") {
    SeekStore.fill(events(feed), 1)
    val before = SeekStore.counting.records.get
    val plain = Flow.of(Vector(() => SeekStore.partition(0, 0L)))
    val tail = Chunks.foldLeft(Flows.partition(plain, 0, 1000L))(0L)((n, _) => n + 1)
    assertEquals(tail, total - 1000L)
    assert(SeekStore.counting.records.get - before >= total,
      "a non-seekable source skipped without reading — where did the elements go?")
  }

  test("A KEYED JOB RESUMES AT ITS POSITION: the resumed run reads less than the topic holds") {
    SeekStore.fill(events(feed), Parts)
    val reference = Flows.fan(Flow.slices(events(feed), Parts),
      Sink.keyed((e: Ev) => e.key, value)(SeekStore.keySum)).runWith
    for at <- Vector(3, 6) do
      val j = Dying(at)
      val _ = intercept[Dying.Died](
        Cluster.stream(KeyedLogJob, feed, Parts, Vector(Cluster.local), Take, j).runWith)
      assertEquals(j.latest.map(_._1), Some(at), s"the run never reached epoch $at")
      val before = SeekStore.counting.records.get
      // the workers' old sessions are gone: every partition opens afresh
      val f = Codecs.cbor(summon[Schema[Folded]]).decode(j.latest.get._2).fold(fail(_), identity)
      Vector.tabulate(Parts)(i => f.base + i).foreach(Sessions.drop)
      val got = Cluster.stream(KeyedLogJob, feed, Parts, Vector(Cluster.local), Take, j.kept).runWith
      val read = SeekStore.counting.records.get - before
      assertEquals(got.value, reference.value, s"died after epoch $at: the resumed keyed job answered wrong")
      assert(read < total, s"died after epoch $at: the resumed run read $read of $total — it replayed from zero")
      // what it read is what was left after the journal's positions
      assertEquals(read, total - f.positions.sum, s"died after epoch $at")
  }

  test("A WINDOWED JOB STILL REPLAYS, and says so by reading the whole topic again") {
    SeekStore.fill(events(feed), Parts)
    val reference = Flows.fan(Flow.slices(events(feed), Parts), WindowJob.sink(feed)).runWith
    val j = Dying(3)
    val _ = intercept[Dying.Died](
      Cluster.stream(WindowLogJob, feed, Parts, Vector(Cluster.local), Take, j).runWith)
    val f = Codecs.cbor(summon[Schema[Folded]]).decode(j.latest.get._2).fold(fail(_), identity)
    Vector.tabulate(Parts)(i => f.base + i).foreach(Sessions.drop)
    val before = SeekStore.counting.records.get
    val got = Cluster.stream(WindowLogJob, feed, Parts, Vector(Cluster.local), Take, j.kept).runWith
    val read = SeekStore.counting.records.get - before
    assertEquals(got.value, reference.value, "the resumed windowed job answered wrong")
    assertEquals(got.dropped, reference.dropped)
    assert(read >= total, s"a windowed sink cannot seek, and yet read only $read of $total")
  }

  test("a replacement worker MID-RUN opens at the position too — the same road as a resume") {
    SeekStore.fill(events(feed), Parts)
    val reference = Flows.fan(Flow.slices(events(feed), Parts),
      Sink.keyed((e: Ev) => e.key, value)(SeekStore.keySum)).runWith
    // A WORKER DIES WITH ITS SESSION, mid-stream. Both workers here are
    // `Cluster.local`, which share one session table — so a throw
    // alone models nothing: the "other" worker finds the same session
    // and continues it. The first version of this test passed with the
    // seek disabled for exactly that reason. The dying worker DROPS
    // the session it was asked for and then throws, which is what a
    // process taking its state with it looks like from the outside;
    // the survivor has no session for that partition and opens one.
    val n = AtomicLong(0)
    val dying: Cluster.Serve = req =>
      req match
        case Req.Advance(session, _, _, _) if n.incrementAndGet() == 6 =>
          Sessions.drop(session)
          throw java.io.IOException("the worker died at its sixth request")
        case other => Cluster.local(other)
    val before = SeekStore.counting.records.get
    val got = Cluster.stream(KeyedLogJob, feed, Parts, Vector(dying, Cluster.local), Take).runWith
    val read = SeekStore.counting.records.get - before
    assertEquals(got.value, reference.value)
    assert(got.failed > 0, "the death never happened")
    // it threw INSTEAD of serving, so the epoch it was asked for was
    // never read by it — the replacement opens at the position and
    // reads exactly that epoch on: the topic is read ONCE in total
    assertEquals(read, total,
      s"$read records read for $total: the replacement replayed its partition from zero")
  }
}
