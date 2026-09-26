package okay.cluster

import okay.given
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.jdk.CollectionConverters.*

/**
 * THE EXCHANGE ACROSS PROCESSES (specs/dataflow.md, stage 14).
 *
 * The bar is stage 4b's: EQUAL to what one process computes — here a
 * plain `groupMapReduce` over the same events, written without the
 * engine, so the engine is not graded against itself.
 *
 * In-process workers first, each `Cluster.exchanging` with its OWN
 * store and reaching the others through a directory of addresses, so
 * killing one loses exactly what a killed process would. Then four
 * real processes.
 */
class TestShuffle extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()
  val feed: Feed = Feed(20000, Late - 1)

  def liveTest(name: String)(body: => Any): Unit = test(name.tag(new munit.Tag("Live")))(body)

  /** the one-process answer, without the engine */
  lazy val expected: Sum =
    val sums = events(feed).groupMapReduce(ShuffleJob.slot)(_.v.toLong)(_ + _)
    val hist = sums.values.groupMapReduce(s => math.floorMod(s, 13L).toInt)(_ => 1L)(_ + _)
    hist.foldLeft(Sum(0, 0, 0))((s, kv) => ShuffleJob.histogram.add(s, kv))

  /** a worker that can die: once `dead`, every request throws, from
   * the coordinator and from its peers alike */
  final class Box(val address: String, dies: Req => Boolean):
    val dead = AtomicBoolean(false)
    val seen = ConcurrentLinkedQueue[Req]()
    var inner: Cluster.Serve = Cluster.local
    val serve: Cluster.Serve = req =>
      seen.add(req): Unit
      if !dead.get && dies(req) then dead.set(true)
      if dead.get then throw java.io.IOException(s"$address is gone")
      inner(req)

  /** `n` exchanging workers reaching each other by address */
  def cluster(n: Int, dies: Int => Req => Boolean = _ => _ => false): Vector[Box] =
    val boxes = Vector.tabulate(n)(i => Box(s"w$i", dies(i)))
    val directory = boxes.map(b => b.address -> b.serve).toMap
    for b <- boxes do b.inner = Cluster.exchanging(b.address, directory)
    boxes

  def peers(boxes: Vector[Box]): Vector[Cluster.Peer] = boxes.map(b => Cluster.Peer(b.address, b.serve))

  test("a two-stage keyed job over 4 workers answers what one process answers") {
    assert(expected.n > 5, s"the histogram is too small to assert anything: $expected")
    for parts <- Vector(1, 4, 8); reducers <- Vector(1, 3, 4) do
      val got = Cluster.shuffle(ShuffleJob, feed, parts, reducers, peers(cluster(4))).runWith
      assertEquals(got.value, expected, s"$parts partitions, $reducers reducers")
      assertEquals(got.reducers, reducers)
  }

  test("the buckets travel worker to worker: the coordinator sends and receives none") {
    val boxes = cluster(4)
    val answers = ConcurrentLinkedQueue[Resp]()
    val watched = boxes.map(b => Cluster.Peer(b.address, (req: Req) => { val r = b.serve(req); answers.add(r): Unit; r }))
    val got = Cluster.shuffle(ShuffleJob, feed, 8, 4, watched).runWith
    assertEquals(got.value, expected)
    assert(!answers.asScala.exists(_.isInstanceOf[Resp.Bucket]), "a bucket reached the coordinator")
    val fetches = boxes.map(_.seen.asScala.count(_.isInstanceOf[Req.Fetch])).sum
    // 4 reducers x 8 partitions, less the ones each reducer held itself
    assert(fetches >= 4 * 8 - 8, s"only $fetches fetches crossed between workers")
  }

  test("a reducer killed mid-exchange: its share is asked of a survivor, the answer unchanged") {
    val boxes = cluster(4, i => req => i == 1 && req.isInstanceOf[Req.Reduce])
    val got = Cluster.shuffle(ShuffleJob, feed, 8, 4, peers(boxes)).runWith
    assert(boxes(1).dead.get, "the reducer never died — the test tested nothing")
    assertEquals(got.value, expected)
    assert(got.failed > 0, "no attempt was lost")
  }

  test("a map holder killed after the map side: its buckets are LOST, re-mapped, and the answer unchanged") {
    val boxes = cluster(4, i => req => i == 2 && req.isInstanceOf[Req.Fetch])
    val maps = AtomicInteger(0)
    val counted = peers(boxes).map(p => p.copy(serve = (req: Req) => {
      if req.isInstanceOf[Req.Shuffle] then maps.incrementAndGet(): Unit
      p.serve(req)
    }))
    val got = Cluster.shuffle(ShuffleJob, feed, 8, 4, counted).runWith
    assert(boxes(2).dead.get, "the holder never died — the test tested nothing")
    assertEquals(got.value, expected)
    // worker 2 held partitions 2 and 6; both ran their map side again
    assert(maps.get >= 8 + 2, s"${maps.get} map requests: the lost partitions were not re-mapped")
  }

  test("a run's buckets are dropped from every worker when it ends") {
    val boxes = cluster(4)
    val _ = Cluster.shuffle(ShuffleJob, feed, 8, 4, peers(boxes)).runWith
    val id = boxes.iterator.flatMap(_.seen.asScala).collectFirst { case Req.Shuffle(_, _, _, _, _, s) => s }.get
    for b <- boxes; part <- 0 until 8 do
      b.inner(Req.Fetch(id, part, 0)) match
        case Resp.Failed(why) => assert(why.startsWith("no bucket"), why)
        case other => fail(s"${b.address} still holds partition $part: $other")
  }

  test("a second stage with event-time windows is refused by name") {
    object Windowed extends Shuffled[Feed, Sum] {
      type A = Ev
      type K = Long
      type Acc = Long
      type O = Long
      def name: String = "test.shuffle.windowed"
      def params: okay.codec.Schema[Feed] = summon[okay.codec.Schema[Feed]]
      def answer: okay.codec.Schema[Sum] = summon[okay.codec.Schema[Sum]]
      def flow(f: Feed, parts: Int): Flow[Ev] = Flow.slices(events(f), parts)
      def key(f: Feed): Ev => Long = ShuffleJob.slot
      def agg(f: Feed): okay.Aggregator[Ev, Long, Long] = value
      def keys: okay.codec.Schema[Long] = summon[okay.codec.Schema[Long]]
      def accs: okay.codec.Schema[Long] = summon[okay.codec.Schema[Long]]
      def andThen(f: Feed): Wire[(Long, Long), Sum] =
        Wire.tumbling(Size, Late, (kv: (Long, Long)) => kv._1.toInt, (kv: (Long, Long)) => kv._2,
          okay.Aggregator.sum[Long].contramap[(Long, Long)](_._2))(
          okay.Aggregator[okay.Pane[Int, Long], Sum, Sum](Sum(0, 0, 0))((s, _) => s)((a, _) => a)(identity))
    }
    val e = intercept[IllegalArgumentException](
      Cluster.shuffle(Windowed, feed, 4, 2, peers(cluster(2))).runWith)
    assert(e.getMessage.contains("event-time order"), e.getMessage)
  }

  liveTest("FOUR REAL PROCESSES: one killed as the reduce side starts, and the answer is the one-process answer") {
    val procs = Workers.spawn(4, "okay.cluster.TestJobs$")
    try
      val ports = Workers.ports(procs).map(_.split(' ')(2).toInt)
      val killed = AtomicBoolean(false)
      val workers = ports.zipWithIndex.map { (port, i) =>
        val address = s"127.0.0.1:$port"
        val base = Served.reconnecting("127.0.0.1", port)
        Cluster.Peer(address, req =>
          if i == 1 && req.isInstanceOf[Req.Reduce] && killed.compareAndSet(false, true) then
            procs(1).destroyForcibly().waitFor(): Unit
          base(req))
      }
      val got = Cluster.shuffle(ShuffleJob, feed, 8, 4, workers).runWith
      assert(killed.get, "no process was killed — the test tested nothing")
      assertEquals(got.value, expected)
      assert(got.failed > 0, "no attempt was lost")
    finally procs.foreach(_.destroyForcibly(): Unit)
  }
}
