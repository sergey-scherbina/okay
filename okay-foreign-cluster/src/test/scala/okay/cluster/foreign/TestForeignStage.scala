package okay.cluster.foreign

import okay.{Aggregator, given}
import okay.cluster.{Cluster, Flow, Flows, Job, Jobs, Wire}
import okay.codec.Schema

final case class Rec(key: Int, v: Long) derives Schema
final case class Out(key: Int, v: Long) derives Schema
final case class Scale(n: Int) derives Schema

object Rows:
  def of(n: Int): IndexedSeq[Rec] = (0 until n).map(i => Rec(i % 7, (i * 31L) % 1000))
  def doubled(n: Int): Long = of(n).map(_.v * 2).sum

/** a batcher inside the JVM: doubles, records every chunk's size, and
 * dies once when told to */
final class Fake(@volatile var diesOnce: Boolean = false) extends Batcher[Rec, Out]:
  val name = "fake:double"
  private val lock = Object()
  private var seen = Vector.empty[Int]
  def sizes: Vector[Int] = lock.synchronized(seen)
  @volatile var died = 0
  def apply(rows: Vector[Rec]): Either[Batcher.Failed, Vector[Out]] =
    lock.synchronized { seen :+= rows.length }
    if diesOnce then
      diesOnce = false
      died += 1
      Left(Batcher.Failed("WorkerDied", "the fake died"))
    else Right(rows.map(r => Out(r.key, r.v * 2)))

/** the job a worker is asked for by name; its batcher is whatever the
 * test installed, which every in-process worker of this JVM shares */
object StageJobs:
  @volatile var batcher: Batcher[Rec, Out] = Fake()
  @volatile var batch: Int = 1000
  @volatile var attempts: Int = 3

  object Doubling extends Job[Scale, Long]:
    type A = Out
    def name: String = "test.foreign.double"
    def params: Schema[Scale] = summon[Schema[Scale]]
    def answer: Schema[Long] = summon[Schema[Long]]
    def flow(p: Scale, parts: Int): Flow[Out] =
      Flow.slices(Rows.of(p.n), parts, chunk = 256).through(batcher, batch, attempts)
    def sink(p: Scale): Wire[Out, Long] = Wire.fold(Aggregator.sum[Long].contramap[Out](_.v))

  Jobs.register(Doubling)
  def install(): Unit = ()

/** the stage without an interpreter: the plumbing, the two failure roads,
 * the batch, the pool (default gate) */
class TestForeignStage extends munit.FunSuite:
  StageJobs.install()

  private def local(p: Scale, parts: Int) =
    Flows.fan(StageJobs.Doubling.flow(p, parts), StageJobs.Doubling.sink(p)).runWith
  private def cluster(p: Scale, parts: Int, workers: Int) =
    Cluster.run(StageJobs.Doubling, p, parts, Vector.fill(workers)(Cluster.local)).runWith

  override def beforeEach(context: BeforeEach): Unit =
    StageJobs.batcher = Fake(); StageJobs.batch = 1000; StageJobs.attempts = 3

  test("the map through a batcher computes, over in-process workers, what the fan computes: every row, doubled") {
    val p = Scale(10000)
    assertEquals(local(p, 4).value, Rows.doubled(10000))
    for workers <- Vector(1, 3) do
      assertEquals(cluster(p, 4, workers).value, Rows.doubled(10000), s"$workers workers")
  }

  test("a chunk is `batch` rows whatever the source's chunk size: one round trip per batch, the last one shorter") {
    val fake = Fake()
    StageJobs.batcher = fake
    assertEquals(local(Scale(10000), 4).value, Rows.doubled(10000))
    // four partitions of 2500 rows, from a source chunked at 256: 1000, 1000, 500 each
    assertEquals(fake.sizes.sorted, Vector.fill(4)(500) ++ Vector.fill(8)(1000))
  }

  test("the FUNCTION's failure fails the run by name, as a considered refusal") {
    StageJobs.batcher = new Batcher[Rec, Out]:
      val name = "fake:boom"
      def apply(rows: Vector[Rec]) = Left(Batcher.Failed("ValueError", "no"))
    val e = intercept[Throwable](cluster(Scale(1000), 2, 3))
    assert(e.getMessage.contains("fake:boom") && e.getMessage.contains("ValueError: no"), e.getMessage)
    val f = intercept[Throwable](local(Scale(1000), 2))
    assert(f.getMessage.contains("fake:boom"), f.getMessage)
  }

  test("a WIRE failure is retried on a fresh interpreter, and the answer is intact") {
    val fake = Fake(diesOnce = true)
    StageJobs.batcher = fake
    assertEquals(cluster(Scale(10000), 4, 2).value, Rows.doubled(10000))
    assertEquals(fake.died, 1)
    assertEquals(cluster(Scale(10000), 4, 2).retried, 0L, "the coordinator saw nothing: the stage healed itself")
  }

  test("past `attempts` a dead wire is a dead worker: the run says so, naming the stage") {
    StageJobs.batcher = new Batcher[Rec, Out]:
      val name = "fake:gone"
      def apply(rows: Vector[Rec]) = Left(Batcher.Failed("WorkerDied", "always"))
    StageJobs.attempts = 2
    val e = intercept[Throwable](cluster(Scale(1000), 2, 2))
    val text = Iterator.iterate(e)(_.getCause).takeWhile(_ != null).map(_.getMessage).mkString(" | ")
    assert(text.contains("fake:gone") && text.contains("2 attempts"), text)
  }

  test("a row type that is not a flat case class is refused when the stage is BUILT, not at the first chunk") {
    val mod = okay.foreign.Foreign.module("nothing", "def f(frame): return frame")
    val e = intercept[IllegalArgumentException](Flow.slices(Rows.of(10), 1).mapPy[Long](mod, "f"))
    assert(e.getMessage.contains("flat case class"), e.getMessage)
  }

  test("a pool opens at most `size` interpreters under more concurrent chunks than that, reuses them, and replaces a dead one") {
    val pool = okay.foreign.Pool[Object]("p", 2, () => Object(), _ => true, _ => ())
    val threads = (1 to 4).map(_ => Thread(() => pool.use { _ => Thread.sleep(40); ((), false) }))
    threads.foreach(_.start()); threads.foreach(_.join())
    assertEquals(pool.opened, 2)
    assertEquals(pool.live, 2)
    pool.use(_ => ((), true))
    assertEquals(pool.live, 1)
    pool.use(_ => ((), false))
    assertEquals((pool.opened, pool.live), (2, 1))
    pool.closeAll()
    assertEquals(pool.live, 0)
  }
