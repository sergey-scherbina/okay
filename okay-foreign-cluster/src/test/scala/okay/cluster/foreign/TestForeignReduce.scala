package okay.cluster.foreign

import okay.given
import okay.cluster.{Cluster, Flow, Flows, Job, Jobs, Wire}
import okay.codec.Schema

/** count, sum and max of `Rec.v` — a partial and the answer */
final case class Stat(n: Long, sum: Long, max: Long) derives Schema

object Stat:
  def of(rows: IndexedSeq[Rec]): Option[Stat] =
    if rows.isEmpty then None else Some(Stat(rows.length, rows.map(_.v).sum, rows.map(_.v).max))
  def merge(a: Stat, b: Stat): Stat = Stat(a.n + b.n, a.sum + b.sum, math.max(a.max, b.max))

/** a reducer inside the JVM: records every step's chunk size and whether
 * it opened a partition, counts merges, dies once when told to */
final class FakeReducer(@volatile var diesOnce: Boolean = false) extends Reducer[Rec, Stat]:
  val name = "fake:stat"
  private val lock = Object()
  private var steps = Vector.empty[(Int, Boolean)]
  @volatile var merges = 0
  @volatile var died = 0
  def seen: Vector[(Int, Boolean)] = lock.synchronized(steps)
  def step(acc: Option[Stat], rows: Vector[Rec]): Either[Batcher.Failed, Stat] =
    lock.synchronized { steps :+= (rows.length, acc.isEmpty) }
    if diesOnce then
      diesOnce = false
      died += 1
      Left(Batcher.Failed("WorkerDied", "the fake died"))
    else
      val here = Stat.of(rows).get
      Right(acc.fold(here)(Stat.merge(_, here)))
  def merge(a: Stat, b: Stat): Either[Batcher.Failed, Stat] =
    merges += 1
    Right(Stat.merge(a, b))

object ReduceJobs:
  @volatile var reducer: Reducer[Rec, Stat] = FakeReducer()
  @volatile var batch: Int = 1000

  object Stats extends Job[Scale, Option[Stat]]:
    type A = Rec
    def name: String = "test.foreign.reduce.stat"
    def params: Schema[Scale] = summon[Schema[Scale]]
    def answer: Schema[Option[Stat]] = Schema.SOption(() => summon[Schema[Stat]])
    def flow(p: Scale, parts: Int): Flow[Rec] = Flow.slices(Rows.of(p.n), parts, chunk = 256)
    def sink(p: Scale): Wire[Rec, Option[Stat]] = Reduce.through(reducer, batch)

  Jobs.register(Stats)
  def install(): Unit = ()

/** the reduce without an interpreter: the wire's plumbing, the two
 * failure roads, the batch, the coordinator's merge (default gate) */
class TestForeignReduce extends munit.FunSuite:
  ReduceJobs.install()

  private def local(p: Scale, parts: Int) =
    Flows.fan(ReduceJobs.Stats.flow(p, parts), ReduceJobs.Stats.sink(p)).runWith
  private def cluster(p: Scale, parts: Int, workers: Int) =
    Cluster.run(ReduceJobs.Stats, p, parts, Vector.fill(workers)(Cluster.local)).runWith

  override def beforeEach(context: BeforeEach): Unit =
    ReduceJobs.reducer = FakeReducer(); ReduceJobs.batch = 1000

  test("the reduce through a reducer computes, over in-process workers, what the fan and the JVM compute: count, sum, max") {
    val p = Scale(10000)
    val expected = Stat.of(Rows.of(10000))
    assertEquals(local(p, 4).value, expected)
    for workers <- Vector(1, 3) do
      assertEquals(cluster(p, 4, workers).value, expected, s"$workers workers")
  }

  test("no rows answers None; a partition shorter than `batch` still folds") {
    assertEquals(cluster(Scale(0), 2, 2).value, None)
    assertEquals(cluster(Scale(7), 2, 2).value, Stat.of(Rows.of(7)))
  }

  test("`step` sees `batch`-row chunks with None first on every partition; `merge` runs partitions - 1 times on the coordinator") {
    val fake = FakeReducer()
    ReduceJobs.reducer = fake
    assertEquals(local(Scale(10000), 4).value, Stat.of(Rows.of(10000)))
    // four partitions of 2500 rows: 1000 (None), 1000 (Some), 500 (Some) each
    assertEquals(fake.seen.map(_._1).sorted, Vector.fill(4)(500) ++ Vector.fill(8)(1000))
    assertEquals(fake.seen.count(_._2), 4, "None as the first acc of every partition")
    assertEquals(fake.merges, 3)
  }

  test("the FUNCTION's failure fails the run by name, as a considered refusal") {
    ReduceJobs.reducer = new Reducer[Rec, Stat]:
      val name = "fake:boom"
      def step(acc: Option[Stat], rows: Vector[Rec]) = Left(Batcher.Failed("ValueError", "no"))
      def merge(a: Stat, b: Stat) = Right(a)
    val e = intercept[Throwable](cluster(Scale(1000), 2, 2))
    assert(e.getMessage.contains("fake:boom") && e.getMessage.contains("ValueError: no"), e.getMessage)
  }

  test("a WIRE failure heals on a fresh interpreter, invisibly to the coordinator") {
    val fake = FakeReducer(diesOnce = true)
    ReduceJobs.reducer = fake
    val got = cluster(Scale(10000), 4, 2)
    assertEquals(got.value, Stat.of(Rows.of(10000)))
    assertEquals(fake.died, 1)
    assertEquals(got.retried, 0L)
  }
