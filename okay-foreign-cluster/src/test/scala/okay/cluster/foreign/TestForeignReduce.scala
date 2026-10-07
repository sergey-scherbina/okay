package okay.cluster.foreign
import okay.freer.given
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
final class FakeReducer(diesOnce: Boolean = false) extends Reducer[Rec, Stat]:
  val name = "fake:stat"
  private val lock = Object()
  private var steps = Vector.empty[(Int, Boolean)]
  @volatile var merges = 0
  private val dies = java.util.concurrent.atomic.AtomicBoolean(diesOnce)
  @volatile var died = 0
  def seen: Vector[(Int, Boolean)] = lock.synchronized(steps)
  def step(acc: Option[Stat], rows: Vector[Rec]): Either[Batcher.Failed, Stat] =
    lock.synchronized { steps :+= (rows.length, acc.isEmpty) }
    // ONE death, whichever partition gets there first: a check-then-set
    // on a @volatile var let two workers both see `true` and both die
    // (died == 2 in a whole-build gate, 2026-09-26)
    if dies.compareAndSet(true, false) then
      died += 1
      Left(Batcher.Failed("WorkerDied", "the fake died"))
    else
      val here = Stat.of(rows).get
      Right(acc.fold(here)(Stat.merge(_, here)))
  def merge(a: Stat, b: Stat): Either[Batcher.Failed, Stat] =
    merges += 1
    Right(Stat.merge(a, b))

/** a job with ITS OWN reducer, under a name no other test uses — the
 * shared-variable reducer was the flake's suspect (foreign-reduce-wire-
 * heal-flake): a partition of one test read another's */
final class ReduceJob(val name: String, reducer: Reducer[Rec, Stat], batch: Int) extends Job[Scale, Option[Stat]]:
  type A = Rec
  def params: Schema[Scale] = summon[Schema[Scale]]
  def answer: Schema[Option[Stat]] = Schema.SOption(() => summon[Schema[Stat]])
  def flow(p: Scale, parts: Int): Flow[Rec] = Flow.slices(Rows.of(p.n), parts, chunk = 256)
  def sink(p: Scale): Wire[Rec, Option[Stat]] = Reduce.through(reducer, batch)

object ReduceJobs:
  private val n = java.util.concurrent.atomic.AtomicInteger(0)
  /** this test's job, registered as itself */
  def of(reducer: Reducer[Rec, Stat], batch: Int = 1000): ReduceJob =
    val job = ReduceJob(s"test.foreign.reduce.stat.${n.incrementAndGet()}", reducer, batch)
    Jobs.register(job)
    job

/** the reduce without an interpreter: the wire's plumbing, the two
 * failure roads, the batch, the coordinator's merge (default gate) */
class TestForeignReduce extends munit.FunSuite:

  private def local(job: ReduceJob, p: Scale, parts: Int) =
    Flows.fan(job.flow(p, parts), job.sink(p)).runWith
  private def cluster(job: ReduceJob, p: Scale, parts: Int, workers: Int) =
    Cluster.run(job, p, parts, Vector.fill(workers)(Cluster.local)).runWith

  test("the reduce through a reducer computes, over in-process workers, what the fan and the JVM compute: count, sum, max") {
    val job = ReduceJobs.of(FakeReducer())
    val p = Scale(10000)
    val expected = Stat.of(Rows.of(10000))
    assertEquals(local(job, p, 4).value, expected)
    for workers <- Vector(1, 3) do
      assertEquals(cluster(job, p, 4, workers).value, expected, s"$workers workers")
  }

  test("no rows answers None; a partition shorter than `batch` still folds") {
    val job = ReduceJobs.of(FakeReducer())
    assertEquals(cluster(job, Scale(0), 2, 2).value, None)
    assertEquals(cluster(job, Scale(7), 2, 2).value, Stat.of(Rows.of(7)))
  }

  test("`step` sees `batch`-row chunks with None first on every partition; `merge` runs partitions - 1 times on the coordinator") {
    val fake = FakeReducer()
    val job = ReduceJobs.of(fake)
    assertEquals(local(job, Scale(10000), 4).value, Stat.of(Rows.of(10000)))
    // four partitions of 2500 rows: 1000 (None), 1000 (Some), 500 (Some) each
    assertEquals(fake.seen.map(_._1).sorted, Vector.fill(4)(500) ++ Vector.fill(8)(1000))
    assertEquals(fake.seen.count(_._2), 4, "None as the first acc of every partition")
    assertEquals(fake.merges, 3)
  }

  test("the FUNCTION's failure fails the run by name, as a considered refusal") {
    val job = ReduceJobs.of(new Reducer[Rec, Stat]:
      val name = "fake:boom"
      def step(acc: Option[Stat], rows: Vector[Rec]) = Left(Batcher.Failed("ValueError", "no"))
      def merge(a: Stat, b: Stat) = Right(a))
    val e = intercept[Throwable](cluster(job, Scale(1000), 2, 2))
    assert(e.getMessage.contains("fake:boom") && e.getMessage.contains("ValueError: no"), e.getMessage)
  }

  test("a WIRE failure heals on a fresh interpreter, invisibly to the coordinator") {
    val fake = FakeReducer(diesOnce = true)
    val job = ReduceJobs.of(fake)
    val got = cluster(job, Scale(10000), 4, 2)
    assertEquals(got.value, Stat.of(Rows.of(10000)))
    assertEquals(fake.died, 1)
    assertEquals(got.retried, 0L)
  }
