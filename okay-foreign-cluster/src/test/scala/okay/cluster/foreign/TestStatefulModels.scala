package okay.cluster.foreign

import okay.given
import okay.cluster.{Cluster, Flow, Flows, Job, Jobs, Wire}
import okay.codec.Schema

final case class Factor(by: Long) derives Schema
/** a running sum within a partition: `v` is the row's value, `run` the sum so far there */
final case class Run(key: Int, v: Long, run: Long) derives Schema

object JvmStateful:
  // partitions run on threads: counted atomically, or a lost update reads 3
  val opened = java.util.concurrent.atomic.AtomicInteger(0)
  val finished = java.util.concurrent.atomic.AtomicInteger(0)
  val mod: JvmModule = JvmModule("stateful")
    // held: a "model" made once from its parameters
    .model[Factor, Long]("fit")(f => f.by)
    .mapWith[Rec, Long, Out]("scale")((rows, by) => rows.map(r => Out(r.key, r.v * by)))
    // stream: the running sum of a partition, and a final row of -1 with the total
    .stream[Rec, Array[Long], Run]("open", "step", "finish")(
      () => { opened.incrementAndGet(): Unit; Array(0L) },
      (s, rows) => rows.map { r => s(0) += r.v; Run(r.key, r.v, s(0)) },
      s => { finished.incrementAndGet(): Unit; Vector(Run(-1, 0, s(0))) })

/** one job text for the map with a model — a `Model` made by `Model.in` — and one
 * for the stream, each over any module type with the extension */
final class ScaleJob[M](val name: String, mod: M)(using Models[M]) extends Job[Scale, Long]:
  type A = Out
  def params: Schema[Scale] = summon[Schema[Scale]]
  def answer: Schema[Long] = summon[Schema[Long]]
  def flow(p: Scale, parts: Int): Flow[Out] =
    Flow.slices(Rows.of(p.n), parts).mapModel[Out](Model.in(mod, "fit", Factor(3)), "scale")
  def sink(p: Scale): Wire[Out, Long] = Wire.fold(okay.Aggregator.sum[Long].contramap[Out](_.v))

final class RunningJob[M](val name: String, mod: M)(using Stateful[M]) extends Job[Scale, (Long, Long)]:
  type A = Run
  def params: Schema[Scale] = summon[Schema[Scale]]
  def answer: Schema[(Long, Long)] = Schema.derived
  def flow(p: Scale, parts: Int): Flow[Run] =
    Flow.slices(Rows.of(p.n), parts).statefulIn[Run](mod, "open", "step", "finish")
  /** the sum of every running value, and the sum of the partitions' finals */
  def sink(p: Scale): Wire[Run, (Long, Long)] =
    Wire.fold(okay.Aggregator.sum[Long].contramap[Run](r => if r.key >= 0 then r.run else 0L))
      .and(Wire.fold(okay.Aggregator.sum[Long].contramap[Run](r => if r.key < 0 then r.run else 0L)))

object StatefulJobs:
  val scale = ScaleJob("test.held.jvm", JvmStateful.mod)
  val running = RunningJob("test.stream.jvm", JvmStateful.mod)
  Jobs.register(scale)
  Jobs.register(running)
  def install(): Unit = ()

  /** what a streaming stage must answer for `n` rows over `parts`
   * contiguous slices: each partition's running sums, and the totals */
  def expected(n: Int, parts: Int): (Long, Long) =
    val xs = Rows.of(n)
    var runs = 0L; var totals = 0L
    for i <- 0 until parts do
      val from = (n.toLong * i / parts).toInt; val until = (n.toLong * (i + 1) / parts).toInt
      var s = 0L
      for r <- xs.slice(from, until) do { s += r.v; runs += s }
      totals += s
    (runs, totals)

/** stage 4 in the JVM (default gate): a held model over the pool, a
 * stateful stage with one state per partition */
class TestStatefulModels extends munit.FunSuite:
  StatefulJobs.install()

  test("a held object: made once from its parameters, passed to every chunk's map — the fan's and the JVM's answer over three workers") {
    val expected = Rows.of(10000).map(_.v * 3).sum
    assertEquals(Flows.fan(StatefulJobs.scale.flow(Scale(10000), 4), StatefulJobs.scale.sink(Scale(10000))).runWith.value, expected)
    assertEquals(Cluster.run(StatefulJobs.scale, Scale(10000), 4, Vector.fill(3)(Cluster.local)).runWith.value, expected)
  }

  test("a stateful stage: the running sum is per PARTITION, opened once and finished once each, and the finals sum to the total") {
    JvmStateful.opened.set(0); JvmStateful.finished.set(0)
    val p = Scale(10000)
    val (runs, totals) = StatefulJobs.expected(10000, 4)
    assertEquals(Flows.fan(StatefulJobs.running.flow(p, 4), StatefulJobs.running.sink(p)).runWith.value, (runs, totals))
    assertEquals(JvmStateful.opened.get, 4)
    assertEquals(JvmStateful.finished.get, 4)
    assertEquals(Cluster.run(StatefulJobs.running, p, 4, Vector.fill(3)(Cluster.local)).runWith.value, (runs, totals))
    assertEquals(totals, Rows.of(10000).map(_.v).sum)
  }

  test("an empty partition still opens and finishes: the final row alone") {
    assertEquals(Flows.collect(Flow.slices(Vector.empty[Rec], 1).statefulIn[Run](JvmStateful.mod, "open", "step", "finish")).runWith,
      Vector(Run(-1, 0, 0)))
  }

  test("the extensions are separate instances: a module type with the base alone has neither a model nor a stateful stage, at compile time") {
    assert(compileErrors("""Model.in(FakeModule("t"), "fit", Factor(1))""").contains("Models"))
    assert(compileErrors("""Flow.slices(Rows.of(1), 1).statefulIn[Run](FakeModule("t"), "open", "step", "finish")""").contains("Stateful"))
  }

  test("a stateful stage or a model the JVM module lacks is refused when the flow is built, naming what it has") {
    val e = intercept[IllegalArgumentException](Flow.slices(Rows.of(1), 1).statefulIn[Run](JvmStateful.mod, "start", "step", "finish"))
    assert(e.getMessage.contains("'start'") && e.getMessage.contains("open/step/finish"), e.getMessage)
    val h = intercept[IllegalArgumentException](Model.in(JvmStateful.mod, "train", Factor(1)))
    assert(h.getMessage.contains("'train'") && h.getMessage.contains("model:fit"), h.getMessage)
  }
