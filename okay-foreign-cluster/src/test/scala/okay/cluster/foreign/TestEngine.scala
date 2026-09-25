package okay.cluster.foreign

import okay.given
import okay.cluster.{Cluster, Flow, Flows, Job, Jobs, Wire}
import okay.codec.Schema

/** the JVM module: Scala functions by name, the shape a Python or R
 * module has — one job text for every language */
object JvmStats:
  val mod: JvmModule = JvmModule("stats")
    .map[Rec, Out]("double")(rows => rows.map(r => Out(r.key, r.v * 2)))
    .reduce[Out, Stat]("step", "merge")(
      (acc, rows) => { val here = Stat(rows.length, rows.map(_.v).sum, rows.map(_.v).max); acc.fold(here)(Stat.merge(_, here)) },
      Stat.merge)
    .map[Rec, Out]("boom")(_ => throw IllegalArgumentException("no"))

/** ONE job, its language decided by the module it is handed and the
 * engine in scope: the same `flow` and `sink` text as the Python job's */
final class StatsJob[M](val name: String, mod: M)(using Engine[M], Reduces[M]) extends Job[Scale, Option[Stat]]:
  type A = Out
  def params: Schema[Scale] = summon[Schema[Scale]]
  def answer: Schema[Option[Stat]] = Schema.SOption(() => summon[Schema[Stat]])
  def flow(p: Scale, parts: Int): Flow[Out] = Flow.slices(Rows.of(p.n), parts).mapIn[Out](mod, "double")
  def sink(p: Scale): Wire[Out, Option[Stat]] = Reduce.in[Out, Stat](mod, "step", "merge")

/** a module type of a test's own, and its engine as a given: the
 * typeclass is open */
final case class FakeModule(tag: String)
object FakeEngine:
  /** the BASE only: this module type maps */
  given Engine[FakeModule] = new:
    def name = "fake"
    def batcher[A: Schema, B: Schema](module: FakeModule, fn: String, workers: Int): Batcher[A, B] =
      new Batcher[A, B]:
        val name = s"fake:${module.tag}:$fn"
        def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]] =
          Right(rows.map(_.asInstanceOf[Rec]).map(r => Out(r.key, r.v * 2).asInstanceOf[B]))
  /** the EXTENSION, its own instance: a module type that reduces */
  object reduces:
    given Reduces[FakeModule] = new:
      def reducer[A: Schema, Acc: Schema](module: FakeModule, step: String, merge: String, workers: Int): Reducer[A, Acc] =
        new Reducer[A, Acc]:
          val name = s"fake:${module.tag}:$step/$merge"
          def step(acc: Option[Acc], rows: Vector[A]): Either[Batcher.Failed, Acc] =
            val outs = rows.map(_.asInstanceOf[Out])
            val here = Stat(outs.length, outs.map(_.v).sum, outs.map(_.v).max)
            Right(acc.fold(here)(a => Stat.merge(a.asInstanceOf[Stat], here)).asInstanceOf[Acc])
          def merge(a: Acc, b: Acc): Either[Batcher.Failed, Acc] =
            Right(Stat.merge(a.asInstanceOf[Stat], b.asInstanceOf[Stat]).asInstanceOf[Acc])

object EngineJobs:
  val jvm = StatsJob("test.engine.jvm", JvmStats.mod)
  val fake =
    import FakeEngine.given
    import FakeEngine.reduces.given
    StatsJob("test.engine.fake", FakeModule("t"))
  Jobs.register(jvm)
  Jobs.register(fake)
  def install(): Unit = ()

/** stage 3: one API over the engines (default gate — the JVM and a fake
 * engine; the Python and R jobs of the same text are in TestPyEngine and
 * TestREngine, Live) */
class TestEngine extends munit.FunSuite:
  EngineJobs.install()

  private def run(job: Job[Scale, Option[Stat]], n: Int, parts: Int, workers: Int) =
    Cluster.run(job, Scale(n), parts, Vector.fill(workers)(Cluster.local)).runWith.value

  test("the JVM engine: map and reduce as Scala functions by name, the same job text as Python's, over three workers") {
    val expected = Stat.of(Rows.of(10000).map(r => Rec(r.key, r.v * 2)))
    assertEquals(run(EngineJobs.jvm, 10000, 4, 3), expected)
    assertEquals(Flows.fan(EngineJobs.jvm.flow(Scale(10000), 4), EngineJobs.jvm.sink(Scale(10000))).runWith.value, expected)
  }

  test("a module type and engine of a test's own: the typeclass is open, and the job text does not change") {
    assertEquals(run(EngineJobs.fake, 5000, 2, 2), Stat.of(Rows.of(5000).map(r => Rec(r.key, r.v * 2))))
  }

  test("the base and the extension are separate instances: with the base alone a module maps, and a reduce on it does not COMPILE") {
    import FakeEngine.given
    val mapped = Flow.slices(Rows.of(10), 1).mapIn[Out](FakeModule("t"), "double")
    assertEquals(Flows.collect(mapped).runWith.length, 10)
    val errors = compileErrors("""Reduce.in[Out, Stat](FakeModule("t"), "step", "merge")""")
    assert(errors.contains("Reduces"), errors)
  }

  test("a function the JVM module does not have is refused when the flow is built, naming what it has") {
    val e = intercept[IllegalArgumentException](Flow.slices(Rows.of(10), 1).mapIn[Out](JvmStats.mod, "triple"))
    assert(e.getMessage.contains("'triple'") && e.getMessage.contains("double"), e.getMessage)
  }

  test("a JVM function's exception is the function's failure: a considered refusal naming the module") {
    val job = StatsJob("test.engine.jvm.boom", JvmStats.mod)
    val boom = new Job[Scale, Option[Stat]]:
      type A = Out
      def name = "test.engine.jvm.boom"
      def params = job.params; def answer = job.answer
      def flow(p: Scale, parts: Int): Flow[Out] = Flow.slices(Rows.of(p.n), parts).mapIn[Out](JvmStats.mod, "boom")
      def sink(p: Scale) = job.sink(p)
    val e = intercept[Throwable](Flows.fan(boom.flow(Scale(100), 1), boom.sink(Scale(100))).runWith)
    assert(e.getMessage.contains("jvm:stats:boom") && e.getMessage.contains("IllegalArgumentException: no"), e.getMessage)
  }
