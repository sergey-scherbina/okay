package okay.cluster.foreign
import okay.freer.given
import okay.std.given
import okay.given

import okay.cluster.{Cluster, Flow, Flows, Job, Jobs, Wire}
import okay.codec.Schema

/** stage 5: the FUNCTIONAL stateful stage on the JVM — `open` makes the
 * state from its parameters, `step` answers rows AND the next state,
 * `finish` the last rows; the state a value, never mutated */
object JvmValued:
  val opened = java.util.concurrent.atomic.AtomicInteger(0)
  val finished = java.util.concurrent.atomic.AtomicInteger(0)
  val mod: JvmModule = JvmModule("valued")
    .streamValue[Rec, Long, Run, Factor]("vopen", "vstep", "vfinish")(
      p => { opened.incrementAndGet(): Unit; p.by },
      (s, rows) => {
        var run = s
        val out = rows.map { r => run += r.v; Run(r.key, r.v, run) }
        (out, run)
      },
      s => { finished.incrementAndGet(): Unit; Vector(Run(-1, 0, s)) })

/** the running sum by VALUE: one job text over any module type with the
 * `StatefulValue` extension, the state a `Long` the JVM carries between steps */
final class ValuedRunningJob[M](val name: String, mod: M, open: String = "vopen", step: String = "vstep", finish: String = "vfinish")
                               (using StatefulValue[M]) extends Job[Scale, (Long, Long)]:
  type A = Run
  def params: Schema[Scale] = summon[Schema[Scale]]
  def answer: Schema[(Long, Long)] = Schema.derived
  def flow(p: Scale, parts: Int): Flow[Run] =
    Flow.slices(Rows.of(p.n), parts).statefulValueIn[Run, Long, Factor](mod, open, step, finish, Factor(0))
  def sink(p: Scale): Wire[Run, (Long, Long)] =
    Wire.fold(okay.freer.Aggregator.sum[Long].contramap[Run](r => if r.key >= 0 then r.run else 0L))
      .and(Wire.fold(okay.freer.Aggregator.sum[Long].contramap[Run](r => if r.key < 0 then r.run else 0L)))

object ValuedJobs:
  val running = ValuedRunningJob("test.valued.jvm", JvmValued.mod)
  Jobs.register(running)
  def install(): Unit = ()

class TestStatefulValue extends munit.FunSuite:
  ValuedJobs.install()

  test("a functional stateful stage: the running sum per PARTITION, the state a value answered by each step, opened and finished once each") {
    JvmValued.opened.set(0); JvmValued.finished.set(0)
    val p = Scale(10000)
    val (runs, totals) = StatefulJobs.expected(10000, 4)
    assertEquals(Flows.fan(ValuedJobs.running.flow(p, 4), ValuedJobs.running.sink(p)).runWith.value, (runs, totals))
    assertEquals(JvmValued.opened.get, 4)
    assertEquals(JvmValued.finished.get, 4)
    assertEquals(Cluster.run(ValuedJobs.running, p, 4, Vector.fill(3)(Cluster.local)).runWith.value, (runs, totals))
  }

  test("the state starts from the parameters: `open(Factor(5))` is 5, so every running sum is 5 higher") {
    val got = Flows.collect(Flow.slices(Rows.of(3), 1).statefulValueIn[Run, Long, Factor](JvmValued.mod, "vopen", "vstep", "vfinish", Factor(5))).runWith
    val xs = Rows.of(3)
    assertEquals(got.map(_.run), Vector(5 + xs(0).v, 5 + xs(0).v + xs(1).v, 5 + xs.map(_.v).sum, 5 + xs.map(_.v).sum))
  }

  test("an empty partition still opens and finishes: the final row alone") {
    assertEquals(Flows.collect(Flow.slices(Vector.empty[Rec], 1).statefulValueIn[Run, Long, Factor](JvmValued.mod, "vopen", "vstep", "vfinish", Factor(0))).runWith,
      Vector(Run(-1, 0, 0)))
  }

  test("its own typeclass: a module type without it is refused at compile time, naming StatefulValue; a JVM module lacking the names is refused when the flow is built") {
    assert(compileErrors("""Flow.slices(Rows.of(1), 1).statefulValueIn[Run, Long, Factor](FakeModule("t"), "vopen", "vstep", "vfinish", Factor(0))""").contains("StatefulValue"))
    val e = intercept[IllegalArgumentException](Flow.slices(Rows.of(1), 1).statefulValueIn[Run, Long, Factor](JvmValued.mod, "start", "vstep", "vfinish", Factor(0)))
    assert(e.getMessage.contains("'start'") && e.getMessage.contains("vopen/vstep/vfinish"), e.getMessage)
  }
