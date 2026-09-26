package okay.cluster.foreign

import okay.given
import okay.cluster.{Cluster, Flow, Flows, Job, Jobs, Wire}
import okay.codec.Schema
import okay.foreign.{Foreign, TestPy}

/** the reduce, in Python: a step over a chunk, a merge of two partials */
object Stats:
  val mod = Foreign.module("stats", """
    def step(frame, acc):
        vs = frame["v"]
        n = len(vs) + (acc["n"] if acc else 0)
        s = sum(vs) + (acc["sum"] if acc else 0)
        m = max(vs + ([acc["max"]] if acc else []))
        return {"n": [n], "sum": [s], "max": [m]}

    def merge(a, b):
        return {"n": a["n"] + b["n"], "sum": a["sum"] + b["sum"], "max": max(a["max"], b["max"])}

    def boom(frame, acc):
        raise ValueError("no")
  """)

object PyReduceJobs:
  def python: String = TestPy.python.getOrElse("python3")

  object Statting extends Job[Scale, Option[Stat]]:
    type A = Rec
    def name: String = "test.foreign.py.reduce"
    def params: Schema[Scale] = summon[Schema[Scale]]
    def answer: Schema[Option[Stat]] = Schema.SOption(() => summon[Schema[Stat]])
    def flow(p: Scale, parts: Int): Flow[Rec] = Flow.slices(Rows.of(p.n), parts)
    def sink(p: Scale): Wire[Rec, Option[Stat]] = Reduce.py[Rec, Stat](Stats.mod, "step", "merge", python)

  object Booming extends Job[Scale, Option[Stat]]:
    type A = Rec
    def name: String = "test.foreign.py.reduce.boom"
    def params: Schema[Scale] = summon[Schema[Scale]]
    def answer: Schema[Option[Stat]] = Schema.SOption(() => summon[Schema[Stat]])
    def flow(p: Scale, parts: Int): Flow[Rec] = Flow.slices(Rows.of(p.n), parts)
    def sink(p: Scale): Wire[Rec, Option[Stat]] = Reduce.py[Rec, Stat](Stats.mod, "boom", "merge", python)

  Jobs.register(Statting)
  Jobs.register(Booming)
  def install(): Unit = ()

/** foreign-reduce over a REAL python3: step and merge in Python, the
 * answer the JVM's (Live) */
class TestPyReduce extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")
  PyReduceJobs.install()

  test("Cluster.run over three in-process workers computes count, sum and max in Python, the JVM's answer to the row") {
    val p = Scale(20000)
    val expected = Stat.of(Rows.of(20000))
    val here = Flows.fan(PyReduceJobs.Statting.flow(p, 4), PyReduceJobs.Statting.sink(p)).runWith
    val there = Cluster.run(PyReduceJobs.Statting, p, 4, Vector.fill(3)(Cluster.local)).runWith
    assertEquals(here.value, expected)
    assertEquals(there.value, expected)
    assertEquals(there.retried, 0L)
  }

  test("a Python exception in `step` fails the run by name") {
    val e = intercept[Throwable](Cluster.run(PyReduceJobs.Booming, Scale(1000), 2, Vector.fill(2)(Cluster.local)).runWith)
    assert(e.getMessage.contains("py:stats:boom/merge") && e.getMessage.contains("no"), e.getMessage)
  }
