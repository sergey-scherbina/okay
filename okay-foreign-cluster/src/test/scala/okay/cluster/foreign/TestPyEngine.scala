package okay.cluster.foreign

import okay.given
import okay.cluster.{Cluster, Jobs}
import okay.py.{Foreign, TestPy}

/** the map AND the reduce of one Python module, for the one job text */
object PyStats:
  val mod = Foreign.module("pystats", """
    def double(frame):
        return {"key": frame["key"], "v": [x * 2 for x in frame["v"]]}

    def step(frame, acc):
        vs = frame["v"]
        n = len(vs) + (acc["n"] if acc else 0)
        s = sum(vs) + (acc["sum"] if acc else 0)
        m = max(vs + ([acc["max"]] if acc else []))
        return {"n": [n], "sum": [s], "max": [m]}

    def merge(a, b):
        return {"n": a["n"] + b["n"], "sum": a["sum"] + b["sum"], "max": max(a["max"], b["max"])}
  """)

object PyEngineJobs:
  /** the interpreter is a given, not an edit of the job — the base and
   * the extension both */
  given Engine[okay.py.PyModule] = Engine.py(TestPy.python.getOrElse("python3"))
  given Reduces[okay.py.PyModule] = Reduces.py(TestPy.python.getOrElse("python3"))
  val job = StatsJob("test.engine.py", PyStats.mod)
  Jobs.register(job)
  def install(): Unit = ()

/** stage 3 over a REAL python3: the same `StatsJob` text as the JVM's,
 * handed a `PyModule` (Live) */
class TestPyEngine extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")
  PyEngineJobs.install()

  test("one job text, the module Python's: map and reduce in python3 over three workers, the JVM's answer") {
    val expected = Stat.of(Rows.of(10000).map(r => Rec(r.key, r.v * 2)))
    assertEquals(Cluster.run(PyEngineJobs.job, Scale(10000), 4, Vector.fill(3)(Cluster.local)).runWith.value, expected)
  }
