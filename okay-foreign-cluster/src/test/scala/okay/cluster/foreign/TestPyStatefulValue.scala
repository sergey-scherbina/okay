package okay.cluster.foreign
import okay.freer.given
import okay.given

import okay.cluster.{Cluster, Jobs}
import okay.foreign.{Foreign, TestPy}

/** stage 5 in Python: `vopen(params)` answers the state, `vstep(frame,
 * state)` answers `{"rows": okay.frame(...), "state": ...}` — a frame AS
 * A VALUE (pyvalue-table) — and `vfinish(state)` the last rows; nothing
 * is held in the interpreter, any worker takes any step */
object PyValued:
  val mod = Foreign.module("pyvalued", """
    import okay

    def vopen(params):
        return params["by"]

    def vstep(frame, state):
        runs = []
        for x in frame["v"]:
            state += x
            runs.append(state)
        return {"rows": okay.frame({"key": frame["key"], "v": frame["v"], "run": runs}), "state": state}

    def vfinish(state):
        return okay.frame({"key": [-1], "v": [0], "run": [state]})
  """)

object PyValuedJobs:
  private val python = TestPy.python.getOrElse("python3")
  given StatefulValue[okay.foreign.PyModule] = StatefulValue.py(python)
  val running = ValuedRunningJob("test.valued.py", PyValued.mod)
  Jobs.register(running)
  def install(): Unit = ()

/** the same `ValuedRunningJob` text as the JVM's, handed a `PyModule` (Live) */
class TestPyStatefulValue extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")
  PyValuedJobs.install()

  test("a functional stateful stage in Python: the state a value carried by the JVM, the running sums and the finals exact over three workers") {
    assertEquals(Cluster.run(PyValuedJobs.running, Scale(10000), 4, Vector.fill(3)(Cluster.local)).runWith.value,
      StatefulJobs.expected(10000, 4))
  }
