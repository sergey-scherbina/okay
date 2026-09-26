package okay.cluster.foreign

import okay.given
import okay.cluster.{Cluster, Jobs}
import okay.foreign.{Foreign, TestPy}

/** the model and the stateful stage, in Python: `fit` makes the held
 * object, `scale(frame, model)` uses it; `open` makes the state, `step`
 * mutates it, `finish` flushes */
object PyStateful:
  val mod = Foreign.module("pystateful", """
    def fit(params):
        return {"by": params["by"]}

    def scale(frame, model):
        return {"key": frame["key"], "v": [x * model["by"] for x in frame["v"]]}

    def open():
        return {"sum": 0}

    def step(frame, state):
        runs = []
        for x in frame["v"]:
            state["sum"] += x
            runs.append(state["sum"])
        return {"key": frame["key"], "v": frame["v"], "run": runs}

    def finish(frame, state):
        return {"key": [-1], "v": [0], "run": [state["sum"]]}
  """)

object PyStatefulJobs:
  private val python = TestPy.python.getOrElse("python3")
  given Models[okay.foreign.PyModule] = Models.py(python)
  given Stateful[okay.foreign.PyModule] = Stateful.py(python)
  val scale = ScaleJob("test.held.py", PyStateful.mod)
  val running = RunningJob("test.stream.py", PyStateful.mod)
  Jobs.register(scale)
  Jobs.register(running)
  def install(): Unit = ()

/** stage 4 over a REAL python3: the same `ScaleJob` and `RunningJob`
 * text as the JVM's, handed a `PyModule` (Live) */
class TestPyStatefulModels extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")
  PyStatefulJobs.install()

  test("a model fit once in Python, used by every chunk over three workers") {
    assertEquals(Cluster.run(PyStatefulJobs.scale, Scale(10000), 4, Vector.fill(3)(Cluster.local)).runWith.value,
      Rows.of(10000).map(_.v * 3).sum)
  }

  test("a stateful stage in Python: one state per partition, the running sums and the finals exact") {
    assertEquals(Cluster.run(PyStatefulJobs.running, Scale(10000), 4, Vector.fill(3)(Cluster.local)).runWith.value,
      StatefulJobs.expected(10000, 4))
  }
