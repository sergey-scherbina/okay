package okay.py

import okay.{!, Reader, State}
import okay.given
import okay.agent.Durable

object TestPyCallbacks:
  val module: String =
    """import okay
      |
      |def minimise(x0):
      |    # a gradient-free search: every value of the objective is asked of okay
      |    best, fb, step = x0, okay.call("objective", x0), 4.0
      |    for _ in range(60):
      |        for cand in (best - step, best + step):
      |            fc = okay.call("objective", cand)
      |            if fc < fb:
      |                best, fb = cand, fc
      |        step /= 2
      |    return best
      |
      |def twice(x):
      |    return okay.call("inc", x) + okay.call("inc", x)
      |
      |def nested(x):
      |    return okay.call("via_python", x) + 1
      |
      |def square(x):
      |    return x * x
      |
      |def catches():
      |    try:
      |        okay.call("inc", "not a number")
      |    except okay.OkayError as e:
      |        return e.kind
      |
      |def unoffered():
      |    return okay.call("nope", 1)
      |""".stripMargin

/** foreign-callbacks against a LIVE python3 (specs/foreign-highlevel.md stage 7) */
class TestPyCallbacks extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  private lazy val w =
    val dir = java.nio.file.Files.createTempDirectory("okay-py-cb")
    java.nio.file.Files.writeString(dir.resolve("okaycb.py"), TestPyCallbacks.module): Unit
    PySubprocess.start(TestPy.python.get, Map("PYTHONPATH" -> dir.toString))
  private given okay.Handler[PyEval] = w.handler
  override def afterAll(): Unit = if TestPy.python.nonEmpty then w.close()

  private val inc = Py.callback[Long, Long]("inc")(x => State.modify[Int](_ + 1).map(_ => x + 1))

  test("a Python optimiser minimises an objective whose target comes from okay's Reader") {
    val objective = Py.callback[Double, Double]("objective")(x => Reader.ask[Double].map(t => (x - t) * (x - t)))
    val fit = Py.fn[Double]("okaycb:minimise").calling(Py.callbacks(objective))(0.0)
    val best = Reader.run(3.25)(fit).runWith
    assert(best.exists(b => math.abs(b - 3.25) < 1e-6), s"$best")
  }

  test("a callback's State is the caller's: two calls from Python, counted in okay") {
    val prog = Py.fn[Long]("okaycb:twice").calling(Py.callbacks(inc))(5L)
    assertEquals(State.handle(0)(prog).runWith, (2, Right(12L)))
  }

  test("a callback that calls Python again, on the same worker, is answered") {
    val via = Py.callback[Long, Long]("via_python")(x => Py.fn[Long]("okaycb:square")(x).map(_.getOrElse(-1L)))
    val prog: Either[Condition, Long] ! PyEval = Py.fn[Long]("okaycb:nested").calling(Py.callbacks(via))(7L)
    assertEquals(prog.runWith, Right(50L))
  }

  test("a callback that fails in okay raises okay.OkayError in Python, which Python may catch") {
    assertEquals(State.handle(0)(Py.fn[String]("okaycb:catches").calling(Py.callbacks(inc))()).runWith,
      (0, Right("Decode")))
  }

  test("a callback this call did not offer is refused by name, in Python") {
    val got = State.handle(0)(Py.fn[Long]("okaycb:unoffered").calling(Py.callbacks(inc))()).runWith._2
    assertEquals(got.left.map(_.kind), Left("LookupError"))
    assert(got.left.exists(_.message.contains("'nope'")), s"$got")
  }

  test("through a worker POOL: a dialogue keeps its worker, and a callback's own call takes another") {
    val dir = java.nio.file.Files.createTempDirectory("okay-py-cb-pool")
    java.nio.file.Files.writeString(dir.resolve("okaycb.py"), TestPyCallbacks.module): Unit
    val pool = PyWorkers.start(2, TestPy.python.get, Map("PYTHONPATH" -> dir.toString))
    try
      val via = Py.callback[Long, Long]("via_python")(x => Py.fn[Long]("okaycb:square")(x).map(_.getOrElse(-1L)))
      val nested: Either[Condition, Long] ! PyEval = Py.fn[Long]("okaycb:nested").calling(Py.callbacks(via))(7L)
      assertEquals(nested.runWith(using pool.handler), Right(50L))
      val counted = State.handle(0)(Py.fn[Long]("okaycb:twice").calling(Py.callbacks(inc))(5L))
      assertEquals(counted.runWith(using pool.handler), (2, Right(12L)))
    finally pool.close()
  }

  test("Durable journals the dialogue; a replay answers every step without Python") {
    val j = Durable.MemoryJournal()
    val prog = Py.fn[Long]("okaycb:twice").calling(Py.callbacks(inc))(5L)
    val live = State.handle(0)(prog).runWith(using Durable.over[PyEval](w.handler, j)())
    assertEquals(live, (2, Right(12L)))
    assertEquals(j.all.map(_.op), Vector("okaycb:twice", "resume", "resume"))
    // the callbacks run again (their State is okay's); Python does not
    val replayed = State.handle(0)(prog).runWith(using Durable.replayingOver[PyEval](j))
    assertEquals(replayed, (2, Right(12L)))
  }
}
