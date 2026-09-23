package okay.py

import okay.{Reader, given}

object TestPyModule:
  val scoring = Py.module("scoring", """
    def mean(xs):
        return sum(xs) / len(xs)

    class Model:
        def __init__(self, bias):
            self.bias = bias
        def predict(self, x):
            return x + self.bias

    def fit(xs):
        import okay
        return sum(okay.call("weight", x) for x in xs)
  """)

/** foreign-inline-modules against a LIVE python3 (specs/foreign-highlevel.md stage 4) */
class TestPyModule extends munit.FunSuite {
  import TestPyModule.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  private lazy val w = PySubprocess.start(TestPy.python.get, modules = Seq(scoring))
  private given okay.Handler[PyEval] = w.handler
  override def afterAll(): Unit = if TestPy.python.nonEmpty then w.close()

  test("a module written beside the Scala: its function, its class held, a callback from it") {
    assertEquals(scoring.fn[Double]("mean")(Vector(1.0, 2.0, 6.0)).runWith, Right(3.0))
    val model = scoring.hold("Model")(10L).runWith.toOption.get
    assertEquals(model.call[Long]("predict")(5L).runWith, Right(15L))
    val weight = Py.callback[Long, Long]("weight")(x => Reader.ask[Long].map(_ * x))
    assertEquals(Reader.run(2L)(scoring.fn[Long]("fit").calling(Py.callbacks(weight))(Vector(1L, 2L, 3L))).runWith,
      Right(12L))
  }

  test("a pool ships the module to every worker") {
    val pool = PyWorkers.start(2, TestPy.python.get, modules = Seq(scoring))
    try
      given okay.Handler[PyEval] = pool.handler
      for _ <- 1 to 4 do
        assertEquals(scoring.fn[Double]("mean")(Vector(2.0, 4.0)).runWith, Right(3.0))
    finally pool.close()
  }
}

/** the literal-only rule, checked by the compiler (default gate) */
class TestPyModuleRule extends munit.FunSuite {

  test("a module's source must be a compile-time constant") {
    val refused = compileErrors("""
      val body = "def f(): return 1"
      Py.module("m", body)
    """)
    assert(refused.contains("expected a constant value"), refused)
    val interpolated = compileErrors("""
      val n = 1
      Py.module("m", s"def f(): return $n")
    """)
    assert(interpolated.contains("expected a constant value"), interpolated)
  }

  test("an indented literal is dedented, and a bad name is refused where the module is made") {
    val m = Py.module("ok_name", """
      def f():
          return 1
    """)
    assertEquals(m.source, "def f():\n    return 1\n")
    val bad = intercept[IllegalArgumentException](Py.module("not-a-name", "x = 1"))
    assert(bad.getMessage.contains("identifier"), bad.getMessage)
  }
}
