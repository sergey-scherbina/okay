package okay.cluster.foreign

import okay.py.{Foreign, TestPy}

object PyFacadeMod:
  val mod = Foreign.module("pyfacade", """
    def echo(rec):
        return rec

    def boom(rec):
        raise ValueError("nope")

    def fecho(frame):
        return frame

    def fboom(frame):
        raise ValueError("nope")

    class Counter:
        def __init__(self, n):
            self.n = n
        def add(self, k):
            return self.n + k

    def make(n):
        return Counter(n)

    def describe(c, k):
        return c.n + k

    import okay

    def priced(order):
        return okay.perform("price_of", order["sku"]).then(lambda p: okay.done(p * order["qty"]))

    def pairs(_):
        return okay.perform("choose", [1, 2]).then(lambda x:
               okay.perform("choose", [10, 20]).then(lambda y:
               okay.done(x + y)))
  """)

/** the conformance body over a REAL Python (Live) */
class TestPyFacade extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  given Calls[okay.py.PyModule] = Calls.py(TestPy.python.getOrElse("python3"))
  given Speaks[okay.py.PyModule] = Speaks.py(TestPy.python.getOrElse("python3"))
  given Frames[okay.py.PyModule] = Frames.py(TestPy.python.getOrElse("python3"))
  given Programs[okay.py.PyModule] = Programs.py(TestPy.python.getOrElse("python3"))
  given holds: Holds.Py = Holds.py(TestPy.python.getOrElse("python3"))
  given methods: Methods.Py = Methods.py(TestPy.python.getOrElse("python3"))

  test("Holds over python3: two objects held, each described with its own state, released") {
    FacadeConformance.holds(PyFacadeMod.mod, "make", "describe")
  }

  test("Methods over python3: a held Counter's add and n") {
    FacadeConformance.methods(PyFacadeMod.mod, "make", "add", "n")
  }

  test("Programs over python3: a callback under a Reader, and a continuation resumed twice") {
    FacadeConformance.programs(PyFacadeMod.mod, "priced", "pairs")
  }

  test("Streams over python3: 20 000 rows through fecho in frames of 4 096, every row back in order") {
    FacadeConformance.streams(PyFacadeMod.mod, "fecho", 20000, 4096)
  }

  test("Frames over python3: a table there and back, the empty one too, boom refused") {
    FacadeConformance.frames(PyFacadeMod.mod, "fecho", "fboom")
  }

  test("Calls over python3: echo, boom, a missing function") {
    FacadeConformance.calls(PyFacadeMod.mod, "echo", "boom")
  }

  test("Speaks over python3: pipes, frames as this interpreter crosses them, multi-shot") {
    val r = FacadeConformance.speaks(PyFacadeMod.mod, "python")
    assertEquals((r.link, r.programs), ("pipes", "multi-shot"))
  }
