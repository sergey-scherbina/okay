package okay.cluster.foreign

import okay.py.{Foreign, TestPy}

object PyFacadeMod:
  val mod = Foreign.module("pyfacade", """
    def echo(rec):
        return rec

    def boom(rec):
        raise ValueError("nope")
  """)

/** the conformance body over a REAL Python (Live) */
class TestPyFacade extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  given Calls[okay.py.PyModule] = Calls.py(TestPy.python.getOrElse("python3"))
  given Speaks[okay.py.PyModule] = Speaks.py(TestPy.python.getOrElse("python3"))

  test("Calls over python3: echo, boom, a missing function") {
    FacadeConformance.calls(PyFacadeMod.mod, "echo", "boom")
  }

  test("Speaks over python3: pipes, frames as this interpreter crosses them, multi-shot") {
    val r = FacadeConformance.speaks(PyFacadeMod.mod, "python")
    assertEquals((r.link, r.programs), ("pipes", "multi-shot"))
  }
