package okay.foreign

import Value.*

/** pyvalue-table over a REAL python3: `okay.frame(...)` inside a dict a
 * function answers reaches the host as a Table, and a Table among a call's
 * arguments reaches the function as a dict of columns (Live) */
class TestFrameInValue extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  private val mod = Foreign.module("framed", """
    import okay

    def stepper(frame, state):
        n = state["n"] + len(frame["v"])
        return {"rows": okay.frame({"v": [x * 2 for x in frame["v"]]}), "state": {"n": n}}

    def untagged(frame):
        return {"rows": {"v": [1, 2]}}
  """)

  test("a function answers {rows: okay.frame(...), state: ...}: a Table and a dict, and takes a Table as an argument") {
    val w = ForeignWorker.start(TestPy.python.get, modules = Seq(mod))
    try
      val frame = Frame(Vector("v" -> Vector(I64(1), I64(2), I64(3))))
      val got = w.handler.handle(ForeignEval.Call("framed:stepper", Vector(Table(frame), Dict(Vector("n" -> I64(4))))))
      assertEquals(got, Right(Dict(Vector(
        "rows" -> Table(Frame(Vector("v" -> Vector(I64(2), I64(4), I64(6))))),
        "state" -> Dict(Vector("n" -> I64(7)))))))
      // a plain dict of lists is a dict of lists, as it always was
      assertEquals(w.handler.handle(ForeignEval.Call("framed:untagged", Vector(Table(frame)))),
        Right(Dict(Vector("rows" -> Dict(Vector("v" -> Arr(Vector(I64(1), I64(2)))))))))
    finally w.close()
  }
