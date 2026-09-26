package okay.foreign

/**
 * `okay.py` kept for a release (foreign-package-name): code written against
 * the old name compiles and behaves unchanged — its types ARE the new ones,
 * its objects the same objects, its patterns match the same values.
 */
class TestPackageAlias extends munit.FunSuite:
  test("okay.py's names are okay.foreign's: types, companions and patterns") {
    val v: okay.py.PyValue = PyValue.I64(1)
    assert(v match { case okay.py.PyValue.I64(n) => n == 1L; case _ => false })
    val op: ForeignEval[Unit] = okay.py.ForeignEval.Forget(1L)
    assertEquals(op, ForeignEval.Forget(1L))
    assert(okay.py.Py eq Py)
    assert(okay.py.Shape.python eq Shape.python)
    val c: okay.py.Condition = okay.py.Condition("Kind", "message")
    assertEquals(c, Condition("Kind", "message"))
  }

  test("Py is Foreign (foreign-api-name): the same object, and a type through one is the other's") {
    assert(Py eq Foreign)
    given Shape = Shape.python
    val cb: Foreign.Callback[okay.Reader[Long, *]] = Py.callback[Long, Long]("inc")(n => okay.Reader.ask[Long].map(_ + n))
    val asPy: Py.Callback[okay.Reader[Long, *]] = cb
    assertEquals(asPy.name, "inc")
  }
