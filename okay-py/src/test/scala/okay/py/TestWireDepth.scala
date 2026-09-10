package okay.py

/**
 * `Wire.enc`/`Wire.dec` recursed natively on `PyValue.Arr`/`Json.JArr`
 * depth — the same defect shape `okay-mcp/Rpc.damaged` and
 * `okay-demo/StateMcp.damaged` already had fixed twice
 * (subprocess-wire-depth-safety). No live Python needed: these are
 * pure functions over `PyValue`/`Json`, built directly (a loop, not a
 * subprocess reply), so this proves the wire boundary alone.
 */
class TestWireDepth extends munit.FunSuite:

  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  def deepArr(n: Int): PyValue =
    var v: PyValue = PyValue.Arr(Vector.empty)
    var i = 0
    while i < n do { v = PyValue.Arr(Vector(v)); i += 1 }
    v

  def depthOf(v: PyValue): Int =
    var d = 0
    var at = v
    var go = true
    while go do at match
      case PyValue.Arr(Vector(one)) => d += 1; at = one
      case PyValue.Arr(vs) if vs.isEmpty => go = false
      case _ => go = false
    d

  test("Wire.enc/dec round-trip a genuinely deep PyValue — no cap, no stack overflow") {
    val n = 100000
    val v = deepArr(n)
    val j = Wire.enc(v)
    assertEquals(depthOf(Wire.dec(j)), n)
  }

  test("ordinary shapes below the threshold are untouched") {
    // Bytes(Array[Byte]) is excluded from this equals check on purpose
    // (Scala's derived equals compares arrays by REFERENCE, not
    // content), and NaN != NaN under Double's own `==` (the case
    // class equals compares the unboxed primitive, not
    // java.lang.Double.equals) — both unrelated pitfalls, not the one
    // under test here
    val v = PyValue.Arr(Vector(PyValue.I64(1), PyValue.Str("x"), PyValue.PyNone))
    assertEquals(Wire.dec(Wire.enc(v)), v)

    Wire.dec(Wire.enc(PyValue.F64(Double.NaN))) match
      case PyValue.F64(d) => assert(d.isNaN)
      case other => fail(s"expected F64(NaN), got $other")

    val bytes = PyValue.Bytes(Array[Byte](1, 2, 3))
    Wire.dec(Wire.enc(bytes)) match
      case PyValue.Bytes(back) => assert(back.sameElements(Array[Byte](1, 2, 3)))
      case other => fail(s"expected Bytes, got $other")
  }
