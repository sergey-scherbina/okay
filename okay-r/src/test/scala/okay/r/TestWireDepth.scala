package okay.r

/**
 * `Wire.enc`/`Wire.dec` recursed natively on `RValue.Vec`/`Json.JArr`
 * depth — the same defect shape `okay-mcp/Rpc.damaged` and
 * `okay-demo/StateMcp.damaged` already had fixed twice
 * (subprocess-wire-depth-safety). No live R needed: these are pure
 * functions over `RValue`/`Json`, built directly (a loop, not a
 * subprocess reply), so this proves the wire boundary alone.
 */
class TestWireDepth extends munit.FunSuite:

  override val munitTimeout = scala.concurrent.duration.Duration(60, "s")

  def deepVec(n: Int): RValue =
    var v: RValue = RValue.Vec(Vector.empty)
    var i = 0
    while i < n do { v = RValue.Vec(Vector(v)); i += 1 }
    v

  def depthOf(v: RValue): Int =
    var d = 0
    var at = v
    var go = true
    while go do at match
      case RValue.Vec(Vector(one)) => d += 1; at = one
      case RValue.Vec(vs) if vs.isEmpty => go = false
      case _ => go = false
    d

  test("Wire.enc/dec round-trip a genuinely deep RValue — no cap, no stack overflow") {
    val n = 100000
    val v = deepVec(n)
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
    val v = RValue.Vec(Vector(RValue.I32(1), RValue.Str("x"), RValue.RNull,
      RValue.NA(RType.Integer)))
    assertEquals(Wire.dec(Wire.enc(v)), v)

    Wire.dec(Wire.enc(RValue.F64(Double.NaN))) match
      case RValue.F64(d) => assert(d.isNaN)
      case other => fail(s"expected F64(NaN), got $other")

    val bytes = RValue.Bytes(Array[Byte](1, 2, 3))
    Wire.dec(Wire.enc(bytes)) match
      case RValue.Bytes(back) => assert(back.sameElements(Array[Byte](1, 2, 3)))
      case other => fail(s"expected Bytes, got $other")
  }
