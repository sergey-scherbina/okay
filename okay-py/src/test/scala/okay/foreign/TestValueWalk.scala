package okay.foreign

import Value.*

/** stack-safety-py-r: the walks the workers share (`refs`, `rebuild`)
 * take a value nested as deep as a program built it */
class TestValueWalk extends munit.FunSuite:

  private val n = 200000
  private def deep(leaf: Value): Value =
    var v = leaf
    for i <- 1 to n do v = if i % 2 == 0 then Arr(Vector(v, I64(i))) else Dict(Vector("k" -> v, "i" -> I64(i)))
    v

  test("refs finds every handle, in order, 200 000 deep") {
    val v = Arr(Vector(deep(Ref(Handle(7L, "obj"))), Ref(Handle(9L, "obj"))))
    assertEquals(Value.refs(v), Vector(7L, 9L))
  }

  test("rebuild renames the leaves and keeps the shape, 200 000 deep") {
    val v = deep(Ref(Handle(7L, "obj")))
    val w = Value.rebuild(v) { case Ref(r) => Ref(r.copy(id = r.id + 1)); case other => other }
    assertEquals(Value.refs(w), Vector(8L))
    // the shape compared by walking, since == recurses on a deep tree
    var a = v; var b = w; var d = 0
    while d < n do
      (a, b) match
        case (Arr(Vector(x, i)), Arr(Vector(y, j))) => assertEquals(i, j); a = x; b = y
        case (Dict(Vector(("k", x), ("i", i))), Dict(Vector(("k", y), ("i", j)))) => assertEquals(i, j); a = x; b = y
        case other => fail(s"shapes part at depth $d: ${other.toString.take(80)}")
      d += 1
  }

  test("rebuildE stops at the first refusal") {
    val v = deep(Ref(Handle(7L, "obj")))
    assertEquals(Value.rebuildE[String](v) { case Ref(r) => Left(s"gone: ${r.id}"); case other => Right(other) }, Left("gone: 7"))
    assertEquals(Value.rebuildE[String](Arr(Vector(I64(1), Str("x"))))(Right(_)), Right(Arr(Vector(I64(1), Str("x")))))
  }
