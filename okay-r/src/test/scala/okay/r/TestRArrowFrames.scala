package okay.r

import RValue.*

/** `RArrowFrames.table`/`.frame` without a live R (default gate): the
 * column mapping okay-py's `ArrowFrames` twin. */
class TestRArrowFrames extends munit.FunSuite:

  private def col(t: okay.arrow.Table, name: String): okay.arrow.Column =
    t.cols.toMap.getOrElse(name, fail(s"no column '$name' in $t"))

  test("every R type round-trips, NA in place, text beyond ASCII") {
    val f = RFrame(Vector(
      "l" -> Vector(Bool(true), NA(RType.Logical), Bool(false)),
      "i" -> Vector(I32(1), I32(-2), NA(RType.Integer)),
      "d" -> Vector(F64(0.5), NA(RType.Double), F64(1e300)),
      "s" -> Vector(Str("kyiv"), Str("чай ☕"), NA(RType.Character))))
    val t = RArrowFrames.table(f).fold(why => fail(why), identity)
    assertEquals(RArrowFrames.frame(t), f)
  }

  test("a column of NA alone keeps its type") {
    val f = RFrame(Vector("d" -> Vector(NA(RType.Double), NA(RType.Double))))
    val t = RArrowFrames.table(f).fold(why => fail(why), identity)
    assert(col(t, "d").isInstanceOf[okay.arrow.Column.Float64], col(t, "d").toString)
    assertEquals(RArrowFrames.frame(t), f)
  }

  test("an empty column is a column of nulls, and an empty frame round-trips") {
    val f = RFrame(Vector("x" -> Vector.empty))
    val t = RArrowFrames.table(f).fold(why => fail(why), identity)
    assert(col(t, "x").isInstanceOf[okay.arrow.Column.Nulls], col(t, "x").toString)
    assertEquals(RArrowFrames.frame(t), f)
  }

  test("a column mixing kinds is refused by name, naming the column and the mismatch") {
    val f = RFrame(Vector("n" -> Vector(I32(1), F64(2.5))))
    val why = RArrowFrames.table(f).left.getOrElse(fail("expected a Left"))
    assert(why.contains("column 'n'"), why)
    assert(why.contains("mixes kinds"), why)
  }

  test("a column the model cannot carry (raw, a nested vector, a held object) is refused by name") {
    for (v, what) <- Seq(Bytes(Array[Byte](1, 2)) -> "raw", Vec(Vector(I32(1))) -> "a nested vector") do
      val f = RFrame(Vector("x" -> Vector(v)))
      val why = RArrowFrames.table(f).left.getOrElse(fail(s"expected a Left for $what"))
      assert(why.contains("column 'x'") && why.contains(what), why)
  }

  test("ragged columns are refused, naming the short one and the frame's length") {
    val f = RFrame(Vector("a" -> Vector(F64(1), F64(2)), "b" -> Vector(F64(1))))
    val why = RArrowFrames.table(f).left.getOrElse(fail("expected a Left"))
    assert(why.contains("column 'b'") && why.contains("has 1 cells") && why.contains("the first has 2"), why)
  }

  test("no columns at all is the empty table") {
    val t = RArrowFrames.table(RFrame(Vector.empty)).fold(why => fail(why), identity)
    assertEquals(t.cols, Vector.empty)
  }
