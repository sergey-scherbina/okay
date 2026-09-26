package okay.r

import okay.arrow.{Column, Table}

/** okay-arrow stage 8: an R frame reads a dictionary-encoded column as its values */
class TestRArrowFramesDictionary extends munit.FunSuite:
  test("a Dictionary column becomes the frame's strings, a typed NA in place") {
    val t = Table(Vector("e" -> Column.Dictionary(Array(1, 0, 0), Column.Utf8(Array("a", "b"), Array(true, true)), false,
      Array(true, false, true))), Vector.empty)
    assertEquals(RArrowFrames.frame(t).cols, Vector("e" -> Vector(RValue.Str("b"), RValue.NA(RType.Character), RValue.Str("a"))))
  }
