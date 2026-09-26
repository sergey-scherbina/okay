package okay.foreign

import okay.arrow.{Column, Table}

/** okay-arrow stage 8: a frame reads a dictionary-encoded column as its values */
class TestArrowFramesDictionary extends munit.FunSuite:
  test("a Dictionary column becomes the frame's strings, nulls in place") {
    val t = Table(Vector("e" -> Column.Dictionary(Array(1, 0, 0), Column.Utf8(Array("a", "b"), Array(true, true)), false,
      Array(true, false, true))), Vector.empty)
    assertEquals(ArrowFrames.frame(t).cols, Vector("e" -> Vector(PyValue.Str("b"), PyValue.PyNone, PyValue.Str("a"))))
  }
