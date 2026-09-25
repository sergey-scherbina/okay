package okay.sql

/**
 * stack-safety-catch-up-okay2: `Typed.fits` compares a field's type with
 * a column's, and both are trees — an array of arrays, a composite of
 * composites. It is an AND over pairs of nodes, walked as a worklist.
 */
class TestTypedDepth extends munit.FunSuite:

  val n = 200000

  def nest(leaf: SqlType, wrap: (SqlType, Int) => SqlType): SqlType =
    var t = leaf
    var i = 0
    while i < n do { t = wrap(t, i); i += 1 }
    t

  val shape: (SqlType, Int) => SqlType = (t, i) =>
    if i % 3 == 0 then SqlType.Row(Vector(SqlType.Text, t)) else SqlType.Arr(t)

  test("types nested 200 000 deep fit, and a mismatch at the bottom is found") {
    assert(Typed.fits(nest(SqlType.I64, shape), nest(SqlType.I32, shape)))
    assert(!Typed.fits(nest(SqlType.I32, shape), nest(SqlType.I64, shape)))
  }

  test("the rules are the same at the top") {
    assert(Typed.fits(SqlType.Arr(SqlType.I64), SqlType.Arr(SqlType.Other("_int8"))))
    assert(Typed.fits(SqlType.Row(Vector(SqlType.F64, SqlType.Text)), SqlType.Row(Vector(SqlType.Num, SqlType.Uuid))))
    assert(!Typed.fits(SqlType.Row(Vector(SqlType.I64)), SqlType.Row(Vector(SqlType.I64, SqlType.I64))))
    assert(!Typed.fits(SqlType.Arr(SqlType.I64), SqlType.I64))
    assert(Typed.fits(SqlType.Json, SqlType.Json))
  }
