package okay2.jdbc

import okay2.sql.SqlValue

/**
 * stack-safety-catch-up-okay2: a driver's nested arrays become a
 * `SqlValue`, and a `SqlValue` becomes what a statement binds, each walk
 * as deep as the VALUE — which nothing bounds. Both are explicit stacks.
 */
class TestJdbcDepth extends munit.FunSuite {

  val n = 200000

  test("a driver's arrays nested 200 000 deep read as a value, and the value binds back") {
    var o: AnyRef = Array[AnyRef](Integer.valueOf(7))
    var i = 1
    while (i < n) { o = Array[AnyRef](o, "x"); i += 1 }
    val v = JdbcSql.valueOf(o)
    var cur = v
    var depth = 1
    var more = true
    while (more) cur match {
      case SqlValue.Arr(Vector(inner, SqlValue.Text("x"))) => cur = inner; depth += 1
      case _ => more = false
    }
    assertEquals(depth, n)
    assertEquals(cur, SqlValue.Arr(Vector(SqlValue.I32(7))))
    var back: AnyRef = JdbcSql.jdbcOf(v)
    depth = 1
    more = true
    while (more) back match {
      case a: Array[AnyRef] if a.length == 2 => assertEquals(a(1), "x": AnyRef); back = a(0); depth += 1
      case _ => more = false
    }
    assertEquals(depth, n)
    back match {
      case a: Array[AnyRef] => assertEquals(a.toVector, Vector[AnyRef](Integer.valueOf(7)))
      case other => fail(s"the innermost array came back as $other")
    }
  }

  test("a composite nested 200 000 deep inside arrays binds as nested arrays") {
    var v: SqlValue = SqlValue.I64(1L)
    var i = 0
    while (i < n) { v = if (i % 2 == 0) SqlValue.Row(Vector(v)) else SqlValue.Arr(Vector(SqlValue.Null, v)); i += 1 }
    var b: AnyRef = JdbcSql.jdbcOf(v)
    var depth = 0
    var more = true
    while (more) b match {
      case a: Array[AnyRef] => b = a(a.length - 1); depth += 1
      case _ => more = false
    }
    assertEquals(depth, n)
    assertEquals(b, java.lang.Long.valueOf(1L): AnyRef)
  }

  test("the leaves convert exactly as before") {
    assertEquals(JdbcSql.valueOf(null), SqlValue.Null: SqlValue)
    // a primitive array outside a java.sql.Array was never an array value
    assert(JdbcSql.valueOf(Array[Int](1, 2)).isInstanceOf[SqlValue.Text])
    assertEquals(JdbcSql.valueOf(Array[AnyRef]()), SqlValue.Arr(Vector.empty): SqlValue)
    assert(JdbcSql.jdbcOf(SqlValue.Arr(Vector.empty)) match { case a: Array[AnyRef] => a.isEmpty; case _ => false })
    assertEquals(JdbcSql.jdbcOf(SqlValue.Null), null)
  }
}
