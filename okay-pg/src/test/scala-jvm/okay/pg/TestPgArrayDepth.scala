package okay.pg

import okay.sql.SqlValue

/**
 * stack-safety-sql-family: `parseArray` recurses once per `{` of a
 * literal read off the socket. Postgres caps an array at MAXDIM = 6
 * dimensions and refuses a deeper literal before storing it, so that is
 * the bound written into the parser — a literal past it is damage from
 * whatever is on the socket, refused by name. Run on a 256 KB stack,
 * where the walk of a 100 000-deep literal would otherwise overflow.
 */
class TestPgArrayDepth extends munit.FunSuite:

  private def smallStack[A](body: => A): A =
    var out: Either[Throwable, A] = Left(IllegalStateException("never ran"))
    val t = Thread(null, () => out = try Right(body) catch case e: Throwable => Left(e), "small-stack", 256L * 1024)
    t.start(); t.join()
    out.fold(e => throw e, identity)

  private def nested(n: Int) = ("{" * n) + "1" + ("}" * n)
  private val int = (s: String) => SqlValue.I32(s.toInt)

  test("six dimensions, Postgres's own maximum, parse") {
    var v: SqlValue = PgSql.parseArray(nested(PgSql.MaxDim), int)
    var d = 0
    while v.isInstanceOf[SqlValue.Arr] do { d += 1; v = v.asInstanceOf[SqlValue.Arr].elems.head }
    assertEquals((d, v), (PgSql.MaxDim, SqlValue.I32(1)))
  }

  test("a seventh dimension is refused by name") {
    val e = intercept[IllegalStateException](PgSql.parseArray(nested(PgSql.MaxDim + 1), int))
    assert(e.getMessage.contains("MAXDIM"), e.getMessage)
  }

  test("a literal nested 100 000 deep is refused on a small stack, not a stack overflow") {
    val e = intercept[IllegalStateException](smallStack(PgSql.parseArray(nested(100000), int)))
    assert(e.getMessage.contains("MAXDIM"), e.getMessage)
  }

  test("the round trip through the literal writer is unchanged") {
    val v = PgSql.parseArray("{{1,2},{NULL,4}}", int)
    assertEquals(v, SqlValue.Arr(Vector(SqlValue.Arr(Vector(SqlValue.I32(1), SqlValue.I32(2))),
      SqlValue.Arr(Vector(SqlValue.Null, SqlValue.I32(4))))))
    assertEquals(PgSql.textOf(v), Some("{{\"1\",\"2\"},{NULL,\"4\"}}"))
  }
