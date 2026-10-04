package okay2.pg

import okay2.sql.SqlValue

/**
 * stack-safety-sql-family (okay-pg's TestPgArrayDepth): `parseArray`
 * recurses once per `{` of a literal read off the socket; Postgres caps
 * an array at MAXDIM = 6 dimensions, which is the bound written into the
 * parser — a literal past it is refused by name. Run on a 256 KB stack.
 */
class TestPgArrayDepth extends munit.FunSuite {

  private def smallStack[A](body: => A): A = {
    var out: Either[Throwable, A] = Left(new IllegalStateException("never ran"))
    val t = new Thread(null, () => out = try Right(body) catch { case e: Throwable => Left(e) }, "small-stack", 256L * 1024)
    t.start(); t.join()
    out.fold(e => throw e, identity)
  }

  private def nested(n: Int) = ("{" * n) + "1" + ("}" * n)
  private val int = (s: String) => SqlValue.I32(s.toInt)

  test("six dimensions, Postgres's own maximum, parse") {
    @annotation.tailrec def depth(v: SqlValue, d: Int): (Int, SqlValue) = v match {
      case SqlValue.Arr(es) => depth(es.head, d + 1)
      case other => (d, other)
    }
    assertEquals(depth(PgSql.parseArray(nested(PgSql.MaxDim), int), 0), (PgSql.MaxDim, SqlValue.I32(1): SqlValue))
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
      SqlValue.Arr(Vector(SqlValue.Null, SqlValue.I32(4))))): SqlValue)
    assertEquals(PgSql.textOf(v), Some("{{\"1\",\"2\"},{NULL,\"4\"}}"))
  }
}
