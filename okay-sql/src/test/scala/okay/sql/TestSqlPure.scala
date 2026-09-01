package okay.sql

import okay.codec.Schema

/** the platform-free half: name mapping, parameter binding, the
 * granted-isolation vocabulary — runs on JVM, JS and Native, which
 * is itself the structural no-java.sql assertion of specs/sql.md */
class TestSqlPure extends munit.FunSuite {

  final case class P(userName: String, age: Option[Int], balance: Double,
                     active: Boolean, blob: Array[Byte])
  given Schema[P] = Schema.derived

  test("camelCase becomes snake_case") {
    assertEquals(Typed.snake("userName"), "user_name")
    assertEquals(Typed.snake("id"), "id")
    assertEquals(Typed.snake("aBC"), "a_b_c")
  }

  test("params bind positionally in declared field order; Option binds Null or the value") {
    val bound = Params.bind(P("ann", Some(7), 1.5, true, Array[Byte](1)))
    assertEquals(bound.length, 5)
    assertEquals(bound(0), SqlValue.Text("ann"))
    assertEquals(bound(1), SqlValue.I32(7))
    assertEquals(bound(2), SqlValue.F64(1.5))
    assertEquals(bound(3), SqlValue.Bool(true))
    bound(4) match
      case SqlValue.Bytes(bs) => assertEquals(bs.toList, List[Byte](1))
      case other => fail(s"expected Bytes, got $other")
    assertEquals(Params.bind(P("bo", None, 0.0, false, Array.empty))(1), SqlValue.Null)
  }

  test("a non-row-shaped param refuses loudly, naming the field") {
    final case class Nested(inner: List[Int])
    given Schema[Nested] = Schema.derived
    val e = intercept[IllegalArgumentException](Params.bind(Nested(List(1))))
    assert(e.getMessage.contains("inner"), e.getMessage)
  }

  test("Granted names a downgrade so the caller can refuse it") {
    assert(!Granted(Isolation.Serializable, Isolation.Serializable).downgraded)
    assert(Granted(Isolation.Serializable, Isolation.ReadCommitted).downgraded)
  }
}
