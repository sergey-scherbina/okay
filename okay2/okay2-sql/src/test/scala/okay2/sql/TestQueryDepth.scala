package okay2.sql

final case class DeepItem(id: Long, name: String, qty: Option[Int])

/** stack-safety-query: a predicate built by a FOLD is as deep as the list
 * is long, and both interpreters and `fields` walked it one frame per
 * `and`/`or` (okay-sql's TestQueryDepth, the same cases) */
class TestQueryDepth extends munit.FunSuite {

  val id = Query.field[DeepItem, Long]("id").toOption.get
  val name = Query.field[DeepItem, String]("name").toOption.get
  val qty = Query.field[DeepItem, Int]("qty").toOption.get
  val n = 200000

  test("a predicate folded 200 000 deep renders, lists its fields and runs") {
    val w = (0 until n).map(i => id =!= i.toLong).reduce(_ and _)
    val (clause, params) = w.sql
    assertEquals(params.length, n)
    assertEquals(params.head, SqlValue.I64(0L): SqlValue)
    assertEquals(params.last, SqlValue.I64((n - 1).toLong): SqlValue)
    assert(clause.startsWith("(" * (n - 1) + "id <> ?) AND (id <> ?))"), clause.take(80))
    assertEquals(clause.count(_ == '?'), n)
    assertEquals(w.fields, Set("id"))
    assert(w.test(DeepItem(-1, "x", None)))
    assert(!w.test(DeepItem(n - 1L, "x", None)))
    assert(!w.test(DeepItem(0, "x", None)))
  }

  test("an OR folded 200 000 deep, and NOTs nested as deep, run too") {
    val any = (0 until n).map(i => id === i.toLong).reduce(_ or _)
    assert(any.test(DeepItem(n - 1L, "x", None)))
    assert(!any.test(DeepItem(n.toLong, "x", None)))
    var neg = id === 7L
    var i = 0
    while (i < n) { neg = !neg; i += 1 }
    assert(neg.test(DeepItem(7, "x", None)))
    assert(neg.sql._1.startsWith("NOT (NOT ("))
  }

  test("the empty clause still drops out exactly as before") {
    val all = Query.Where.all[DeepItem]
    assertEquals((all and (id === 1L)).sql, ("id = ?", Vector[SqlValue](SqlValue.I64(1L))))
    assertEquals(((id === 1L) or all).sql, ("id = ?", Vector[SqlValue](SqlValue.I64(1L))))
    assertEquals((all and all).sql, ("", Vector.empty[SqlValue]))
    assertEquals(((all and all) or (name like "a%")).sql, ("name LIKE ?", Vector[SqlValue](SqlValue.Text("a%"))))
    assertEquals((!all).sql, ("NOT ()", Vector.empty[SqlValue]))
    assertEquals(((!all) and qty.isNull).sql, ("(NOT ()) AND (qty IS NULL)", Vector.empty[SqlValue]))
    assertEquals((qty.isNotNull or ((id < 3L) and !(name === "b"))).sql,
      ("(qty IS NOT NULL) OR ((id < ?) AND (NOT (name = ?)))", Vector[SqlValue](SqlValue.I64(3L), SqlValue.Text("b"))))
    assertEquals(((id === 1L) and all and (id === 2L)).fields, Set("id"))
    assert(all.test(DeepItem(1, "x", None)))
    assert((!(all and all)).fields.isEmpty)
  }
}
