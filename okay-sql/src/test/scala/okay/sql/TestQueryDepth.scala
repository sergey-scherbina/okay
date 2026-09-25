package okay.sql

import okay.codec.Schema

/**
 * stack-safety-query: a predicate is a tree, and one built by a FOLD —
 * `conds.reduce(_ and _)`, the natural way to write "all of these" —
 * is as deep as the list is long. Both interpreters and `fields` walked
 * it with one frame per `and`/`or`; they are explicit stacks now, and the
 * rendering stays exactly what it was.
 */
class TestQueryDepth extends munit.FunSuite:

  final case class Item(id: Long, name: String, qty: Option[Int])
  given Schema[Item] = Schema.derived

  val id = Query.field[Item, Long]("id").toOption.get
  val name = Query.field[Item, String]("name").toOption.get
  val qty = Query.field[Item, Int]("qty").toOption.get
  val n = 200000

  test("a predicate folded 200 000 deep renders, lists its fields and runs") {
    val w = (0 until n).map(i => id =!= i.toLong).reduce(_ and _)
    val (clause, params) = w.sql
    assertEquals(params.length, n)
    assertEquals(params.head, SqlValue.I64(0L))
    assertEquals(params.last, SqlValue.I64((n - 1).toLong))
    assert(clause.startsWith("(" * (n - 1) + "id <> ?) AND (id <> ?))"), clause.take(80))
    assertEquals(clause.count(_ == '?'), n)
    assertEquals(w.fields, Set("id"))
    assert(w.test(Item(-1, "x", None)))
    assert(!w.test(Item(n - 1, "x", None)))      // the LAST one refuses: the whole chain was run
    assert(!w.test(Item(0, "x", None)))          // the first refuses: short-circuit
  }

  test("an OR folded 200 000 deep, and NOTs nested as deep, run too") {
    val any = (0 until n).map(i => id === i.toLong).reduce(_ or _)
    assert(any.test(Item(n - 1, "x", None)))
    assert(!any.test(Item(n, "x", None)))
    var neg = id === 7L
    var i = 0
    while i < n do { neg = !neg; i += 1 }      // an even number of NOTs
    assert(neg.test(Item(7, "x", None)))
    assert(neg.sql._1.startsWith("NOT (NOT ("))
  }

  test("the empty clause still drops out exactly as before") {
    val all = Query.Where.all[Item]
    assertEquals((all and (id === 1L)).sql, ("id = ?", Vector(SqlValue.I64(1L))))
    assertEquals(((id === 1L) or all).sql, ("id = ?", Vector(SqlValue.I64(1L))))
    assertEquals((all and all).sql, ("", Vector.empty))
    assertEquals(((all and all) or (name like "a%")).sql, ("name LIKE ?", Vector(SqlValue.Text("a%"))))
    assertEquals((!all).sql, ("NOT ()", Vector.empty))
    assertEquals(((!all) and qty.isNull).sql, ("(NOT ()) AND (qty IS NULL)", Vector.empty))
    assertEquals((qty.isNotNull or ((id < 3L) and !(name === "b"))).sql,
      ("(qty IS NOT NULL) OR ((id < ?) AND (NOT (name = ?)))", Vector(SqlValue.I64(3L), SqlValue.Text("b"))))
    assertEquals(((id === 1L) and all and (id === 2L)).fields, Set("id"))
    assert(all.test(Item(1, "x", None)))
    assert((!(all and all)).fields.isEmpty)
  }
