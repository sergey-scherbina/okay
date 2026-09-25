package okay.sql

import okay.codec.Schema

/**
 * stack-safety-okay2-catch-up: `Typed.shapeOf` matches the Schema TREE
 * directly, and a derived schema of a recursive type is a cycle of lazy
 * thunks — so a row type that holds itself was a StackOverflowError at
 * the first `Query.field`, where "not row-shaped" was the promise. A row
 * cannot hold itself (there is no column for it), so a product met again
 * on its own path is refused by name, at construction.
 */
class TestTypedRecursive extends munit.FunSuite:

  final case class Tree(label: String, kids: Vector[Tree]) derives Schema
  final case class A(name: String, b: Option[B]) derives Schema
  final case class B(n: Int, a: Option[A]) derives Schema
  final case class Flat(id: Long, tags: Vector[String], at: Option[Long]) derives Schema

  test("a self-recursive row type is refused by name, not a stack overflow") {
    val e = Query.field[Tree, String]("label")
    assert(e.left.exists(m => m.contains("Tree") && m.contains("recursive")), e.toString)
  }

  test("mutual recursion is refused too, naming the product met again") {
    val e = Query.field[A, String]("name")
    assert(e.left.exists(m => m.contains("recursive") && (m.contains("A") || m.contains("B"))), e.toString)
  }

  test("a flat row with nested collections is still row-shaped") {
    assertEquals(Query.field[Flat, Long]("id").isRight, true)
    assertEquals(Query.columns[Flat], Right(Vector("id", "tags", "at")))
  }
