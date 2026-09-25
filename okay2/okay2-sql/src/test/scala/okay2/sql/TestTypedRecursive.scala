package okay2.sql

import okay2.codec.Schema

final case class RecTree(label: String, kids: Vector[RecTree])
object RecTree { implicit lazy val schema: Schema[RecTree] = Schema.derived }
final case class RecA(name: String, b: Option[RecB])
object RecA { implicit lazy val schema: Schema[RecA] = Schema.derived }
final case class RecB(n: Int, a: Option[RecA])
object RecB { implicit lazy val schema: Schema[RecB] = Schema.derived }
final case class FlatRow(id: Long, tags: Vector[String], at: Option[Long])
object FlatRow { implicit lazy val schema: Schema[FlatRow] = Schema.derived }

/** a row type that holds itself is refused by name at construction, not a
 * StackOverflowError in `Typed.shapeOf` (okay-sql's TestTypedRecursive) */
class TestTypedRecursive extends munit.FunSuite {

  test("a self-recursive row type is refused by name, not a stack overflow") {
    val e = Query.field[RecTree, String]("label")
    assert(e.left.exists(m => m.contains("RecTree") && m.contains("recursive")), e.toString)
  }

  test("mutual recursion is refused too, naming the product met again") {
    val e = Query.field[RecA, String]("name")
    assert(e.left.exists(m => m.contains("recursive") && (m.contains("RecA") || m.contains("RecB"))), e.toString)
  }

  test("a flat row with nested collections is still row-shaped") {
    assertEquals(Query.field[FlatRow, Long]("id").isRight, true)
    assertEquals(Query.columns[FlatRow], Right(Vector("id", "tags", "at")))
  }
}
