package okay2.sql

import Row._

/** the typed-row builder: a compile-time-checked partial UPDATE/INSERT
 * with no case class declared (okay-sql's TestRow) */
class TestRow extends munit.FunSuite {

  val name = Column[String]("name")
  val age = Column[Int]("age")
  val balance = Column[Double]("balance")

  test("columns encode in insertion order") {
    val row = Row.empty.updated(name, "grace").updated(age, 30)
    assertEquals(row.toParams, Right(Vector("name" -> SqlValue.Text("grace"), "age" -> SqlValue.I32(30))))
    assertEquals(row.values, Right(Vector(SqlValue.Text("grace"), SqlValue.I32(30))))
  }

  test("an empty row encodes to nothing") {
    assertEquals(Row.empty.toParams, Right(Vector.empty))
  }

  test("a wide row keeps every column, none dropped or reordered") {
    val row = Row.empty.updated(name, "ada").updated(age, 28).updated(balance, 12.5)
    assertEquals(row.toParams, Right(Vector("name" -> SqlValue.Text("ada"), "age" -> SqlValue.I32(28), "balance" -> SqlValue.F64(12.5))))
  }

  test("the row's TYPE is exactly its columns: reading one never set does not compile") {
    assert(compileErrors(
      """okay2.sql.Row.empty.updated(okay2.sql.Column[String]("name"), "grace").get(okay2.sql.Column[Int]("age"))""").nonEmpty)
    // paired: the column that WAS set reads back
    assertEquals(Row.empty.updated(name, "grace").get(name), "grace")
  }
}
