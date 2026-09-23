package scala2probe

import okay.codec.Schema
import okay.sql.{Bad, SqlValue}
import okay.scala2._

object SqlModel {
  final case class Person(id: Long, fullName: String, age: Int)
  object Person {
    implicit val schema: Schema[Person] =
      Schemas.product3("Person", "id", "fullName", "age")(Person.apply)(p => (p.id, p.fullName, p.age))
  }
  final case class ByAge(min: Int)
  object ByAge {
    implicit val schema: Schema[ByAge] = Schemas.product1("ByAge", "min")(ByAge.apply)(_.min)
  }
}

/** okay-sql from Scala 2.13, against an in-memory H2 (specs/scala2-facade.md, stage 8) */
class TestSqlFromScala2 extends munit.FunSuite {
  import SqlModel._

  private var n = 0
  // the columns are NOT NULL because Person's fields are not Options:
  // verify reports a nullable column for a non-Option field as drift
  def fresh(nameColumn: String = "full_name VARCHAR(64) NOT NULL"): (java.sql.Connection, Db) = {
    n += 1
    // H2's own Driver, not DriverManager: DriverManager scans for drivers
    // ONCE per JVM and then hands out only those visible to the caller's
    // class loader, so in the unforked full matrix, where okay-jdbc's
    // suite registered H2 from ITS loader first, it answered "No suitable
    // driver found" here although this suite passed alone
    val c = new org.h2.Driver().connect("jdbc:h2:mem:s2sql" + n + ";DB_CLOSE_DELAY=-1", new java.util.Properties())
    val db = Db.jdbc(c)
    Eff.runAsync(db.update("CREATE TABLE person (id BIGINT PRIMARY KEY, " + nameColumn + ", age INT NOT NULL)"))
    (c, db)
  }

  test("insert with parameters, read back decoded by column label") {
    val (c, db) = fresh()
    try {
      val prog = for {
        a <- db.update("INSERT INTO person VALUES (?, ?, ?)", SqlValue.I64(1L), SqlValue.Text("Ada Lovelace"), SqlValue.I32(36))
        b <- db.updateOf("INSERT INTO person (id, full_name, age) VALUES (?, ?, ?)", Person(2, "Charles Babbage", 79))
        people <- db.all[Person]("SELECT * FROM person ORDER BY id")
      } yield (a + b, people)
      assertEquals(Eff.runAsync(Throws.run(prog)), Right((2L, Vector(Person(1, "Ada Lovelace", 36), Person(2, "Charles Babbage", 79)))))
      assertEquals(Eff.runAsync(Throws.run(db.allOf[Person, ByAge]("SELECT * FROM person WHERE age >= ?", ByAge(50)))).map(_.map(_.id)), Right(Vector(2L)))
    } finally c.close()
  }

  test("a row that does not decode is a Left in the stream, and a typed Throws[Bad] in all") {
    val (c, db) = fresh(nameColumn = "full_name VARCHAR(64)")
    try {
      Eff.runAsync(db.update("INSERT INTO person VALUES (1, NULL, 3)"))
      val streamed = Eff.runAsync(db.rows[Person]("SELECT * FROM person").runCollect)
      assert(streamed.head.isLeft, streamed.toString)
      Eff.runAsync(Throws.run(db.all[Person]("SELECT * FROM person"))) match {
        // H2 reports an unquoted column's name in upper case
        case Left(Bad(column, _, _)) => assertEquals(column, "FULL_NAME")
        case other => fail("expected a Bad, got " + other)
      }
    } finally c.close()
  }

  test("a transaction commits when its body completes and rolls back when it fails") {
    val (c, db) = fresh()
    try {
      Eff.runAsync(db.transaction()(tx => tx.update("INSERT INTO person VALUES (1, 'a', 1)")))
      val failed = scala.util.Try(Eff.runAsync(db.transaction()(tx =>
        tx.update("INSERT INTO person VALUES (2, 'b', 2)").flatMap(_ => Async.delay[Long](throw new IllegalStateException("boom"))))))
      assert(failed.isFailure)
      assertEquals(Eff.runAsync(Throws.run(db.all[Person]("SELECT * FROM person"))).map(_.map(_.id)), Right(Vector(1L)))
    } finally c.close()
  }

  test("verify names the columns a query no longer has") {
    val (c, db) = fresh()
    try {
      assertEquals(Eff.runAsync(db.verify[Person]("SELECT * FROM person")), Vector.empty)
      val drift = Eff.runAsync(db.verify[Person]("SELECT id, age FROM person"))
      assert(drift.exists(_.column.equalsIgnoreCase("full_name")), drift.toString)
    } finally c.close()
  }
}
