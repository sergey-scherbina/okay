package okay2.sql

final case class Customer(id: Long, userName: String, age: Option[Int], balance: Double, active: Boolean)

/** the query's two interpreters read one declaration: the rendered SQL
 * and the in-memory test agree by construction, and a field is refused
 * by name and by type where it is declared (okay-sql's TestQueryPure) */
class TestQueryPure extends munit.FunSuite {

  val id = Query.field[Customer, Long]("id").toOption.get
  val name = Query.field[Customer, String]("userName").toOption.get
  val age = Query.field[Customer, Int]("age").toOption.get
  val balance = Query.field[Customer, Double]("balance").toOption.get
  val active = Query.field[Customer, Boolean]("active").toOption.get

  val ann = Customer(1, "ann", Some(25), 10.5, true)
  val bob = Customer(2, "bob", None, -3.25, false)
  val cy = Customer(3, "cyril", Some(40), 0.0, true)
  val all = Vector(ann, bob, cy)

  test("a field is refused by NAME and by TYPE at construction, naming the fault") {
    assert(Query.field[Customer, Long]("nosuch").left.exists(_.contains("nosuch")))
    assert(Query.field[Customer, Int]("userName").left.exists(_.contains("Text")))
    assert(Query.field[Customer, String]("age").isLeft)
    assertEquals(Query.field[Customer, Int]("age").isRight, true)
  }

  test("DESCRIBE: the clause, its parameters in order, the columns as Typed names them") {
    val w = (name like "a%") and (age >= 18) and !(active === false)
    assertEquals(w.sql, ("((user_name LIKE ?) AND (age >= ?)) AND (NOT (active = ?))",
      Vector(SqlValue.Text("a%"), SqlValue.I32(18), SqlValue.Bool(false))))
    assertEquals(w.fields, Set("userName", "age", "active"))
    assertEquals((age.isNull or (balance < 0.0)).sql, ("(age IS NULL) OR (balance < ?)", Vector(SqlValue.F64(0.0))))
    assertEquals(Query.select[Customer]("customer").toOption.get.where(id === 1L),
      ("SELECT id, user_name, age, balance, active FROM customer WHERE id = ?", Vector(SqlValue.I64(1L))))
    assertEquals(Query.select[Customer]("customer").toOption.get.all._1, "SELECT id, user_name, age, balance, active FROM customer")
  }

  test("RUN: the same predicate in memory — three-valued NULL, LIKE, numbers, negation") {
    assertEquals(all.filter(c => (name like "a%").test(c)), Vector(ann))
    assertEquals(all.filter(c => (age >= 18).test(c)), Vector(ann, cy))
    assertEquals(all.filter(c => (age < 18).test(c)), Vector.empty)
    assertEquals(all.filter(c => age.isNull.test(c)), Vector(bob))
    assertEquals(all.filter(c => (!(age >= 30)).test(c)), Vector(ann, bob))
    assertEquals(all.filter(c => ((balance < 0.0) or (active === true)).test(c)), Vector(ann, bob, cy))
    assertEquals(all.filter(c => (name like "c_ril").test(c)), Vector(cy))
    assertEquals(all.filter(c => (name like "%b%").test(c)), Vector(bob))
    assertEquals(all.filter(c => Query.Where.all[Customer].test(c)), all)
  }

  test("UPDATE: the statement, its parameters (sets, then where), and the same edit in memory") {
    val u = Query.update[Customer]("customer").set(balance, 1.0).set(active, false)
    assertEquals(u.where(id === 2L), ("UPDATE customer SET balance = ?, active = ? WHERE id = ?",
      Vector(SqlValue.F64(1.0), SqlValue.Bool(false), SqlValue.I64(2L))))
    assertEquals(u(bob), Right(bob.copy(balance = 1.0, active = false)))
    val w = id === 2L
    assertEquals(all.map(c => if (w.test(c)) u(c).toOption.get else c), Vector(ann, bob.copy(balance = 1.0, active = false), cy))
  }
}
