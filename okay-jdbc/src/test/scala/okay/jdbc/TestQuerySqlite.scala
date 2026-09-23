package okay.jdbc

import okay.{!, +, %, Async, Stream, Writer}
import okay.given
import okay.codec.Schema
import okay.sql.{Query, Sql, SqlValue, Typed}
import java.nio.file.Files
import java.sql.DriverManager

/**
 * specs/optics-outside.md stage 9 — THE LAW against a real engine: the
 * rows SQLite returns for `Query.select(...).where(w)` are exactly
 * the rows `w.test` keeps in memory, for every predicate here; an
 * UPDATE rendered by `Query.update` leaves the table as the in-memory
 * edit leaves the rows.
 */
class TestQuerySqlite extends munit.FunSuite {

  final case class Customer(id: Long, userName: String, age: Option[Int], balance: Double, active: Boolean)
  given Schema[Customer] = Schema.derived

  private var dbFile: String = null

  override def beforeAll(): Unit =
    dbFile = Files.createTempDirectory("okay-query").resolve("q.db").toString
    val c = DriverManager.getConnection(s"jdbc:sqlite:$dbFile")
    try
      val st = c.createStatement()
      st.execute("create table customer(id integer primary key not null, user_name text not null, age integer, balance real not null, active boolean not null)")
      st.execute("insert into customer values (1, 'ann', 25, 10.5, 1), (2, 'bob', null, -3.25, 0), (3, 'cyril', 40, 0.0, 1), (4, 'Dana', 17, 5.0, 0)")
      st.close()
    finally c.close()

  def withDb[A](f: Sql => A): A =
    val conn = DriverManager.getConnection(s"jdbc:sqlite:$dbFile")
    try f(JdbcSql(conn))
    finally conn.close()

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  def rows(db: Sql, sql: String, params: Vector[SqlValue]): Vector[Customer] =
    summon[Stream[[W] =>> Unit ! Writer % W + Async, Async]]
      .iterator(Typed.rows[Customer](db, sql, params)).toVector.flatten.collect { case Right(c) => c }

  val id = Query.field[Customer, Long]("id").toOption.get
  val name = Query.field[Customer, String]("userName").toOption.get
  val age = Query.field[Customer, Int]("age").toOption.get
  val balance = Query.field[Customer, Double]("balance").toOption.get
  val active = Query.field[Customer, Boolean]("active").toOption.get
  val select = Query.select[Customer]("customer").toOption.get

  test("THE LAW: the engine's rows for every predicate are the in-memory test's rows") {
    val predicates: Vector[Query.Where[Customer]] = Vector(
      Query.Where.all, id === 1L, id =!= 1L, age >= 18, age < 18, age.isNull, age.isNotNull,
      balance < 0.0, (balance >= 0.0) and active.===(true), (name like "a%") or (name like "%yr%"),
      !(active === false), (age >= 18) and !(name like "c%"), name like "c_ril", name like "b%")
    withDb { db =>
      val everyone = rows(db, select.all._1, select.all._2)
      assertEquals(everyone.map(_.id), Vector(1L, 2L, 3L, 4L))
      for w <- predicates do
        val (sql, params) = select.where(w)
        val fromEngine = rows(db, sql, params).sortBy(_.id)
        val inMemory = everyone.filter(w.test).sortBy(_.id)
        assertEquals(fromEngine, inMemory, s"$sql with $params")
    }
  }

  test("SQLite's LIKE is case-insensitive for ASCII where SQL's is not — the one place the two roads part, said out loud") {
    withDb { db =>
      val (sql, params) = select.where(name like "d%")
      assertEquals(rows(db, sql, params).map(_.userName), Vector("Dana"))   // the engine matches 'Dana'
      assertEquals(rows(db, select.all._1, select.all._2).filter((name like "d%").test), Vector.empty)   // SQL's LIKE does not
    }
  }

  test("UPDATE: the engine after the statement equals the rows after the in-memory edit") {
    withDb { db =>
      val before = rows(db, select.all._1, select.all._2)
      val w = (age >= 18) and active.===(true)
      val u = Query.update[Customer]("customer").set(balance, 99.0)
      val (sql, params) = u.where(w)
      val n = run(db.update(sql, params))
      assertEquals(n, 2L)
      val after = rows(db, select.all._1, select.all._2).sortBy(_.id)
      val expected = before.map(c => if w.test(c) then u(c).toOption.get else c).sortBy(_.id)
      assertEquals(after, expected)
    }
  }
}
