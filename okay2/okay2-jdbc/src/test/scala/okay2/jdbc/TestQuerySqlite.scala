package okay2.jdbc

import java.nio.file.Files
import java.sql.DriverManager
import okay2.sql.{Query, Sql, SqlValue}

/** THE LAW against a real engine (okay-jdbc's TestQuerySqlite): the rows
 * SQLite returns for `Query.select(...).where(w)` are the rows `w.test`
 * keeps in memory, for every predicate here; an UPDATE leaves the table
 * as the in-memory edit leaves the rows */
class TestQuerySqlite extends munit.FunSuite {

  private var dbFile: String = null

  override def beforeAll(): Unit = {
    dbFile = Files.createTempDirectory("okay2-query").resolve("q.db").toString
    val c = DriverManager.getConnection(s"jdbc:sqlite:$dbFile")
    try {
      val st = c.createStatement()
      st.execute("create table q_customer(id integer primary key not null, user_name text not null, age integer, balance real not null, active boolean not null)")
      st.execute("insert into q_customer values (1, 'ann', 25, 10.5, 1), (2, 'bob', null, -3.25, 0), (3, 'cyril', 40, 0.0, 1), (4, 'Dana', 17, 5.0, 0)")
      st.close()
    } finally c.close()
  }

  def withDb[A](f: Sql => A): A = {
    val conn = DriverManager.getConnection(s"jdbc:sqlite:$dbFile")
    try f(new JdbcSql(conn))
    finally conn.close()
  }

  def rows(db: Sql, sql: String, params: Vector[SqlValue]): Vector[QCustomer] =
    Run.rows[QCustomer](db, sql, params).collect { case Right(c) => c }

  val id = Query.field[QCustomer, Long]("id").toOption.get
  val name = Query.field[QCustomer, String]("userName").toOption.get
  val age = Query.field[QCustomer, Int]("age").toOption.get
  val balance = Query.field[QCustomer, Double]("balance").toOption.get
  val active = Query.field[QCustomer, Boolean]("active").toOption.get
  val select = Query.select[QCustomer]("q_customer").toOption.get

  test("THE LAW: the engine's rows for every predicate are the in-memory test's rows") {
    val predicates: Vector[Query.Where[QCustomer]] = Vector(
      Query.Where.all, id === 1L, id =!= 1L, age >= 18, age < 18, age.isNull, age.isNotNull,
      balance < 0.0, (balance >= 0.0) and (active === true), (name like "a%") or (name like "%yr%"),
      !(active === false), (age >= 18) and !(name like "c%"), name like "c_ril", name like "b%")
    withDb { db =>
      val everyone = rows(db, select.all._1, select.all._2)
      assertEquals(everyone.map(_.id), Vector(1L, 2L, 3L, 4L))
      for (w <- predicates) {
        val (sql, params) = select.where(w)
        assertEquals(rows(db, sql, params).sortBy(_.id), everyone.filter(c => w.test(c)).sortBy(_.id), s"$sql with $params")
      }
    }
  }

  test("SQLite's LIKE is case-insensitive for ASCII where SQL's is not: the one place the two roads part") {
    withDb { db =>
      val (sql, params) = select.where(name like "d%")
      assertEquals(rows(db, sql, params).map(_.userName), Vector("Dana"))
      assertEquals(rows(db, select.all._1, select.all._2).filter(c => (name like "d%").test(c)), Vector.empty)
    }
  }

  test("UPDATE: the engine after the statement equals the rows after the in-memory edit") {
    withDb { db =>
      val before = rows(db, select.all._1, select.all._2)
      val w = (age >= 18) and (active === true)
      val u = Query.update[QCustomer]("q_customer").set(balance, 99.0)
      val (sql, params) = u.where(w)
      assertEquals(Run(db.update(sql, params)), 2L)
      val expected = before.map(c => if (w.test(c)) u(c).toOption.get else c).sortBy(_.id)
      assertEquals(rows(db, select.all._1, select.all._2).sortBy(_.id), expected)
    }
  }
}
