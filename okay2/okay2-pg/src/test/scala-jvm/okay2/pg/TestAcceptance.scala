package okay2.pg

import java.sql.DriverManager
import okay2.codec.Schema
import okay2.jdbc.JdbcSql
import okay2.sql.{Sql, Typed}
import Models._

/**
 * The two-driver acceptance (okay-pg's TestAcceptance, specs/sql.md): the
 * SAME typed program runs unmodified over the JDBC driver (H2) and the pg
 * wire driver against equivalent schemas, and the four verify drifts are
 * caught on BOTH drivers, naming the column — the typed layer, not the
 * driver, owns the contract.
 */
class TestAcceptance extends PgLive {

  /** the shared setup: the same table, each engine's own DDL accent */
  def setUp(db: Sql, serialBigint: String): Unit = {
    run(db.update("drop table if exists acceptance")): Unit
    run(db.update(s"""create table acceptance(
      id $serialBigint primary key not null,
      user_name varchar(64) not null,
      age int,
      balance double precision not null,
      active boolean not null)""")): Unit
    run(db.update("insert into acceptance values (1, 'ann', 25, 10.5, true), (2, 'bob', null, -3.25, false)")): Unit
  }

  /** THE program: one function of the trait alone; returns everything it
   * saw, so the assertion is that two drivers return the SAME value */
  def program(db: Sql, params: String): (Vector[String], Vector[(Long, String, Option[Int])], Long,
    Vector[String], Vector[String], Vector[String], Vector[String]) = {
    val clean = run(Typed.verify[AccCustomer](db, "select * from acceptance")).map(_.column)
    val rows = chunks(Typed.rows[AccCustomer](db, "select * from acceptance order by id"))
      .flatten.collect { case Right(c) => (c.id, c.userName, c.age) }.toVector
    // the SQL string is the dialect's: pg's extended protocol numbers
    // placeholders, JDBC marks them — the TYPED program is identical
    val inserted = run(Typed.update(db, s"insert into acceptance(id, user_name, balance, active) values ($params)")(
      NewRow(90, "zed", 1.0, true)))
    run(db.update("delete from acceptance where id = 90")): Unit

    def drifts[A](sql: String)(implicit s: Schema[A]): Vector[String] =
      run(Typed.verify[A](db, sql)).map(_.column.toLowerCase).distinct
    val dropped = drifts[AccCustomer]("select id, age, balance, active from acceptance")
    val renamed = drifts[AccCustomer]("select id, user_name as login, age, balance, active from acceptance")
    val retyped = drifts[AccCustomer]("select cast(id as varchar(20)) id, user_name, age, balance, active from acceptance")
    val nullab = drifts[Strict]("select id, age from acceptance")
    (clean, rows, inserted, dropped, renamed, retyped, nullab)
  }

  test("the same typed program, two drivers, one answer") {
    assume(available, skipped)
    val pg = connect()
    val h2conn = DriverManager.getConnection("jdbc:h2:mem:acc;DB_CLOSE_DELAY=-1", "sa", "")
    try {
      val h2 = new JdbcSql(h2conn)
      setUp(pg, "bigint")
      setUp(h2, "bigint")
      val overPg = program(pg, "$1, $2, $3, $4")
      val overH2 = program(h2, "?, ?, ?, ?")
      assertEquals(overPg, overH2)
      // and the drift content is the right one on both
      assertEquals(overPg._1, Vector.empty[String]) // clean verify
      assertEquals(overPg._3, 1L) // typed insert counted
      assertEquals(overPg._4, Vector("user_name")) // dropped
      assertEquals(overPg._5, Vector("user_name")) // renamed
      assertEquals(overPg._6, Vector("id")) // retyped (+ lost not-null)
      assertEquals(overPg._7, Vector("age")) // nullability
    } finally { pg.close(); h2conn.close() }
  }
}
