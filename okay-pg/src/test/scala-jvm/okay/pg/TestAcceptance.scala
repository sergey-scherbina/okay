package okay.pg

import okay.{!, +, Async, Chunk, Handler, Produce}
import okay.given
import okay.crypto.given
import okay.codec.Schema
import okay.jdbc.JdbcSql
import okay.sql.{Sql, Typed}
import java.sql.DriverManager

/**
 * The two-driver acceptance (specs/sql.md): the SAME typed program
 * runs unmodified over the JDBC driver (H2) and the pg wire driver
 * against equivalent schemas — the cluster acceptance-test move,
 * applied to SQL. And the four verify drifts are caught on BOTH
 * drivers, naming the column, which is the claim that the typed
 * layer, not the driver, owns the contract.
 */
class TestAcceptance extends munit.FunSuite {

  // integration-test-gate: out of the default gate, into `sbt integrationTest`
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  final case class Customer(id: Long, userName: String, age: Option[Int],
                            balance: Double, active: Boolean)
  given Schema[Customer] = Schema.derived

  final case class Strict(id: Long, age: Int)
  given Schema[Strict] = Schema.derived

  val host = sys.env.getOrElse("OKAY_PG_HOST", "127.0.0.1")
  val port = sys.env.get("OKAY_PG_PORT").flatMap(_.toIntOption).getOrElse(5432)

  lazy val available: Boolean =
    try { okay.!.run(okay.Async.run[PgSql, Nothing](PgSql.connect(host, port, "okay", "okay", "okay"))).close(); true }
    catch { case _: Throwable => false }

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  def collectChunks[A](s: Chunk[A] ! (Produce + Async)): List[Chunk[A]] =
    import okay.!.*
    def go(rest: Chunk[A] ! (Produce + Async), acc: List[Chunk[A]]): List[Chunk[A]] =
      (rest.resume: @unchecked) match
        case Pure(_) => acc.reverse
        case Effect(e) => okay.<|>[Async, Produce](e) match
          case Left(a) => (summon[Handler[Async]].handle(a): Unit); acc.reverse
          case Right(c) => (c.asInstanceOf[Chunk[A]] :: acc).reverse
        case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
          case Left(a) => go(k(summon[Handler[Async]].handle(a)), acc)
          case Right(c) => go(k(c), c.asInstanceOf[Chunk[A]] :: acc)
    go(s, Nil)

  /** the shared setup: the same table, each engine's own DDL accent */
  def setUp(db: Sql, serialBigint: String): Unit =
    run(db.update("drop table if exists acceptance")): Unit
    run(db.update(s"""create table acceptance(
      id $serialBigint primary key not null,
      user_name varchar(64) not null,
      age int,
      balance double precision not null,
      active boolean not null)""")): Unit
    run(db.update("insert into acceptance values " +
      "(1, 'ann', 25, 10.5, true), (2, 'bob', null, -3.25, false)")): Unit
    ()

  /**
   * THE program: one function of the trait alone. It verifies,
   * reads typed rows, writes with typed params, checks the four
   * drifts — and returns everything it saw, so the assertion is
   * that two drivers return the SAME value.
   */
  def program(db: Sql, params: String): (Vector[String], Vector[(Long, String, Option[Int])], Long,
    Vector[String], Vector[String], Vector[String], Vector[String]) =
    val clean = run(Typed.verify[Customer](db, "select * from acceptance")).map(_.column)
    val rows = collectChunks(Typed.rows[Customer](db, "select * from acceptance order by id"))
      .flatten.collect { case Right(c) => (c.id, c.userName, c.age) }.toVector
    final case class NewRow(id: Long, userName: String, balance: Double, active: Boolean)
    given Schema[NewRow] = Schema.derived
    // the SQL string is the dialect's (bind-don't-model keeps it
    // visible): pg's extended protocol numbers placeholders, JDBC
    // marks them — the TYPED program around the string is identical
    val inserted = run(Typed.update(db,
      s"insert into acceptance(id, user_name, balance, active) values ($params)")(
      NewRow(90, "zed", 1.0, true)))
    run(db.update("delete from acceptance where id = 90")): Unit

    def drifts[A: Schema](sql: String): Vector[String] =
      run(Typed.verify[A](db, sql)).map(_.column.toLowerCase).distinct
    val dropped = drifts[Customer]("select id, age, balance, active from acceptance")
    val renamed = drifts[Customer](
      "select id, user_name as login, age, balance, active from acceptance")
    val retyped = drifts[Customer](
      "select cast(id as varchar(20)) id, user_name, age, balance, active from acceptance")
    val nullab = drifts[Strict]("select id, age from acceptance")
    (clean, rows, inserted, dropped, renamed, retyped, nullab)

  test("the same typed program, two drivers, one answer") {
    assume(available, s"no Postgres at $host:$port — the acceptance skips")
    val pg = okay.!.run(okay.Async.run[PgSql, Nothing](PgSql.connect(host, port, "okay", "okay", "okay")))
    val h2conn = DriverManager.getConnection("jdbc:h2:mem:acc;DB_CLOSE_DELAY=-1", "sa", "")
    try
      val h2 = JdbcSql(h2conn)
      setUp(pg, "bigint")
      setUp(h2, "bigint")
      val overPg = program(pg, "$1, $2, $3, $4")
      val overH2 = program(h2, "?, ?, ?, ?")
      assertEquals(overPg, overH2)
      // and the drift content is the right one on both
      assertEquals(overPg._1, Vector.empty)         // clean verify
      assertEquals(overPg._3, 1L)                   // typed insert counted
      assertEquals(overPg._4, Vector("user_name"))  // dropped
      assertEquals(overPg._5, Vector("user_name"))  // renamed
      assertEquals(overPg._6, Vector("id"))         // retyped (+ lost not-null)
      assertEquals(overPg._7, Vector("age"))        // nullability
    finally
      pg.close(); h2conn.close()
  }
}
