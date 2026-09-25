package okay2.jdbc

import java.nio.file.Files
import java.sql.DriverManager
import okay2.{!, +, Resource, Throws, pure}
import okay2.async.Async
import okay2.sql._
import okay2.sql.javatime._

/** SQLite through the seam (okay-jdbc's TestSqlite, without its Writes
 * bridge case): the embedded engine everyone has, the same typed layer
 * with zero new machinery, and read-only open mode as the embedded
 * world's no-DDL posture */
class TestSqlite extends munit.FunSuite {

  private var dbFile: String = null

  override def beforeAll(): Unit = {
    dbFile = Files.createTempDirectory("okay2-sqlite").resolve("their.db").toString
    val c = DriverManager.getConnection(s"jdbc:sqlite:$dbFile")
    try {
      val st = c.createStatement()
      st.execute("""create table customer(
        id integer primary key not null,
        user_name text not null,
        age integer,
        balance real not null,
        active boolean not null,
        avatar blob)""")
      st.execute("insert into customer values (1, 'ann', 25, 10.5, 1, x'0102'), (2, 'bob', null, -3.25, 0, null)")
      st.execute("create table stamps(id integer primary key not null, at timestamp not null, d date not null)")
      st.execute("insert into stamps values (1, '2026-09-02T06:00:00Z', '2026-09-02')")
      st.close()
    } finally c.close()
  }

  def withDb[A](f: Sql => A): A = {
    val conn = DriverManager.getConnection(s"jdbc:sqlite:$dbFile")
    try f(new JdbcSql(conn))
    finally conn.close()
  }

  def region[A](prog: A ! (Resource + Async)): A = Run(Resource.run[A, Async](prog))

  test("rows decode by label from SQLite's honest metadata; verify passes") {
    withDb { db =>
      assertEquals(Run(Typed.verify[Customer](db, "select * from customer")), Vector.empty)
      val rs = Run.rows[Customer](db, "select * from customer order by id")
      assertEquals(rs.length, 2)
      val ann = rs.head.toOption.get
      assertEquals(ann.userName, "ann")
      assertEquals(ann.age, Some(25))
      assertEquals(ann.avatar.map(_.toList), Some(List[Byte](1, 2)))
      val bob = rs(1).toOption.get
      assertEquals(bob.age, None)
      assertEquals(bob.balance, -3.25)
    }
  }

  test("verify names a drift: the dropped column, on SQLite metadata") {
    withDb { db =>
      val drifts = Run(Typed.verify[Customer](db, "select id, age, balance, active, avatar from customer"))
      assertEquals(drifts.map(_.column), Vector("user_name"))
      assertEquals(drifts.head.found, "absent")
    }
  }

  test("transact: SQLite grants the level it maps; a handled abort rolls back") {
    val conn = DriverManager.getConnection(s"jdbc:sqlite:$dbFile")
    try {
      val db = new JdbcSql(conn)
      val g = region(Typed.transact[Granted, Async](db, Isolation.Serializable)(g => pure[Async, Granted](g)))
      assertEquals(g.granted, Isolation.Serializable)
      val prog = Typed.transact[Long, Throws[String]](db, Isolation.Serializable) { _ =>
        db.update("insert into customer(id, user_name, balance, active) values (50, 'x', 1, 1)")
          .flatMap(_ => Throws.raise[String, Long]("no"))
      }
      assertEquals(region(Throws.runEither[Long, String, Resource with Async](prog)), Left("no"))
      assert(conn.getAutoCommit)
      assertEquals(Run.chunks(db.query("select count(*) c from customer where id = 50")).flatten.head.head, SqlValue.I32(0))
    } finally conn.close()
  }

  test("read-only open mode is the embedded no-DDL posture: reads full, writes refuse") {
    val props = new java.util.Properties()
    props.setProperty("open_mode", "1") // SQLITE_OPEN_READONLY
    val conn = DriverManager.getConnection(s"jdbc:sqlite:$dbFile", props)
    try {
      val db = new JdbcSql(conn)
      assertEquals(Run(Typed.verify[Customer](db, "select * from customer")), Vector.empty)
      assertEquals(Run.rows[Customer](db, "select * from customer").length, 2)
      intercept[java.sql.SQLException](Run(db.update("insert into customer(id, user_name, balance, active) values (70, 'w', 0, 1)"))): Unit
      intercept[java.sql.SQLException](Run(db.update("create table mine(x int)"))): Unit
    } finally conn.close()
  }

  test("no getObject(Class), no parameter metadata: the ISO text fallback reads and binds timestamp/date columns") {
    withDb { db =>
      val sql = "select id, at, d from stamps order by id"
      val one = SqliteStamp(1, java.time.Instant.parse("2026-09-02T06:00:00Z"), java.time.LocalDate.of(2026, 9, 2))
      assertEquals(Run.rows[SqliteStamp](db, sql), Vector(Right(one)))
      val two = SqliteStamp(2, java.time.Instant.parse("1969-12-31T23:59:59.999999Z"), java.time.LocalDate.of(1899, 12, 31))
      assertEquals(Run(Typed.update(db, "insert into stamps values (?, ?, ?)")(two)), 1L)
      assertEquals(Run.rows[SqliteStamp](db, sql), Vector(Right(one), Right(two)))
    }
  }
}
