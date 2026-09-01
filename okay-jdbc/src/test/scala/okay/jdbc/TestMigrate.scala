package okay.jdbc

import okay.{!, Async}
import okay.given
import okay.sql.{Sql, SqlValue}
import java.sql.DriverManager

/**
 * The migration boxes of specs/jdbc.md: our OWN database, so DDL
 * rights are the point — and the fingerprint rule holds at this seam
 * exactly as at every other: a changed applied script refuses loudly
 * and nothing else runs.
 */
class TestMigrate extends munit.FunSuite {

  private var n = 0
  def freshDb(): java.sql.Connection =
    n += 1
    DriverManager.getConnection(s"jdbc:h2:mem:mig$n;DB_CLOSE_DELAY=-1")

  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  val v1 = Script(1, "users", "create table users(id int primary key, name varchar(64))")
  val v2 = Script(2, "seed", "insert into users values (1, 'ann'); insert into users values (2, 'bo')")
  val v3 = Script(3, "index", "create index users_name on users(name)")

  test("a fresh database applies in order; the table records; a re-run applies nothing") {
    val conn = freshDb(); val db: Sql = JdbcSql(conn)
    val first = run(Migrate(db, Seq(v1, v2)))
    assertEquals(first.map(_.map(_.version)), Right(Vector(1, 2)))
    val again = run(Migrate(db, Seq(v1, v2)))
    assertEquals(again, Right(Vector()))
    // the database answers "what am I"
    val st = conn.createStatement()
    val rs = st.executeQuery("select version, checksum from okay_schema_version order by version")
    rs.next(); assertEquals(rs.getInt(1), 1)
    assertEquals(rs.getString(2), Migrate.checksum(v1.sql))
    conn.close()
  }

  test("a new script appended: only it applies") {
    val conn = freshDb(); val db: Sql = JdbcSql(conn)
    assert(run(Migrate(db, Seq(v1, v2))).isRight)
    assertEquals(run(Migrate(db, Seq(v1, v2, v3))).map(_.map(_.version)), Right(Vector(3)))
    conn.close()
  }

  test("a changed applied script refuses naming the version; nothing else runs") {
    val conn = freshDb(); val db: Sql = JdbcSql(conn)
    assert(run(Migrate(db, Seq(v1))).isRight)
    val tampered = v1.copy(sql = v1.sql + " -- innocent comment")
    val out = run(Migrate(db, Seq(tampered, v2)))
    assert(out.left.exists(m => m.contains("v1") && m.contains("CHANGED")), out.toString)
    // v2 did not run: no rows arrived
    val rs = conn.createStatement().executeQuery("select count(*) from users")
    rs.next(); assertEquals(rs.getInt(1), 0)
    conn.close()
  }

  test("a vanished applied script refuses the same way") {
    val conn = freshDb(); val db: Sql = JdbcSql(conn)
    assert(run(Migrate(db, Seq(v1))).isRight)
    val out = run(Migrate(db, Seq(v2)))
    assert(out.left.exists(_.contains("VANISHED")), out.toString)
    conn.close()
  }

  test("duplicate or unordered versions refuse before touching the database") {
    val conn = freshDb(); val db: Sql = JdbcSql(conn)
    assert(run(Migrate(db, Seq(v1, v1.copy(name = "again")))).left.exists(_.contains("duplicate")))
    assert(run(Migrate(db, Seq(v2, v1))).left.exists(_.contains("order")))
    // nothing was ensured or applied
    val rs = conn.getMetaData.getTables(null, null, "OKAY_SCHEMA_VERSION", null)
    assert(!rs.next(), "the version table exists although both runs refused early")
    conn.close()
  }

  test("a failing script leaves no version row; the fixed one applies next run") {
    val conn = freshDb(); val db: Sql = JdbcSql(conn)
    assert(run(Migrate(db, Seq(v1))).isRight)
    val broken = Script(2, "seed", "insert into no_such_table values (1)")
    val out = run(Migrate(db, Seq(v1, broken)))
    assert(out.left.exists(m => m.contains("v2") && m.contains("failed")), out.toString)
    val rs = conn.createStatement().executeQuery("select count(*) from okay_schema_version")
    rs.next(); assertEquals(rs.getInt(1), 1)
    assertEquals(run(Migrate(db, Seq(v1, v2))).map(_.map(_.version)), Right(Vector(2)))
    conn.close()
  }

  test("record sees every Applied of the run, after its commit — the ops hook") {
    val conn = freshDb(); val db: Sql = JdbcSql(conn)
    var seen = Vector.empty[Applied]
    val out = run(Migrate(db, Seq(v1, v2), record = seen :+= _))
    assertEquals(out.map(_.map(_.version)), Right(Vector(1, 2)))
    assertEquals(seen.map(a => (a.version, a.name)), Vector((1, "users"), (2, "seed")))
    assertEquals(seen.map(_.checksum), Vector(Migrate.checksum(v1.sql), Migrate.checksum(v2.sql)))
    conn.close()
  }
}
