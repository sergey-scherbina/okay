package okay.delta

import okay.{!, +, Async, Chunk, ChunkBuf, Handler, Produce}
import okay.given
import okay.sql.{SqlType, SqlValue, Typed}
import okay.sql.given
import okay.jdbc.JdbcSql
import java.nio.file.Files
import java.sql.DriverManager

/**
 * Delta without Spark (specs/data.md, lake-delta): the kernel writes
 * the table, the kernel reads it back, and DuckDB's delta extension —
 * the JDBC read road — reads the same files through the seam. The
 * load id lands once.
 */
class TestDelta extends munit.FunSuite:

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

  val columns = Vector(
    Delta.Column("id", SqlType.I32, nullable = false),
    Delta.Column("sensor", SqlType.Text, nullable = false),
    Delta.Column("value", SqlType.F64),
    Delta.Column("seen", SqlType.I64),
    Delta.Column("ok", SqlType.Bool),
    Delta.Column("amount", SqlType.Num))

  def row(i: Int): Vector[SqlValue] = Vector(
    SqlValue.I32(i), SqlValue.Text(s"sensor-${i % 3}"),
    if i % 5 == 0 then SqlValue.Null else SqlValue.F64(i * 0.5),
    SqlValue.I64(i.toLong * 1000), SqlValue.Bool(i % 2 == 0),
    SqlValue.Num(BigDecimal(s"$i.25")))

  def rows(from: Int, until: Int): Chunk[Vector[SqlValue]] = ChunkBuf.of((from until until).toVector.map(row))

  final case class Reading(id: Int, sensor: String, value: Option[Double], seen: Option[Long],
                           ok: Option[Boolean], amount: Option[BigDecimal])
  given okay.codec.Schema[Reading] = okay.codec.Schema.derived

  def freshTable(): String =
    val dir = Files.createTempDirectory("okay-delta")
    dir.resolve("readings").toString

  test("create + append through the kernel; the kernel's own scan reads it back equal; snapshot names version and schema") {
    val path = freshTable()
    assertEquals(run(Delta.create(path, columns)).version, 0L)
    assertEquals(run(Delta.append(path, rows(0, 100))).version, 1L)
    assertEquals(run(Delta.append(path, rows(100, 150))).version, 2L)
    val snap = run(Delta.snapshot(path))
    assertEquals(snap.version, 2L)
    assertEquals(snap.columns.map(c => (c.label, c.tpe, c.nullable)), columns.map(c => (c.name, c.tpe, c.nullable)))
    val got = run(Delta.rows(path)).sortBy(r => r.head match { case SqlValue.I32(i) => i; case _ => -1 })
    assertEquals(got.length, 150)
    assertEquals(got(7), row(7))
    assertEquals(got(10), row(10))   // the NULL survived
    assertEquals(got(149).last, SqlValue.Num(BigDecimal("149.25")))
  }

  test("a load id lands ONCE: the same (app, version) appended twice is one append") {
    val path = freshTable()
    run(Delta.create(path, columns)): Unit
    run(Delta.append(path, rows(0, 10), loadId = Some(("okay-load", 1L)))): Unit
    val again = try { run(Delta.append(path, rows(0, 10), loadId = Some(("okay-load", 1L)))): Unit; Right(()) }
                catch { case e: Exception => Left(e.getClass.getSimpleName) }
    assert(again.isLeft, s"the duplicate load was accepted: $again")
    assertEquals(run(Delta.rows(path)).length, 10)
    run(Delta.append(path, rows(10, 20), loadId = Some(("okay-load", 2L)))): Unit
    assertEquals(run(Delta.rows(path)).length, 20)
  }

  test("refusals are named: an array column, a row that does not fit the schema") {
    val path = freshTable()
    val e1 = intercept[IllegalArgumentException](run(Delta.create(path, Vector(Delta.Column("tags", SqlType.Arr(SqlType.Text))))))
    assert(e1.getMessage.contains("tags"), e1.getMessage)
    run(Delta.create(path, columns)): Unit
    val e2 = intercept[IllegalArgumentException](run(Delta.append(path, ChunkBuf.of(Vector(
      Vector(SqlValue.Text("not an id"), SqlValue.Text("s"), SqlValue.Null, SqlValue.Null, SqlValue.Null, SqlValue.Null))))))
    assert(e2.getMessage.contains("'id'"), e2.getMessage)
  }

  test("road 1: the kernel-written table read through the JDBC seam by DuckDB's delta extension — typed rows, verify (skips offline)") {
    val path = freshTable()
    run(Delta.create(path, columns)): Unit
    run(Delta.append(path, rows(0, 50))): Unit
    val conn = DriverManager.getConnection("jdbc:duckdb:")
    try
      val loaded =
        try { val st = conn.createStatement(); st.execute("INSTALL delta"); st.execute("LOAD delta"); st.close(); true }
        catch case e: Exception =>
          // the forked JVM's stdout is not always shown: leave the reason where it can be read
          Files.writeString(java.nio.file.Path.of(sys.props("java.io.tmpdir")).resolve("okay-delta-ext.txt"),
            s"duckdb delta extension unavailable: ${e.getMessage}")
          false
      assume(loaded, "DuckDB's delta extension could not be installed (offline?) — skips")
      val db = JdbcSql(conn)
      val sql = s"select id, sensor, value, seen, ok, amount from delta_scan('$path') order by id"
      val got = collectChunks(Typed.rows[Reading](db, sql)).flatten
      assertEquals(got.length, 50)
      assertEquals(got(7), Right(Reading(7, "sensor-1", Some(3.5), Some(7000L), Some(false), Some(BigDecimal("7.25")))))
      assertEquals(got(10).map(_.value), Right(None))
      // DuckDB describes a table function's columns as nullable — the
      // Parquet lesson again (specs/data.md): a lake column can be null
      // under you, so the non-Option fields are named, nothing else is
      assertEquals(run(Typed.verify[Reading](db, sql)).map(d => (d.column, d.found)),
        Vector(("id", "nullable"), ("sensor", "nullable")))
    finally conn.close()
  }
