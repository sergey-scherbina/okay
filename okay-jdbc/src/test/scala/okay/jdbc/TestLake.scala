package okay.jdbc

import okay.{!, +, Async, Chunk, Handler, Produce}
import okay.given
import okay.codec.Schema
import okay.sql.{Sql, Typed}
import java.nio.file.Files
import java.sql.DriverManager

/**
 * The lake read road (specs/data.md, "Data lakes"): a Parquet file
 * is queried through the SAME Sql seam as every relational source —
 * DuckDB embedded is the engine, `read_parquet` the table, and the
 * machinery above the driver is ZERO new code: `Typed.verify`
 * passes, `Typed.rows` streams at fetch-size, which is the whole
 * point of having cut the seam.
 */
class TestLake extends munit.FunSuite {

  // Parquet's own schema marks these fields OPTIONAL (that is how
  // the format ships them by default), and verify makes you say so:
  // the Option fields below are the fingerprint lesson working, not
  // a concession — a lake column really can be null under you
  final case class Reading(id: Option[Long], sensor: Option[String],
                           value: Option[Double])
  given Schema[Reading] = Schema.derived

  private var dir: java.nio.file.Path = null

  override def beforeAll(): Unit =
    dir = Files.createTempDirectory("okay-lake")
    // the lake: a Parquet file written by "someone else" (DuckDB
    // itself serves as the producer — no extra machinery)
    val c = DriverManager.getConnection("jdbc:duckdb:")
    try
      val st = c.createStatement()
      st.execute(
        s"""copy (select range as id,
                        'sensor-' || (range % 7) as sensor,
                        range * 0.5 as value
                 from range(100000))
            to '${parquet}' (format parquet)""")
      st.close()
    finally c.close()

  override def afterAll(): Unit =
    Files.deleteIfExists(java.nio.file.Path.of(parquet))
    Files.deleteIfExists(dir)
    ()

  private def parquet: String = dir.resolve("readings.parquet").toString

  def withDb[A](f: Sql => A): A =
    val conn = DriverManager.getConnection("jdbc:duckdb:")
    try f(JdbcSql(conn))
    finally conn.close()

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

  val query = s"select id, sensor, value from read_parquet('PATH')"
  def sql: String = query.replace("PATH", parquet)

  test("verify passes against the Parquet file's own metadata") {
    withDb { db =>
      assertEquals(run(Typed.verify[Reading](db, sql)), Vector.empty)
    }
  }

  test("100k Parquet rows stream at fetch-size: constant memory, every row intact") {
    withDb { db =>
      val chunks = collectChunks(Typed.rows[Reading](db, sql + " order by id"))
      // never more than a fetch-size chunk in flight — the
      // constant-memory assertion, structural
      assert(chunks.nonEmpty)
      assert(chunks.forall(_.length <= 64), s"a chunk exceeded fetch-size")
      assertEquals(chunks.map(_.length).sum, 100000)
      val first = chunks.head.head.toOption.get
      assertEquals(first, Reading(Some(0L), Some("sensor-0"), Some(0.0)))
      assert(chunks.flatten.forall(_.isRight), "a lake row failed to decode")
    }
  }

  test("the same typed program, an aggregation pushed to the engine") {
    withDb { db =>
      final case class PerSensor(sensor: Option[String], n: Option[Long])
      given Schema[PerSensor] = Schema.derived
      val rs = collectChunks(Typed.rows[PerSensor](db,
        s"select sensor, count(*) n from read_parquet('${parquet}') group by sensor order by sensor")).flatten
      assertEquals(rs.length, 7)
      assert(rs.forall(_.isRight))
    }
  }
}
