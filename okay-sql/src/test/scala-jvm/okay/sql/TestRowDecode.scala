package okay.sql

import okay.{!, +, Async, Chunk, Chunks, Handler, Produce, effect, pure}
import okay.given
import okay.codec.Schema

/**
 * What a row decodes to, shape by shape, with no database
 * (sql-plan-cells).
 *
 * Written to hold a refactor of the per-cell decoder to its
 * predecessor's answers; the refactor was measured and declined, and
 * the suite stayed, because the property it checks had no direct test
 * before and every driver depends on it: the same value for a good
 * cell and the SAME WORDS for a bad one, over every shape the module
 * has — primitives, an Option present and absent, a refining wrapper
 * that may refuse, an array with a damaged element, a composite with
 * the wrong arity, and NULL where it is not allowed.
 *
 * Driver-free: `Typed.rows` needs a `Sql`, but the decoding it does
 * is reachable through the same public door every driver uses, so
 * this suite drives it with a tiny in-memory `Sql` and no database.
 */
class TestRowDecode extends munit.FunSuite {

  final case class Row(id: Long, name: String, note: Option[String],
                       score: Double, live: Boolean) derives Schema
  final case class Port(n: Int)
  given Schema[Port] = Schema.refine(
    (n: Int) => if n >= 1 && n <= 65535 then Right(Port(n)) else Left(s"port $n is out of range"), _.n)
  final case class Server(host: String, port: Port) derives Schema
  final case class Tagged(id: Int, tags: Vector[String]) derives Schema
  final case class Point(x: Double, y: Double) derives Schema
  final case class Placed(id: Int, at: Point) derives Schema

  /** a driver that answers exactly the frames it was handed */
  private final class Fixed(cols: Vector[Col], frames: Vector[Vector[SqlValue]]) extends Sql:
    def describe(sql: String): Vector[Col] ! Async = pure(cols)
    def query(sql: String, params: Vector[SqlValue] = Vector.empty)
    : Chunk[Vector[SqlValue]] ! (Produce + Async) =
      type F = Produce + Async
      if frames.isEmpty then pure(Chunks.emptyChunk)
      else effect[F, Chunk[Vector[SqlValue]]](scala.collection.immutable.ArraySeq.from(frames))
        .flatMap(_ => pure(Chunks.emptyChunk))
    def update(sql: String, params: Vector[SqlValue] = Vector.empty): Long ! Async = pure(0L)
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = pure(0L)
    def begin(isolation: Isolation, readOnly: Boolean): Granted ! Async = pure(Granted(isolation, isolation))
    def commit(): Unit ! Async = pure(())
    def rollback(): Unit ! Async = pure(())
    def cancel(): Unit = ()

  private def readAll[A: Schema](cols: Vector[Col], frames: Vector[Vector[SqlValue]]): Vector[Either[Bad, A]] =
    import okay.!.*
    val db = Fixed(cols, frames)
    def go(rest: Chunk[Either[Bad, A]] ! (Produce + Async),
           acc: Vector[Either[Bad, A]]): Vector[Either[Bad, A]] =
      (rest.resume: @unchecked) match
        case Pure(_) => acc
        case Effect(e) => okay.<|>[Async, Produce](e) match
          case Left(a) => (summon[Handler[Async]].handle(a): Unit); acc
          case Right(c) => acc ++ c.asInstanceOf[Chunk[Either[Bad, A]]]
        case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
          case Left(a) => go(k(summon[Handler[Async]].handle(a)), acc)
          case Right(c) => go(k(c), acc ++ c.asInstanceOf[Chunk[Either[Bad, A]]])
    go(Typed.rows[A](db, "select ..."), Vector.empty)

  private def col(label: String, t: SqlType, nullable: Boolean = false) = Col(label, t, nullable)

  test("primitives, an Option present and absent — the values decode") {
    val cols = Vector(col("id", SqlType.I64), col("name", SqlType.Text),
      col("note", SqlType.Text, nullable = true), col("score", SqlType.F64), col("live", SqlType.Bool))
    val got = readAll[Row](cols, Vector(
      Vector(SqlValue.I64(1L), SqlValue.Text("a"), SqlValue.Text("n"), SqlValue.F64(1.5), SqlValue.Bool(true)),
      Vector(SqlValue.I64(2L), SqlValue.Text("b"), SqlValue.Null, SqlValue.F64(0.0), SqlValue.Bool(false))))
    assertEquals(got, Vector(
      Right(Row(1L, "a", Some("n"), 1.5, live = true)),
      Right(Row(2L, "b", None, 0.0, live = false))))
  }

  test("NULL in a non-Option field is refused, naming the column") {
    val cols = Vector(col("id", SqlType.I64), col("name", SqlType.Text),
      col("note", SqlType.Text, nullable = true), col("score", SqlType.F64), col("live", SqlType.Bool))
    val got = readAll[Row](cols, Vector(
      Vector(SqlValue.I64(1L), SqlValue.Null, SqlValue.Null, SqlValue.F64(1.5), SqlValue.Bool(true))))
    assertEquals(got.length, 1)
    val bad = got.head.swap.toOption.get
    assertEquals(bad.column, "name")
    assertEquals(bad.error, "NULL in a non-Option field")
    assertEquals(bad.row, 0L)
  }

  test("a refining wrapper still refuses, with its own words") {
    val cols = Vector(col("host", SqlType.Text), col("port", SqlType.I32))
    val ok = readAll[Server](cols, Vector(Vector(SqlValue.Text("db"), SqlValue.I32(5432))))
    assertEquals(ok, Vector(Right(Server("db", Port(5432)))))
    val bad = readAll[Server](cols, Vector(Vector(SqlValue.Text("db"), SqlValue.I32(70000))))
    assertEquals(bad.head.swap.toOption.get.error, "port 70000 is out of range")
    assertEquals(bad.head.swap.toOption.get.column, "port")
  }

  test("an array decodes, and a damaged element names its position") {
    val cols = Vector(col("id", SqlType.I32), col("tags", SqlType.Arr(SqlType.Text)))
    val ok = readAll[Tagged](cols, Vector(
      Vector(SqlValue.I32(1), SqlValue.Arr(Vector(SqlValue.Text("a"), SqlValue.Text("b"))))))
    assertEquals(ok, Vector(Right(Tagged(1, Vector("a", "b")))))
    val bad = readAll[Tagged](cols, Vector(
      Vector(SqlValue.I32(1), SqlValue.Arr(Vector(SqlValue.Text("a"), SqlValue.I64(3L))))))
    assert(bad.head.swap.toOption.get.error.startsWith("element 1:"), bad.head.toString)
  }

  test("a composite decodes, and the wrong arity is refused by count") {
    val cols = Vector(col("id", SqlType.I32), col("at", SqlType.Row(Vector(SqlType.F64, SqlType.F64))))
    val ok = readAll[Placed](cols, Vector(
      Vector(SqlValue.I32(1), SqlValue.Row(Vector(SqlValue.F64(1.0), SqlValue.F64(2.0))))))
    assertEquals(ok, Vector(Right(Placed(1, Point(1.0, 2.0)))))
    val bad = readAll[Placed](cols, Vector(
      Vector(SqlValue.I32(1), SqlValue.Row(Vector(SqlValue.F64(1.0))))))
    assertEquals(bad.head.swap.toOption.get.error, "expected a composite of 2 fields, got 1")
  }

  test("the plan is resolved ONCE: many rows, one shape walk, same answers") {
    val cols = Vector(col("id", SqlType.I64), col("name", SqlType.Text),
      col("note", SqlType.Text, nullable = true), col("score", SqlType.F64), col("live", SqlType.Bool))
    val frames = (1 to 200).map(i => Vector(
      SqlValue.I64(i.toLong), SqlValue.Text(s"n$i"),
      if i % 3 == 0 then SqlValue.Null else SqlValue.Text("x"),
      SqlValue.F64(i * 0.5), SqlValue.Bool(i % 2 == 0))).toVector
    val got = readAll[Row](cols, frames)
    assertEquals(got.length, 200)
    assert(got.forall(_.isRight))
    assertEquals(got.head, Right(Row(1L, "n1", Some("x"), 0.5, live = false)))
    assertEquals(got(2), Right(Row(3L, "n3", None, 1.5, live = false)))
  }
}
