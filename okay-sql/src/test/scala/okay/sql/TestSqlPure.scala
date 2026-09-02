package okay.sql

import okay.{!, +, Async, Chunk, Produce, effect}
import okay.codec.Schema

/** the platform-free half: name mapping, parameter binding, the
 * granted-isolation vocabulary — runs on JVM, JS and Native, which
 * is itself the structural no-java.sql assertion of specs/sql.md */
class TestSqlPure extends munit.FunSuite {

  final case class P(userName: String, age: Option[Int], balance: Double,
                     active: Boolean, blob: Array[Byte])
  given Schema[P] = Schema.derived

  test("camelCase becomes snake_case") {
    assertEquals(Typed.snake("userName"), "user_name")
    assertEquals(Typed.snake("id"), "id")
    assertEquals(Typed.snake("aBC"), "a_b_c")
  }

  test("params bind positionally in declared field order; Option binds Null or the value") {
    val bound = Params.bind(P("ann", Some(7), 1.5, true, Array[Byte](1)))
    assertEquals(bound.length, 5)
    assertEquals(bound(0), SqlValue.Text("ann"))
    assertEquals(bound(1), SqlValue.I32(7))
    assertEquals(bound(2), SqlValue.F64(1.5))
    assertEquals(bound(3), SqlValue.Bool(true))
    bound(4) match
      case SqlValue.Bytes(bs) => assertEquals(bs.toList, List[Byte](1))
      case other => fail(s"expected Bytes, got $other")
    assertEquals(Params.bind(P("bo", None, 0.0, false, Array.empty))(1), SqlValue.Null)
  }

  test("a non-row-shaped param refuses loudly, naming the field") {
    enum Color { case Red, Blue }
    given Schema[Color] = Schema.derived
    final case class Painted(hue: Color)
    given Schema[Painted] = Schema.derived
    val e = intercept[IllegalArgumentException](Params.bind(Painted(Color.Red)))
    assert(e.getMessage.contains("hue"), e.getMessage)
  }

  // ── sql-schema-composite: arrays and nested products ─────────────

  final case class Addr(street: String, zip: Option[Int], active: Boolean)
  given Schema[Addr] = Schema.derived
  final case class Person(id: Int, tags: Vector[String], scores: List[Option[Int]],
                          grid: Vector[Vector[Int]], home: Addr, work: Option[Addr])
  given Schema[Person] = Schema.derived

  val ann = Person(1, Vector("a", "b"), List(Some(3), None), Vector(Vector(1), Vector(2, 3)),
    Addr("main", Some(10), true), None)
  val annRow: Vector[SqlValue] = Vector(
    SqlValue.I32(1),
    SqlValue.Arr(Vector(SqlValue.Text("a"), SqlValue.Text("b"))),
    SqlValue.Arr(Vector(SqlValue.I32(3), SqlValue.Null)),
    SqlValue.Arr(Vector(SqlValue.Arr(Vector(SqlValue.I32(1))),
      SqlValue.Arr(Vector(SqlValue.I32(2), SqlValue.I32(3))))),
    SqlValue.Row(Vector(SqlValue.Text("main"), SqlValue.I32(10), SqlValue.Bool(true))),
    SqlValue.Null)

  test("a Vector/List field binds as Arr (nested, Option elements as Null) and a nested case class as Row") {
    assertEquals(Params.bind(ann), annRow)
  }

  /** a driver made of one described frame: the decode side of the
   * binding is platform-free, so it is proven here on all three */
  final class OneFrame(cols: Vector[Col], rows: Vector[Vector[SqlValue]]) extends Sql:
    def describe(sql: String): Vector[Col] ! Async = okay.pure(cols)
    def query(sql: String, params: Vector[SqlValue]): Chunk[Vector[SqlValue]] ! (Produce + Async) =
      effect[Produce + Async, Chunk[Vector[SqlValue]]](scala.collection.immutable.ArraySeq.from(rows))
    def update(sql: String, params: Vector[SqlValue]): Long ! Async = okay.pure(0L)
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = okay.pure(0L)
    def begin(isolation: Isolation): Granted ! Async = okay.pure(Granted(isolation, isolation))
    def commit(): Unit ! Async = okay.pure(())
    def rollback(): Unit ! Async = okay.pure(())
    def cancel(): Unit = ()

  val addrT = SqlType.Row(Vector(SqlType.Text, SqlType.I32, SqlType.Bool))
  val personCols = Vector(
    Col("id", SqlType.I32, false),
    Col("tags", SqlType.Arr(SqlType.Text), false),
    Col("scores", SqlType.Arr(SqlType.I32), false),
    Col("grid", SqlType.Arr(SqlType.Arr(SqlType.I32)), false),
    Col("home", addrT, false),
    Col("work", addrT, true))

  /** OneFrame never performs an Async operation, so its programs run
   * on JS too: Produce yields chunks, anything else is a test bug */
  def pureOf[A, F[+_]](p: A ! F): A =
    import okay.!.*
    (p.resume: @unchecked) match
      case Pure(a) => a
      case other => fail(s"an effect where none was expected: $other")

  def decoded[A: Schema](db: Sql): Vector[Either[Bad, A]] =
    import okay.!.*
    def go(rest: Chunk[Either[Bad, A]] ! (Produce + Async), acc: Vector[Either[Bad, A]]): Vector[Either[Bad, A]] =
      (rest.resume: @unchecked) match
        case Pure(_) => acc
        case Effect(e) => okay.<|>[Async, Produce](e) match
          case Right(c) => acc ++ c.asInstanceOf[Chunk[Either[Bad, A]]]
          case Left(_) => fail("Async where none was expected")
        case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
          case Right(c) => go(k(c), acc ++ c.asInstanceOf[Chunk[Either[Bad, A]]])
          case Left(_) => fail("Async where none was expected")
    go(Typed.rows[A](db, "q"), Vector.empty)

  test("Arr/Row decode into Vector/List/nested case class, recursively — the mirror of bind") {
    val out = decoded[Person](OneFrame(personCols, Vector(annRow)))
    assertEquals(out, Vector(Right(ann)))
  }

  test("verify speaks Arr/Row: a matching shape is clean, a driver's unnamed element type passes, a wrong element type drifts") {
    def drifts(cols: Vector[Col]) = pureOf(Typed.verify[Person](OneFrame(cols, Vector.empty), "q"))
    assertEquals(drifts(personCols), Vector.empty)
    // JDBC cannot name the element: Arr(Other) fits any Arr field
    assertEquals(drifts(personCols.updated(1, Col("tags", SqlType.Arr(SqlType.Other("ARRAY")), false))), Vector.empty)
    val wrong = drifts(personCols.updated(1, Col("tags", SqlType.Arr(SqlType.I64), false)))
    assertEquals(wrong.map(_.column), Vector("tags"))
    val shortRow = drifts(personCols.updated(4, Col("home", SqlType.Row(Vector(SqlType.Text)), false)))
    assertEquals(shortRow.map(_.column), Vector("home"))
  }

  test("composite damage is data naming the column: arity mismatch, a bad element") {
    val shortHome = annRow.updated(4, SqlValue.Row(Vector(SqlValue.Text("main"))))
    val badTag = annRow.updated(1, SqlValue.Arr(Vector(SqlValue.Text("a"), SqlValue.I32(2))))
    val out = decoded[Person](OneFrame(personCols, Vector(shortHome, badTag, annRow)))
    assertEquals(out.map(_.isRight), Vector(false, false, true))
    val Left(b0) = out(0): @unchecked
    assertEquals(b0.column, "home"); assert(b0.error.contains("3 fields"), b0.error)
    val Left(b1) = out(1): @unchecked
    assertEquals(b1.column, "tags"); assert(b1.error.contains("element 1"), b1.error)
    assertEquals(b1.row, 1L)
  }

  test("Granted names a downgrade so the caller can refuse it") {
    assert(!Granted(Isolation.Serializable, Isolation.Serializable).downgraded)
    assert(Granted(Isolation.Serializable, Isolation.ReadCommitted).downgraded)
  }
}
