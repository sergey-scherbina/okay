package okay2.sql

import Drive.{OneFrame, rows => decoded, run}

final case class P(userName: String, age: Option[Int], balance: Double, active: Boolean, blob: Array[Byte])

sealed trait Color
object Color {
  case object Red extends Color
  case object Blue extends Color
}
final case class Painted(hue: Color)

final case class Addr(street: String, zip: Option[Int], active: Boolean)
final case class Person(id: Int, tags: Vector[String], scores: List[Option[Int]],
                        grid: Vector[Vector[Int]], home: Addr, work: Option[Addr])
final case class Acct(id: Int, balance: BigDecimal, approx: Double, asText: String, when: String)
final case class Whole(id: Int, balance: Int)
final case class Holding(id: Int, quantity: BigInt)
final case class Tagged(id: Int, ref: java.util.UUID, note: String)

/** the platform-free half (okay-sql's TestSqlPure): name mapping,
 * parameter binding, decode through a one-frame driver, verify, the
 * granted-isolation vocabulary, placeholders, temporal text — on JVM,
 * Scala.js and Scala Native, itself the no-java.sql assertion */
class TestSqlPure extends munit.FunSuite {

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
    bound(4) match {
      case SqlValue.Bytes(bs) => assertEquals(bs.toList, List[Byte](1))
      case other => fail(s"expected Bytes, got $other")
    }
    assertEquals(Params.bind(P("bo", None, 0.0, false, Array.empty)).apply(1), SqlValue.Null)
  }

  test("a non-row-shaped param refuses loudly, naming the field") {
    val e = intercept[IllegalArgumentException](Params.bind(Painted(Color.Red)))
    assert(e.getMessage.contains("hue"), e.getMessage)
  }

  val ann = Person(1, Vector("a", "b"), List(Some(3), None), Vector(Vector(1), Vector(2, 3)), Addr("main", Some(10), true), None)
  val annRow: Vector[SqlValue] = Vector(
    SqlValue.I32(1),
    SqlValue.Arr(Vector(SqlValue.Text("a"), SqlValue.Text("b"))),
    SqlValue.Arr(Vector(SqlValue.I32(3), SqlValue.Null)),
    SqlValue.Arr(Vector(SqlValue.Arr(Vector(SqlValue.I32(1))), SqlValue.Arr(Vector(SqlValue.I32(2), SqlValue.I32(3))))),
    SqlValue.Row(Vector(SqlValue.Text("main"), SqlValue.I32(10), SqlValue.Bool(true))),
    SqlValue.Null)

  test("a Vector/List field binds as Arr (nested, Option elements as Null) and a nested case class as Row") {
    assertEquals(Params.bind(ann), annRow)
  }

  val addrT = SqlType.Row(Vector(SqlType.Text, SqlType.I32, SqlType.Bool))
  val personCols = Vector(
    Col("id", SqlType.I32, false),
    Col("tags", SqlType.Arr(SqlType.Text), false),
    Col("scores", SqlType.Arr(SqlType.I32), false),
    Col("grid", SqlType.Arr(SqlType.Arr(SqlType.I32)), false),
    Col("home", addrT, false),
    Col("work", addrT, true))

  test("Arr/Row decode into Vector/List/nested case class, recursively — the mirror of bind") {
    assertEquals(decoded[Person](new OneFrame(personCols, Vector(annRow))), Vector(Right(ann)))
  }

  test("verify speaks Arr/Row: a matching shape is clean, a driver's unnamed element type passes, a wrong element type drifts") {
    def drifts(cols: Vector[Col]) = run(Typed.verify[Person](new OneFrame(cols, Vector.empty), "q"))
    assertEquals(drifts(personCols), Vector.empty)
    assertEquals(drifts(personCols.updated(1, Col("tags", SqlType.Arr(SqlType.Other("ARRAY")), false))), Vector.empty)
    assertEquals(drifts(personCols.updated(1, Col("tags", SqlType.Arr(SqlType.I64), false))).map(_.column), Vector("tags"))
    assertEquals(drifts(personCols.updated(4, Col("home", SqlType.Row(Vector(SqlType.Text)), false))).map(_.column), Vector("home"))
  }

  test("composite damage is data naming the column: arity mismatch, a bad element") {
    val shortHome = annRow.updated(4, SqlValue.Row(Vector(SqlValue.Text("main"))))
    val badTag = annRow.updated(1, SqlValue.Arr(Vector(SqlValue.Text("a"), SqlValue.I32(2))))
    val out = decoded[Person](new OneFrame(personCols, Vector(shortHome, badTag, annRow)))
    assertEquals(out.map(_.isRight), Vector(false, false, true))
    val b0 = out(0).swap.toOption.get
    assertEquals(b0.column, "home"); assert(b0.error.contains("3 fields"), b0.error)
    val b1 = out(1).swap.toOption.get
    assertEquals(b1.column, "tags"); assert(b1.error.contains("element 1"), b1.error)
    assertEquals(b1.row, 1L)
  }

  val acctCols = Vector(Col("id", SqlType.I32, false), Col("balance", SqlType.Num, false),
    Col("approx", SqlType.Num, false), Col("as_text", SqlType.Num, false), Col("when", SqlType.Timestamp, false))
  val money = BigDecimal("12345678901234567890.123456789")

  test("a Num decodes exactly into BigDecimal and String, lossy into Double by the field's choice; a String reads any vendor type") {
    val row = Vector(SqlValue.I32(1), SqlValue.Num(money), SqlValue.Num(money), SqlValue.Num(money),
      SqlValue.Timestamp(Temporal.parseTimestamp("2026-09-02 06:00:00+00").get))
    assertEquals(decoded[Acct](new OneFrame(acctCols, Vector(row))),
      Vector(Right(Acct(1, money, money.toDouble, money.toString, "2026-09-02T06:00:00Z"))))
    assert(BigDecimal(money.toDouble) != money)
    assertEquals(Params.bind(Acct(1, money, 0.0, "", "")).apply(1), SqlValue.Text(money.toString))
  }

  test("verify: BigDecimal/Double/String fit Num, String fits Other; an Int does not fit Num") {
    assertEquals(run(Typed.verify[Acct](new OneFrame(acctCols, Vector.empty), "q")), Vector.empty)
    assertEquals(run(Typed.verify[Whole](new OneFrame(acctCols, Vector.empty), "q")).map(_.column), Vector("balance"))
  }

  val holdCols = Vector(Col("id", SqlType.I32, false), Col("quantity", SqlType.Num, false))
  val u64max = (BigInt(1) << 64) - 1

  test("a BigInt reads a whole Num exactly, widens an integer column, refuses a fraction; binds as its digits") {
    def one(v: SqlValue) = decoded[Holding](new OneFrame(holdCols, Vector(Vector(SqlValue.I32(1), v))))
    assertEquals(one(SqlValue.Num(BigDecimal(u64max))), Vector(Right(Holding(1, u64max))))
    assertEquals(one(SqlValue.I64(-5L)), Vector(Right(Holding(1, BigInt(-5)))))
    assert(one(SqlValue.Num(BigDecimal("1.5"))).head.isLeft)
    assertEquals(Params.bind(Holding(1, u64max)).apply(1), SqlValue.Text("18446744073709551615"))
    assertEquals(run(Typed.verify[Holding](new OneFrame(holdCols, Vector.empty), "q")), Vector.empty)
    val i64Col = Vector(Col("id", SqlType.I32, false), Col("quantity", SqlType.I64, false))
    assertEquals(run(Typed.verify[Holding](new OneFrame(i64Col, Vector.empty), "q")), Vector.empty)
  }

  test("Granted names a downgrade so the caller can refuse it") {
    assert(!Granted(Isolation.Serializable, Isolation.Serializable).downgraded)
    assert(Granted(Isolation.Serializable, Isolation.ReadCommitted).downgraded)
  }

  test("Placeholders.numbered: `?` becomes `$1..$n`; quoted literals and identifiers are left alone") {
    assertEquals(Placeholders.numbered("SELECT a FROM t WHERE x = ? AND y IN (?, ?)"),
      "SELECT a FROM t WHERE x = $1 AND y IN ($2, $3)")
    assertEquals(Placeholders.numbered("UPDATE t SET s = 'why?', \"odd?col\" = ? WHERE q = 'it''s?' AND id = ?"),
      "UPDATE t SET s = 'why?', \"odd?col\" = $1 WHERE q = 'it''s?' AND id = $2")
    assertEquals(Placeholders.numbered("SELECT 1"), "SELECT 1")
  }

  test("Temporal: civil dates round-trip over centuries, and the epoch is day 0") {
    assertEquals(Temporal.daysFromCivil(1970, 1, 1), 0)
    assertEquals(Temporal.daysFromCivil(2000, 3, 1), 11017)
    assertEquals(Temporal.civilFromDays(-719468), (0, 3, 1))
    for (days <- Seq(-1000000, -719468, -1, 0, 1, 59, 60, 11017, 20698, 1000000)) {
      val (y, m, d) = Temporal.civilFromDays(days)
      assertEquals(Temporal.daysFromCivil(y, m, d), days, s"$days -> $y-$m-$d")
    }
    assertEquals(Temporal.renderDate(20698), "2026-09-02")
    assertEquals(Temporal.parseDate("2026-09-02"), Some(20698))
    assertEquals(Temporal.parseDate("2026-13-02"), None)
    assertEquals(Temporal.renderDate(Temporal.parseDate("1899-12-31").get), "1899-12-31")
  }

  test("Temporal: a timestamp parses in pg's, H2's and ISO's forms, offsets applied, and renders ISO UTC") {
    val six = 20698L * 86400000000L + 6L * 3600000000L
    assertEquals(Temporal.parseTimestamp("2026-09-02 06:00:00+00"), Some(six))
    assertEquals(Temporal.parseTimestamp("2026-09-02T06:00:00Z"), Some(six))
    assertEquals(Temporal.parseTimestamp("2026-09-02 08:00:00+02"), Some(six))
    assertEquals(Temporal.parseTimestamp("2026-09-02 08:30:00+02:30"), Some(six))
    assertEquals(Temporal.parseTimestamp("2026-09-02 03:00:00-03"), Some(six))
    assertEquals(Temporal.parseTimestamp("2026-09-02 06:00:00"), Some(six), "no zone reads as UTC")
    assertEquals(Temporal.parseTimestamp("2026-09-02 06:00:00.123456+00"), Some(six + 123456))
    assertEquals(Temporal.parseTimestamp("2026-09-02 06:00:00.5"), Some(six + 500000))
    assertEquals(Temporal.parseTimestamp("2026-09-02T06:00:00.123456789Z"), Some(six + 123456), "nanos truncate to micros")
    assertEquals(Temporal.parseTimestamp("not a time"), None)
    assertEquals(Temporal.renderTimestamp(six), "2026-09-02T06:00:00Z")
    assertEquals(Temporal.renderTimestamp(six + 500000), "2026-09-02T06:00:00.5Z")
    assertEquals(Temporal.renderTimestamp(six + 1), "2026-09-02T06:00:00.000001Z")
    assertEquals(Temporal.renderTimestamp(-1L), "1969-12-31T23:59:59.999999Z", "before the epoch floors, never rounds")
    for (us <- Seq(0L, -1L, six, six + 1, -86400000000L * 400000))
      assertEquals(Temporal.parseTimestamp(Temporal.renderTimestamp(us)), Some(us))
  }

  test("Temporal: a time of day parses with or without seconds and fraction, and renders back") {
    assertEquals(Temporal.parseTime("06:00:00"), Some(21600000000L))
    assertEquals(Temporal.parseTime("06:00"), Some(21600000000L))
    assertEquals(Temporal.parseTime("23:59:59.999999"), Some(86399999999L))
    assertEquals(Temporal.parseTime("24:00:00"), None)
    assertEquals(Temporal.renderTime(21600000000L), "06:00:00")
    assertEquals(Temporal.renderTime(86399999999L), "23:59:59.999999")
  }

  test("a UUID field binds as Uuid and decodes from one; a String field reads a uuid column as its text") {
    val u = java.util.UUID.fromString("6ba7b810-9dad-11d1-80b4-00c04fd430c8")
    assertEquals(Params.bind(Tagged(1, u, "x")), Vector(SqlValue.I32(1), SqlValue.Uuid(u), SqlValue.Text("x")))
    val cols = Vector(Col("id", SqlType.I32, false), Col("ref", SqlType.Uuid, false), Col("note", SqlType.Uuid, false))
    assertEquals(decoded[Tagged](new OneFrame(cols, Vector(Vector(SqlValue.I32(1), SqlValue.Uuid(u), SqlValue.Uuid(u))))),
      Vector(Right(Tagged(1, u, u.toString))))
  }
}
