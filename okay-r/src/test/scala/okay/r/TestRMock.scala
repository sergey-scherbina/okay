package okay.r

import okay.Handler
import RValue.*

/**
 * What "an R call is an OPERATION" actually buys, checked without an
 * R anywhere (specs/r.md, and the correction recorded there).
 *
 * Two of the three claims the framing makes are structural and hold:
 * a canned-answers handler IS the mock, and a program written against
 * `REval` does not know which engine answered it. The third —
 * "journalable by `Durable`" — is NOT true as the spec first wrote
 * it, and the reason is in specs/r.md rather than in a comment here.
 */
class TestRMock extends munit.FunSuite:

  /** the mock, and there is nothing else to it: a handler is the seam */
  private def canned(answers: Map[String, RValue]): Handler[REval] = new:
    def handle[A](e: REval[A]): A = e match
      case REval.Call(fn, _) =>
        answers.get(fn).toRight(Condition("simpleError", s"no canned answer for '$fn'"))
      case REval.Frame(_, in, _) => Right(in)

  test("a canned-answers handler IS the mock — no R, no process, no test double to write") {
    val h = canned(Map("forecast::auto.arima" -> Vec(Vector(F64(42)))))
    assertEquals(h.handle(REval.Call("forecast::auto.arima", Vector.empty)),
      Right(Vec(Vector(F64(42)))))
    // and an unmocked call is a condition, not a null
    assert(h.handle(REval.Call("stats::lm", Vector.empty)).left.exists(_.message.contains("stats::lm")))
  }

  test("a program written against REval cannot tell which engine answered") {
    // the whole program: it names operations and nothing else
    def program(h: Handler[REval]): Either[Condition, RValue] =
      h.handle(REval.Call("mean", Vector(Vec(Vector(F64(1), F64(3))))))
    assertEquals(program(canned(Map("mean" -> Vec(Vector(F64(2)))))), Right(Vec(Vector(F64(2)))))
  }

  test("REval carries the TypeableK a row split needs, and its test is TOTAL") {
    // what makes REval usable in an effect row at all: the runtime
    // test that says an operation is one of ours
    val T = summon[okay.TypeableK[REval]]
    assert(T.unapply[Any](REval.Call("f", Vector.empty)).isDefined)
    assert(T.unapply[Any](REval.Frame("f", RFrame(Vector.empty), Vector.empty)).isDefined)
    // and says no to something that is not
    assert(T.unapply[Any]("not an operation").isEmpty)
  }

  test("the wire round-trips every RValue shape without an R present") {
    val values = Vector(
      RNull, NA(RType.Logical), NA(RType.Integer), NA(RType.Double), NA(RType.Character),
      Bool(true), Bool(false), I32(7), I32(-1), F64(1.5), F64(0.0),
      Str(""), Str("ok"), Vec(Vector(I32(1), NA(RType.Integer))))
    for v <- values do assertEquals(Wire.dec(Wire.enc(v)), v, v.toString)
    // NaN separately, because NaN != NaN
    Wire.dec(Wire.enc(F64(Double.NaN))) match
      case F64(d) => assert(d.isNaN)
      case other => fail(s"not a NaN: $other")
    // bytes separately, because an Array has reference equality
    Wire.dec(Wire.enc(Bytes(Array[Byte](1, 2)))) match
      case Bytes(bs) => assertEquals(bs.toVector, Vector[Byte](1, 2))
      case other => fail(s"not bytes: $other")
  }

  test("a frame round-trips through the wire, columns and order intact") {
    val f = RFrame(Vector(
      "x" -> Vector(F64(1), NA(RType.Double)),
      "s" -> Vector(Str("a"), Str("b"))))
    assertEquals(Wire.decFrame(Wire.encFrame(f)), Right(f))
  }

  test("something that is not a frame is a condition naming what arrived") {
    val bad = Wire.decFrame(okay.codec.Json.JStr("nope"))
    assert(bad.left.exists(_.kind == "WireError"), bad.toString)
  }

  // ── the typed frame layer (r-finish, specs/r.md) ─────────────────

  final case class Obs(site: String, temp: Double, n: Int, ok: Boolean, note: Option[String])
      derives okay.codec.Schema

  test("a frame maps to a Seq of a flat case class and BACK: order, count and NA in place") {
    val rows = Vector(
      Obs("kyiv", 21.5, 3, true, Some("clear")),
      Obs("lviv", 18.0, 5, false, None))
    val f = RFrame.of(rows).fold(c => fail(s"of: $c"), identity)
    assertEquals(f.cols.map(_._1), Vector("site", "temp", "n", "ok", "note"),
      "the field order IS the column order")
    assertEquals(f.cols.map(_._2.length), Vector(2, 2, 2, 2, 2))
    // the absent note keeps its COLUMN's type rather than becoming a logical NA
    assertEquals(f.cols.last._2, Vector(Str("clear"), NA(RType.Character)))
    assertEquals(f.rows[Obs], Right(rows))
  }

  test("a column the Schema does not name is an error NAMING the column; so is a field with no column") {
    val extra = RFrame(Vector(
      "site" -> Vector(Str("kyiv")), "temp" -> Vector(F64(1.0)), "n" -> Vector(I32(1)),
      "ok" -> Vector(Bool(true)), "note" -> Vector(RNull), "rainfall" -> Vector(F64(0.0))))
    extra.rows[Obs] match
      case Left(c) => assert(c.message.contains("'rainfall'"), c.message)
      case Right(_) => fail("an unnamed column was accepted")
    val short = RFrame(Vector("site" -> Vector(Str("kyiv")), "temp" -> Vector(F64(1.0))))
    short.rows[Obs] match
      case Left(c) => assert(c.message.contains("'n'"), c.message)
      case Right(_) => fail("a missing column was accepted")
  }

  test("a cell that does not fit its field is a condition naming the column and the row") {
    val wrong = RFrame(Vector(
      "site" -> Vector(Str("kyiv"), Str("lviv")), "temp" -> Vector(F64(1.0), Str("warm")),
      "n" -> Vector(I32(1), I32(2)), "ok" -> Vector(Bool(true), Bool(true)),
      "note" -> Vector(RNull, RNull)))
    wrong.rows[Obs] match
      case Left(c) =>
        assert(c.message.contains("'temp'") && c.message.contains("row 1"), c.message)
      case Right(_) => fail("a text cell landed in a Double field")
  }

  test("an empty frame of the right shape is an empty Seq, not an error") {
    val empty = RFrame(Vector("site" -> Vector.empty, "temp" -> Vector.empty, "n" -> Vector.empty,
      "ok" -> Vector.empty, "note" -> Vector.empty))
    assertEquals(empty.rows[Obs], Right(Vector.empty))
    assertEquals(RFrame.of(Vector.empty[Obs]).map(_.cols.map(_._1)),
      Right(Vector("site", "temp", "n", "ok", "note")))
  }

  test("an integer column reads into a Double field, and NA into an Option — R's own widening, stated") {
    final case class Row2(n: Double, maybe: Option[Int]) derives okay.codec.Schema
    val f = RFrame(Vector("n" -> Vector(I32(7)), "maybe" -> Vector(NA(RType.Integer))))
    assertEquals(f.rows[Row2], Right(Vector(Row2(7.0, None))))
  }

  // ── the columnar frame wire (r-frame-columnar-wire) ──────────────

  /** structural equality, NaN-aware: `F64(NaN) == F64(NaN)` is false
   * because `Double.NaN != Double.NaN`, which is R's own answer too
   * (`NaN == NaN` is NA there) — so a frame carrying a NaN cannot be
   * compared with `assertEquals` and is compared here instead */
  private def sameFrame(a: RFrame, b: RFrame): Boolean =
    a.cols.map(_._1) == b.cols.map(_._1) &&
      a.cols.map(_._2).zip(b.cols.map(_._2)).forall { (x, y) =>
        x.length == y.length && x.zip(y).forall {
          case (F64(p), F64(q)) => (p.isNaN && q.isNaN) || p == q
          case (p, q) => p == q
        }
      }

  test("the frame wire is COLUMNAR: one type per column, a plain values array, absences as indices") {
    val f = RFrame(Vector(
      "n" -> Vector(I32(1), NA(RType.Integer), I32(3)),
      "x" -> Vector(F64(1.5), F64(Double.NaN), NA(RType.Double))))
    val j = Wire.encFrame(f)
    val text = okay.codec.Json.print(j)
    assert(text.contains("\"type\":\"i\""), text)
    assert(text.contains("\"values\":[1,0,3]"), text)   // the absence carries the type's zero
    assert(text.contains("\"na\":[1]"), text)
    assert(text.contains("\"nan\":[1]"), text)
    assert(!text.contains("\"t\":\"i\""), "a per-cell tag survived: " + text)
    assert(Wire.decFrame(j).exists(sameFrame(_, f)), Wire.decFrame(j).toString)
  }

  test("all four NAs keep their type across the wire, and NA stays apart from NaN") {
    val f = RFrame(Vector(
      "l" -> Vector(NA(RType.Logical), Bool(true)),
      "i" -> Vector(NA(RType.Integer), I32(2)),
      "d" -> Vector(NA(RType.Double), F64(Double.NaN)),
      "s" -> Vector(NA(RType.Character), Str("x"))))
    assert(Wire.decFrame(Wire.encFrame(f)).exists(sameFrame(_, f)),
      Wire.decFrame(Wire.encFrame(f)).toString)
  }

  test("an empty column keeps its NAME and its place; the type of an EMPTY column is not ours to keep") {
    val f = RFrame(Vector("a" -> Vector.empty, "b" -> Vector(I32(1))))
    val back = Wire.decFrame(Wire.encFrame(f)).fold(c => fail(s"$c"), identity)
    assertEquals(back.cols.map(_._1), Vector("a", "b"))
    assertEquals(back.cols.head._2, Vector.empty)
    // stated rather than hidden: RFrame types VALUES, not columns, so
    // an empty column has no type on our side to send or to restore
  }

  test("a column the four atomic types cannot carry keeps the per-cell form, and still round-trips") {
    val f = RFrame(Vector(
      "mixed" -> Vector(I32(1), Str("two")),
      "raw" -> Vector(Bytes(Array[Byte](1, 2)))))
    val text = okay.codec.Json.print(Wire.encFrame(f))
    assert(text.contains("cells"), text)
    val back = Wire.decFrame(Wire.encFrame(f)).fold(c => fail(s"$c"), identity)
    assertEquals(back.cols.head._2, Vector(I32(1), Str("two")))
    assertEquals(back.cols.last._2.head match { case Bytes(b) => b.toVector; case o => fail(s"$o") },
      Vector[Byte](1, 2))
  }

  test("a frame format version this host does not know refuses BY NAME rather than guessing") {
    val future = okay.codec.Json.JObj(Vector(
      "t" -> okay.codec.Json.JStr("frame"),
      "v" -> okay.codec.Json.JNum(99),
      "cols" -> okay.codec.Json.JArr(Vector.empty)))
    Wire.decFrame(future) match
      case Left(c) => assert(c.message.contains("v99") && c.message.contains("v2"), c.message)
      case Right(_) => fail("a future frame format was accepted")
  }

  test("the v1 shape still READS: a host meeting its own older frame in a fixture is not stuck") {
    val v1 = okay.codec.Json.JObj(Vector(
      "t" -> okay.codec.Json.JStr("frame"),
      "cols" -> okay.codec.Json.JArr(Vector(
        okay.codec.Json.JArr(Vector(okay.codec.Json.JStr("n"),
          okay.codec.Json.JArr(Vector(
            okay.codec.Json.JObj(Vector("t" -> okay.codec.Json.JStr("i"), "v" -> okay.codec.Json.JNum(7))),
            okay.codec.Json.JObj(Vector("t" -> okay.codec.Json.JStr("na"), "of" -> okay.codec.Json.JStr("integer")))))))))))
    assertEquals(Wire.decFrame(v1), Right(RFrame(Vector("n" -> Vector(I32(7), NA(RType.Integer))))))
  }

