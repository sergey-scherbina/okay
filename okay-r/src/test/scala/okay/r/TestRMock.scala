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
