package okay.foreign

import okay.codec.Schema
import PyValue.*

object TestPyCodec:
  final case class Line(sku: String, qty: Int) derives Schema
  final case class Basket(id: Long, lines: Vector[Line], coupon: Option[String] = None) derives Schema

/** PyCodec without python3 (foreign-typed-calls): runs in the default gate */
class TestPyCodec extends munit.FunSuite {
  import TestPyCodec.*

  test("a nested case class round-trips, a default filling a missing field") {
    val b = Basket(9007199254740993L, Vector(Line("a", 1), Line("b", 2)))
    assertEquals(PyCodec.decode[Basket](PyCodec.encode(b)), Right(b))
    assertEquals(PyCodec.decode[Basket](Dict(Vector("id" -> I64(1), "lines" -> Arr(Vector.empty)))),
      Right(Basket(1, Vector.empty)))
  }

  test("a decode failure names the path") {
    val bad = Dict(Vector("id" -> I64(1), "lines" -> Arr(Vector(
      Dict(Vector("sku" -> Str("a"), "qty" -> I64(1))), Dict(Vector("sku" -> Str("b"), "qty" -> Str("x")))))))
    assertEquals(PyCodec.decode[Basket](bad), Left(Condition("Decode", ".lines[1].qty: expected an int that fits 32 bits, got Str(x)")))
  }

  test("a dict survives the wire both ways, nested past any native depth") {
    var v: PyValue = I64(0)
    for i <- 1 to 20000 do v = Dict(Vector(s"k$i" -> v))
    // walked with a loop: `==` and munit's diff recurse on a deep tree
    var back = Wire.dec(Wire.enc(v))
    var depth = 20000
    while depth > 0 do
      back match
        case Dict(Vector((k, inner))) =>
          assertEquals(k, s"k$depth")
          back = inner
        case other => fail(s"at depth $depth: $other")
      depth -= 1
    assertEquals(back, I64(0))
  }

  test("an int past 2^53 crosses the wire as digits, exactly") {
    val v = I64(9007199254740993L)
    assertEquals(Wire.enc(v), okay.codec.Json.JObj(Vector("t" -> okay.codec.Json.JStr("int"),
      "v" -> okay.codec.Json.JStr("9007199254740993"))))
    assertEquals(Wire.dec(Wire.enc(v)), v)
  }

  test("PyFrame.of and rows are inverses") {
    val lines = Vector(Line("a", 1), Line("b", 2))
    assertEquals(PyFrame.of(lines).flatMap(_.rows[Line]), Right(lines))
  }
}
