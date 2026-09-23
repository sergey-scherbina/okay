package okay.r

import okay.codec.Schema
import RValue.*

object TestRCodec:
  final case class Order(sku: String, qty: Int, price: Double, note: Option[String] = None) derives Schema
  final case class Big(id: Long) derives Schema
  enum Shape derives Schema:
    case Circle(r: Double)
    case Rect(w: Double, h: Double)

/** RCodec without R (foreign-typed-calls): runs in the default gate */
class TestRCodec extends munit.FunSuite {
  import TestRCodec.*

  test("a case class is a named list, and R's length-1 vectors decode as its scalars") {
    val o = Order("kyiv-7", 2, 1.5)
    assertEquals(RCodec.encode(o),
      Named(Vector("sku" -> Str("kyiv-7"), "qty" -> I32(2), "price" -> F64(1.5), "note" -> NA(RType.Character))))
    // what R answers: every scalar a length-1 vector
    val fromR = Named(Vector("sku" -> Vec(Vector(Str("kyiv-7"))), "qty" -> Vec(Vector(I32(2))),
      "price" -> Vec(Vector(F64(1.5))), "note" -> Vec(Vector(NA(RType.Character)))))
    assertEquals(RCodec.decode[Order](fromR), Right(o))
  }

  test("a sum names its case in `type`") {
    assertEquals(RCodec.encode(Shape.Rect(2, 3): Shape),
      Named(Vector("type" -> Str("Rect"), "w" -> F64(2), "h" -> F64(3))))
    assertEquals(RCodec.decode[Shape](Named(Vector("type" -> Vec(Vector(Str("Circle"))), "r" -> F64(1)))),
      Right(Shape.Circle(1)))
  }

  test("a Long past 32 bits: an exact double, then its digits — never truncated") {
    for x <- Seq(3000000000L, -3000000000L, 9007199254740993L, Long.MaxValue, 7L) do
      assertEquals(RCodec.decode[Long](RCodec.encode(x)), Right(x), s"$x")
  }

  test("the frame codec keeps a Long past 32 bits (it truncated with toInt)") {
    val rows = Vector(Big(3000000000L), Big(1L))
    assertEquals(RFrame.of(rows).flatMap(_.rows[Big]), Right(rows))
  }

  test("a decode failure names the path, R-style") {
    assertEquals(RCodec.decode[Vector[Order]](Vec(Vector(
      RCodec.encode(Order("a", 1, 1.0)), Named(Vector("sku" -> Str("b"), "qty" -> Str("x"), "price" -> F64(1)))))),
      Left(Condition("Decode", "[2]$qty: expected an integer, got Str(x)")))
  }

  test("NULL, NA and the empty vector are None; a value is Some") {
    for v <- Seq(RNull, NA(RType.Integer), Vec(Vector(NA(RType.Integer))), Vec(Vector.empty)) do
      assertEquals(RCodec.decode[Option[Int]](v), Right(None), s"$v")
    assertEquals(RCodec.decode[Option[Int]](Vec(Vector(I32(4)))), Right(Some(4)))
  }

  test("a named list survives the wire both ways, nested") {
    val v = Named(Vector("a" -> Named(Vector("b" -> I32(1))), "c" -> Vec(Vector(Str("x"), Str("y")))))
    assertEquals(Wire.dec(Wire.enc(v)), v)
  }
}
