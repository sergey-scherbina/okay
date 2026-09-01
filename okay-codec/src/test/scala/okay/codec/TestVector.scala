package okay.codec

/**
 * codec-vector: Schema learns Vector, recursion is PROVEN rather
 * than promised, and the closing exhibit is the type that filed the
 * task — okay-ui's tree derives and round-trips through both wires.
 * (The derivation of Ui itself lives in okay-ui's suite; here the
 * same shapes are proven on local types, keeping the dependency
 * arrow pointing the right way.)
 */
class TestVector extends munit.FunSuite {

  final case class Bag(names: Vector[String], nums: Vector[Int],
                       nested: Vector[Vector[Boolean]])
  given Schema[Bag] = Schema.derived

  test("Vector round-trips JSON and CBOR, nested included") {
    val b = Bag(Vector("a", "b c"), Vector(1, -2, 3),
      Vector(Vector(true), Vector.empty, Vector(false, true)))
    assertEquals(Json.read[Bag](Json.write(b)), Right(b))
    assertEquals(Cbor.read[Bag](Cbor.write(b)), Right(b))
  }

  test("a damaged JSON element is skipped, the arrived ones survive (the SList rule)") {
    assertEquals(Json.read[Vector[Int]]("[1,2,oops,3"), Right(Vector(1, 2, 3)))
  }

  // ---- recursion: the doc comment's claim, now a test

  final case class Tree(label: String, kids: Vector[Tree])
  given Schema[Tree] = Schema.derived

  test("a RECURSIVE type derives and round-trips — the thunks earn their keep") {
    def deep(n: Int): Tree =
      if n == 0 then Tree("leaf", Vector.empty)
      else Tree(s"n$n", Vector(deep(n - 1), Tree(s"s$n", Vector.empty)))
    val t = deep(64)
    assertEquals(Json.read[Tree](Json.write(t)), Right(t))
    assertEquals(Cbor.read[Tree](Cbor.write(t)), Right(t))
  }

  enum Expr:
    case Num(n: Int)
    case Add(l: Expr, r: Expr)
  given Schema[Expr] = Schema.derived

  test("a recursive SUM derives too") {
    val e = Expr.Add(Expr.Add(Expr.Num(1), Expr.Num(2)), Expr.Num(3))
    assertEquals(Json.read[Expr](Json.write(e)), Right(e))
  }
}
