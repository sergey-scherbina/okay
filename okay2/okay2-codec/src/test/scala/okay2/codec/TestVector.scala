package okay2.codec

final case class Bag(names: Vector[String], nums: Vector[Int], nested: Vector[Vector[Boolean]])

/** Vector as a container, recursion proven (okay-codec's TestVector,
 * JSON half). */
class TestVector extends munit.FunSuite {

  test("Vector round-trips, nested included") {
    val b = Bag(Vector("a", "b c"), Vector(1, -2, 3), Vector(Vector(true), Vector.empty, Vector(false, true)))
    assertEquals(Json.read[Bag](Json.write(b)), Right(b))
  }

  test("a damaged JSON element is skipped, the arrived ones survive") {
    assertEquals(Json.read[Vector[Int]]("[1,2,oops,3"), Right(Vector(1, 2, 3)))
  }

  test("a RECURSIVE type derives and round-trips") {
    def deep(n: Int): Tree =
      if (n == 0) Tree("leaf", Vector.empty)
      else Tree(s"n$n", Vector(deep(n - 1), Tree(s"s$n", Vector.empty)))
    val t = deep(50)
    assertEquals(Json.read[Tree](Json.write(t)), Right(t))
  }

  test("a recursive SUM derives too") {
    val e: Expr = Expr.Add(Expr.Add(Expr.Num(1), Expr.Num(2)), Expr.Num(3))
    assertEquals(Json.read[Expr](Json.write(e)), Right(e))
  }
}
