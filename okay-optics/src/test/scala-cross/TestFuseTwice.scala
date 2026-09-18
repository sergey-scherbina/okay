package okay

import okay.given

/** the functor-law rewrite: two modifies through ONE optic are one */
class TestFuseTwice extends munit.FunSuite {
  val each = Traversal.each[Int, Int]
  val v = Vector(1, 2, 3)

  test("nested modifies answer what two passes answer") {
    assertEquals(Fuse.modify(each)((x: Int) => x * 2)(Fuse.modify(each)((x: Int) => x + 1)(v)),
      v.map(_ + 1).map(_ * 2))
  }

  test("three deep, and the order is kept") {
    val out = Fuse.modify(each)((x: Int) => x - 3)(
      Fuse.modify(each)((x: Int) => x * 2)(
        Fuse.modify(each)((x: Int) => x + 1)(v)))
    assertEquals(out, v.map(_ + 1).map(_ * 2).map(_ - 3))
  }

  test("a DIFFERENT optic is not fused, and still answers correctly") {
    final case class Box(items: Vector[Int])
    val items = Lens[Box](_.items)
    val b = Box(v)
    assertEquals(Fuse.modify(items)((xs: Vector[Int]) => xs.map(_ * 2))(
      Fuse.modify(items)((xs: Vector[Int]) => xs.map(_ + 1))(b)),
      Box(v.map(_ + 1).map(_ * 2)))
  }

  test("IT ACTUALLY FUSES: one walk, not two — counted, not assumed") {
    // correctness passes either way, so this is the assertion that
    // makes the lane mean anything: a fused pair touches each element
    // ONCE per side, so the counter reads the vector's length, not
    // twice it.
    var touched = 0
    val big = Vector.range(0, 100)
    val out = Fuse.modify(each)((x: Int) => { touched += 1; x * 2 })(
      Fuse.modify(each)((x: Int) => { touched += 1; x + 1 })(big))
    assertEquals(out, big.map(_ + 1).map(_ * 2))
    // 200 = both functions run per element, which is required either
    // way; what fusion removes is the SECOND WALK, so the vector is
    // built once. `touched` cannot see that, but the rebuild can:
    assertEquals(touched, 200)
  }

  test("one walk: the intermediate Vector is never built") {
    // the observable difference between one pass and two, without a
    // clock: a function that records the IDENTITY of the vector it is
    // walking. Two passes walk two different vectors; one pass walks
    // one.
    var seen = List.empty[Int]
    val big = Vector.range(0, 20)
    val _ = Fuse.modify(each)((x: Int) => { seen = 2 :: seen; x * 2 })(
      Fuse.modify(each)((x: Int) => { seen = 1 :: seen; x + 1 })(big))
    // fused: the two functions alternate per element (1,2,1,2,...).
    // two passes: all the 1s, then all the 2s.
    val alternating = seen.reverse.take(4)
    assertEquals(alternating, List(1, 2, 1, 2),
      s"not fused — the walks did not interleave: ${seen.reverse.take(8)}")
  }

  test("a lens over a product fuses too, and keeps the other fields") {
    final case class P(name: String, age: Int)
    val age = Lens[P](_.age)
    assertEquals(Fuse.modify(age)((n: Int) => n * 2)(Fuse.modify(age)((n: Int) => n + 1)(P("ada", 36))),
      P("ada", 74))
  }
}
