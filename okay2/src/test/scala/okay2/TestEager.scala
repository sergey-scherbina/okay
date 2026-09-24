package okay2

import Produce.Emit

/** The opt-in eager encoding: kyo-class speed, kyo's stated hazards —
 * the Scala 3 core's TestEager */
class TestEager extends munit.FunSuite {
  import Eager._

  def prog[M[_, _]](implicit E: Effects[M]): M[Produce, Int] =
    E.flatMap(E.perform[Produce, Int](Emit(1)))(x => E.map(E.perform[Produce, Int](Emit(x + 1)))(y => x + y))

  test("the tagless encodings agree: Eager, Free") {
    assertEquals(Effects[Eager.Rep].runWith(prog[Eager.Rep]), 3)
    assertEquals(prog[Free].runWith, 3)
  }

  test("eagerness is real: a pure bind chain evaluates at CONSTRUCTION") {
    val E = Effects[Eager.Rep]
    var steps = 0
    val built = (1 to 1000).foldLeft(E.pure[Produce, Int](0)) { (m, _) =>
      E.flatMap(m)((x: Int) => { steps += 1; E.pure[Produce, Int](x + 1) })
    }
    assertEquals(steps, 1000)                   // the work already happened
    assertEquals(E.runWith(built), 1000)        // running is O(1): it IS the value
    assert((built: Any).isInstanceOf[Integer])  // literally the boxed result
  }

  test("the same chain under Free stays a value until run (the contrast)") {
    val E = Effects[Free]
    var steps = 0
    val built = (1 to 1000).foldLeft(E.pure[Produce, Int](0)) { (m, _) =>
      E.flatMap(m)((x: Int) => { steps += 1; E.pure[Produce, Int](x + 1) })
    }
    assertEquals(steps, 0)                      // nothing ran at construction
    assertEquals(built.runWith, 1000)
    assertEquals(steps, 1000)
  }

  test("tailcall: a deferred call commits to the tree, not the O(1) value path") {
    def isEven[M[_, _]](n: Int)(implicit E: Effects[M]): M[Pure, Boolean] =
      if (n == 0) E.pure[Pure, Boolean](true) else E.tailcall(isOdd[M](n - 1))
    def isOdd[M[_, _]](n: Int)(implicit E: Effects[M]): M[Pure, Boolean] =
      if (n == 0) E.pure[Pure, Boolean](false) else E.tailcall(isEven[M](n - 1))
    assertEquals(Effects[Eager.Rep].runWith(isEven[Eager.Rep](1000000)), true)
    assertEquals(Effects[Eager.Rep].runWith(isOdd[Eager.Rep](1000000)), false)
  }

  test("operations still suspend; toFree normalizes at any point") {
    val E = Effects[Eager.Rep]
    val m = E.flatMap(E.perform[Produce, Int](Emit(20)))((x: Int) => E.pure[Produce, Int](x + 22))
    assert((m: Any).isInstanceOf[Free[_, _]])
    assertEquals(Eager.toFree(m).runWith, 42)
    assertEquals(E.runWith(m), 42)
  }

  test("foldCont gives the operations their meaning, the same under both encodings") {
    val h = Interpr.of[Produce, Int]
    assertEquals(Effects[Free].foldCont(prog[Free])(h) / identity, 3)
    assertEquals(Effects[Eager.Rep].foldCont(prog[Eager.Rep])(h) / identity, 3)
  }
}
