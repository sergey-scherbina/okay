package okay

/** The opt-in eager encoding: kyo-class speed, kyo's stated hazards. */
class TestEager extends munit.FunSuite {

  import Eager.given

  def prog[M[_[+_], _]](using E: Effects[M]): M[Produce, Int] =
    E.perform[Produce, Int](1).flatMap(x => E.perform[Produce, Int](x + 1).map(y => x + y))

  test("the tagless encodings agree: Eager, Free, Eff") {
    assertEquals(prog[Eager].runWith, 3)
    assertEquals(prog[Free].runWith, 3)
    assertEquals(prog[Eff].runWith, 3)
  }

  test("eagerness is real: a pure bind chain evaluates at CONSTRUCTION") {
    val E = summon[Effects[Eager]]
    var steps = 0
    val built = (1 to 1000).foldLeft(E.pure[Produce, Int](0)): (m, _) =>
      E.flatMap(m)(x => { steps += 1; E.pure(x + 1) })
    assertEquals(steps, 1000)              // the work already happened
    assertEquals(built.runWith, 1000)      // running is O(1): it IS the value
    assert(built.isInstanceOf[Integer])    // literally the unboxed result
  }

  test("the same chain under Free stays a value until run (the contrast)") {
    val E = summon[Effects[Free]]
    var steps = 0
    val built = (1 to 1000).foldLeft(E.pure[Produce, Int](0)): (m, _) =>
      E.flatMap(m)(x => { steps += 1; E.pure(x + 1) })
    assertEquals(steps, 0)                 // nothing ran at construction
    assertEquals(built.runWith, 1000)
    assertEquals(steps, 1000)
  }

  test("operations still suspend; toFree normalizes at any point") {
    val E = summon[Effects[Eager]]
    val m = E.flatMap(E.perform[Produce, Int](20))(x => E.pure(x + 22))
    assert(m.isInstanceOf[Free[?, ?]])
    assertEquals(Eager.toFree(m).runWith, 42)
    assertEquals(m.runWith, 42)
  }
}
