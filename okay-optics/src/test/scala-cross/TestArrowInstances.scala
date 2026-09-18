package okay

import okay.given
import okay.laws.{ArrowLaws, ArrowLawsSuite}

/**
 * THE TWO CARRIERS `optics-arrow-instances` ADDS, AGAINST THE SHARED
 * SUITE — three lines each, which is what `arrow-laws` was written to
 * make possible (specs/arrows-plan.md, lane 4).
 *
 * `Optic.Arrow` had ONE instance before this: `Mealy` in okay-lex.
 * The comment beside the class says arrows and optics are written on
 * one `Profunctor` here; with one instance that was a remark about
 * the literature. With three it is a fact about the tree.
 */

/** plain functions — the observation is one input, since a function
 * has no state a sequence could reveal */
class TestFunctionArrowLaws extends ArrowLawsSuite[Function1]:
  def laws: ArrowLaws[Function1] =
    ArrowLaws(summon[Optic.Arrow[Function1] & Optic.Traversing[Function1]],
      (i: Int) => i * 2 + 1,
      new ArrowLaws.Observe[Function1]:
        type Out[Y] = Seq[Y]
        def run[X, Y](p: X => Y, xs: Seq[X]): Seq[Y] = xs.map(p))

/** the Kleisli, at `Option` — a carrier with a real `flatMap` whose
 * short circuit the laws must survive */
class TestKleisliArrowLaws extends ArrowLawsSuite[[X, Y] =>> Optic.Star[Option, X, Y]]:
  def laws: ArrowLaws[[X, Y] =>> Optic.Star[Option, X, Y]] =
    ArrowLaws(Optic.kleisliArrow[Option],
      Optic.Star[Option, Int, Int](i => Some(i * 2 + 1)),
      new ArrowLaws.Observe[[X, Y] =>> Optic.Star[Option, X, Y]]:
        type Out[Y] = Seq[Option[Y]]
        def run[X, Y](p: Optic.Star[Option, X, Y], xs: Seq[X]): Seq[Option[Y]] =
          xs.map(p.run))

/**
 * What the instances are FOR, as call sites rather than as laws: the
 * two combinators that did not exist on a plain function before.
 */
class TestArrowUse extends munit.FunSuite {

  private val A = summon[Optic.Arrow[Function1] & Optic.Traversing[Function1]]

  test("fanout: one input, both functions, both answers") {
    val both = A.fanout((i: Int) => i + 1, (i: Int) => i * 10)
    assertEquals(both(4), (5, 40))
  }

  test("split: a pair, each half through its own function") {
    val pair = A.split((i: Int) => i + 1, (s: String) => s.length)
    assertEquals(pair((4, "abc")), (5, 3))
  }

  test("the optics still resolve at Function1 — one instance, both roles") {
    // the ambiguity this lane had to avoid: `Arrow` extends `Strong`,
    // so a SECOND given would have made every `set` in the library
    // ambiguous. One widened instance keeps both roles.
    final case class P(name: String, age: Int)
    val age = Lens[P](_.age)
    assertEquals(age.set(41)(P("ada", 36)), P("ada", 41))
    assertEquals(age.get(P("ada", 36)), 36)
  }

  test("the Kleisli arrow composes effects, and its short circuit survives") {
    val K = Optic.kleisliArrow[Option]
    val half = Optic.Star[Option, Int, Int](i => Option.when(i % 2 == 0)(i / 2))
    val twice = K.compose(half, half)
    assertEquals(twice.run(8), Some(2))   // 8 -> 4 -> 2
    assertEquals(twice.run(4), Some(1))   // 4 -> 2 -> 1
    assertEquals(twice.run(6), None)      // 6 -> 3, odd, and the second step stops
  }
}
