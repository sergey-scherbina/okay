package okay.cats

import _root_.cats.data.NonEmptyList
import _root_.cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.{Arbitrary, Gen}

/**
 * Semigroup, Monoid, Group across cats-kernel and okay
 * (specs/cats-kernel-bridge.md): each test holds only ONE side's
 * combiner and needs the other's.
 */
/** the default import alone: either side's combiner serves both Validateds */
class TestCombine extends munit.FunSuite with okay.testkit.Munit.Diagnosed {
  import _root_.cats.syntax.all.*

  final case class Peak(n: Int)
  given okay.freer.Semigroup[Peak] with
    def combine(x: Peak, y: Peak): Peak = Peak(x.n max y.n)

  test("okay.freer.Validated under cats' traverse, with only a cats Semigroup") {
    val out = List(1, 2, 3).traverse(i =>
      if i == 2 then okay.freer.Validated.valid[NonEmptyList[String], Int](i)
      else okay.freer.Validated.invalid[NonEmptyList[String], Int](NonEmptyList.one(s"$i")))
    assertEquals(out, okay.freer.Validated.invalid(NonEmptyList.of("1", "3")))
  }

  test("cats' Validated under okay's Selective, with only an okay Semigroup") {
    val out = okay.traverse(Seq(1, 2, 3))(i =>
      if i == 2 then _root_.cats.data.Validated.valid[Peak, Int](i)
      else _root_.cats.data.Validated.invalid[Peak, Int](Peak(i)))
    assertEquals(out, _root_.cats.data.Validated.invalid(Peak(3)))
  }

  test("a type both sides combine (String): okay's first, no ambiguity") {
    import okay.freer.given
    val out = List(1, 2).traverse(i => okay.freer.Validated.invalid[String, Int](s"<$i>"))
    assertEquals(out, okay.freer.Validated.invalid("<1><2>"))
  }
}

class TestFromCatsKernel extends munit.FunSuite with okay.testkit.Munit.Diagnosed {
  import FromCatsKernel.given

  test("okay.freer.Validated accumulates with only a cats Semigroup (NonEmptyList)") {
    val check = (i: Int) =>
      if i % 2 == 1 then okay.freer.Validated.invalid[NonEmptyList[String], Int](NonEmptyList.one(s"odd $i"))
      else okay.freer.Validated.valid[NonEmptyList[String], Int](i)
    val out = okay.traverse(Seq(1, 2, 3))(check)
    note(s"traverse answered $out")
    assertEquals(out, okay.freer.Validated.invalid(NonEmptyList.of("odd 1", "odd 3")))
  }

  test("cats' Group arrives as okay's, inverse kept, for a type okay has none for") {
    val G = summon[okay.freer.Group[(Int, Int)]]
    assertEquals(G.combine((5, 2), G.inverse((5, 2))), G.empty)
  }
}

class TestToCatsKernel extends munit.ScalaCheckSuite with okay.testkit.Munit.Diagnosed {
  import ToCatsKernel.given
  import _root_.cats.syntax.all.*

  /** a combiner only okay defines: the maximum, with a floor */
  final case class Peak(n: Int)
  given okay.freer.Monoid[Peak] with
    def empty: Peak = Peak(Int.MinValue)
    def combine(x: Peak, y: Peak): Peak = Peak(x.n max y.n)

  test("cats' combineAll over an okay Monoid") {
    assertEquals(List(Peak(3), Peak(9), Peak(4)).combineAll, Peak(9))
  }

  test("cats' own Validated accumulates (its Applicative) with only an okay Semigroup") {
    val a = _root_.cats.data.Validated.invalid[Peak, Int](Peak(2))
    val b = _root_.cats.data.Validated.invalid[Peak, Int](Peak(7))
    assertEquals((a, b).mapN(_ + _), _root_.cats.data.Validated.invalid(Peak(7)))
  }

  given Arbitrary[Peak] = Arbitrary(Gen.choose(-100, 100).map(Peak(_)))
  given _root_.cats.kernel.Eq[Peak] = _root_.cats.kernel.Eq.fromUniversalEquals

  for (id, prop) <- MonoidTests[Peak].monoid.all.properties do property(s"cats Monoid from okay's: $id")(prop)
}
