package okay

import okay.given
import okay.Optic.arrows.*

/**
 * THE GLYPHS, AGAINST THE METHODS THEY SPELL.
 *
 * Every test here is a PAIR: the glyph and the named method, asserted
 * equal on the same inputs. A glyph that quietly means something else
 * than the method it claims to spell is worse than no glyph, and the
 * only way to know is to run both.
 *
 * The carrier is `Function1`, because its equality is observable
 * directly; `Mealy` and `Proc` have their own law suites and this
 * file is about the SPELLING, not about the laws.
 */
class TestArrowGlyphs extends munit.FunSuite {

  private val A = summon[Optic.Arrow[Function1] & Optic.Traversing[Function1]]
  private val f: Int => Int = _ + 1
  private val g: Int => Int = _ * 3
  private val h: String => Int = _.length

  private val ints = Seq(0, 1, 2, -1, 7)
  private def same[X, Y](l: X => Y, r: X => Y, xs: Seq[X]): Unit =
    assertEquals(xs.map(l), xs.map(r))

  test(">>> is compose, left to right — and a plain function is an arrow") {
    same(f >>> g, A.compose(g, f), ints)
    assertEquals((f >>> g)(4), 15)          // (4+1)*3
  }

  test("<<< is the same composition, written the other way round") {
    same(g <<< f, f >>> g, ints)
    assertEquals((g <<< f)(4), 15)
  }

  test("*** is split: a pair, each half through its own arrow") {
    val pair = f *** h
    assertEquals(pair((4, "abc")), (5, 3))
    assertEquals(pair((4, "abc")), A.split(f, h)((4, "abc")))
  }

  test("&&& is fanout: one input, both arrows") {
    same(f &&& g, A.fanout(f, g), ints)
    assertEquals((f &&& g)(4), (5, 12))
  }

  test("`left` is the mirror of `right`, and agrees with it through a swap") {
    // the operator's question: the literature's ArrowChoice primitive
    // is `left`. Here `right` is, because a prism needs it — so
    // `left` is derived, and this is the test that they are mirrors.
    val C = summon[Optic.Traversing[Function1]]
    val swapIn: Either[String, Int] => Either[Int, String] = _.fold(Right(_), Left(_))
    val swapOut: Either[Int, Int] => Either[Int, Int] = _.fold(Right(_), Left(_))
    val viaLeft = C.left[String, Int, Int](h)
    val viaRight = (e: Either[String, Int]) => swapOut(C.right[String, Int, Int](h)(swapIn(e)))
    assertEquals(viaLeft(Left("abcd")), viaRight(Left("abcd")))
    assertEquals(viaLeft(Right(9)), viaRight(Right(9)))
    assertEquals(viaLeft(Left("abcd")), Left(4))
    assertEquals(viaLeft(Right(9)), Right(9))
  }

  test("+++ is a sum, each side through its own arrow") {
    val sum = f +++ h
    assertEquals(sum(Left(4)), Left(5))
    assertEquals(sum(Right("abcd")), Right(4))
  }

  test("||| is fanin: both sides to one answer") {
    val fan = f ||| h
    assertEquals(fan(Left(4)), 5)
    assertEquals(fan(Right("abcd")), 4)
  }

  test("the glyphs compose with each other, which is the point of having them") {
    val both = (f &&& g) >>> A.arr((p: (Int, Int)) => p._1 + p._2)
    same(both, (i: Int) => (i + 1) + (i * 3), ints)
  }

  test("`>=>` is the KLEISLI composition, and it lives beside `>>>` without a fight") {
    // WHY THE TWO GLYPHS ARE DIFFERENT, and it is not taste. An
    // effectful function is ALSO a `P[A, B]` (with `B = Option[Int]`),
    // so while Kleisli held `>>>` the arrow `>>>` could not be added
    // beside it: the arrow extension won resolution and then failed
    // to typecheck, the next arrow having to start at `Option[Int]`.
    // The first cut of this file is where that showed. Each glyph now
    // has the name its own literature gives it.
    val half: Int => Option[Int] = i => Option.when(i % 2 == 0)(i / 2)
    val dec: Int => Option[Int] = i => Option.when(i > 0)(i - 1)
    val k = half >=> dec
    assertEquals(k(8), Some(3))
    assertEquals(k(2), Some(0))
    assertEquals(k(3), None)
  }
}
