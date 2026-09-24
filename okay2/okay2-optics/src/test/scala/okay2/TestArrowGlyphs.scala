package okay2

import okay2.Optic._
import okay2.Optic.arrows._

/** THE GLYPHS, AGAINST THE METHODS THEY SPELL — the Scala 3 core's
 * TestArrowGlyphs, on `Function1` whose equality is observable */
class TestArrowGlyphs extends munit.FunSuite {

  private val A = implicitly[Arrow[Function1] with Traversing[Function1]]
  private val f: Int => Int = _ + 1
  private val g: Int => Int = _ * 3
  private val h: String => Int = _.length

  private val ints = Seq(0, 1, 2, -1, 7)
  private def same[X, Y](l: X => Y, r: X => Y, xs: Seq[X]): Unit = assertEquals(xs.map(l), xs.map(r))

  test(">>> is compose, left to right — and a plain function is an arrow") {
    same(f >>> g, A.compose(g, f), ints)
    assertEquals((f >>> g)(4), 15)
  }

  test("<<< is the same composition, written the other way round") {
    same(g <<< f, f >>> g, ints)
  }

  test("*** is split and &&& is fanout") {
    assertEquals((f *** h)((4, "abc")), (5, 3))
    assertEquals((f *** h)((4, "abc")), A.split(f, h)((4, "abc")))
    same(f &&& g, A.fanout(f, g), ints)
    assertEquals((f &&& g)(4), (5, 12))
  }

  test("`left` is the mirror of `right`, and agrees with it through a swap") {
    val swapIn: Either[String, Int] => Either[Int, String] = _.fold(Right(_), Left(_))
    val swapOut: Either[Int, Int] => Either[Int, Int] = _.fold(Right(_), Left(_))
    val viaLeft = A.left[String, Int, Int](h)
    val viaRight = (e: Either[String, Int]) => swapOut(A.right[String, Int, Int](h)(swapIn(e)))
    assertEquals(viaLeft(Left("abcd")), viaRight(Left("abcd")))
    assertEquals(viaLeft(Right(9)), viaRight(Right(9)))
    assertEquals(viaLeft(Left("abcd")), Left(4))
  }

  test("+++ is a sum, ||| is fanin") {
    val sum = f +++ h
    assertEquals(sum(Left(4)), Left(5))
    assertEquals(sum(Right("abcd")), Right(4))
    val fan = f ||| h
    assertEquals(fan(Left(4)), 5)
    assertEquals(fan(Right("abcd")), 4)
  }

  test("the glyphs compose with each other") {
    val both = (f &&& g) >>> A.arr((p: (Int, Int)) => p._1 + p._2)
    same(both, (i: Int) => (i + 1) + (i * 3), ints)
  }

  test("`>=>` is the KLEISLI composition, and it lives beside `>>>` without a fight") {
    val half: Int => Option[Int] = i => Option.when(i % 2 == 0)(i / 2)
    val dec: Int => Option[Int] = i => Option.when(i > 0)(i - 1)
    val k = half >=> dec
    assertEquals(k(8), Some(3))
    assertEquals(k(3), None)
  }

  test("the Kleisli arrow, by name: Star over Option") {
    val K = kleisliArrow[Option]
    val half = Star[Option, Int, Int](i => Option.when(i % 2 == 0)(i / 2))
    assertEquals(K.compose(half, half).run(8), Some(2))
    assertEquals(K.fanout(half, K.arr((i: Int) => i + 1)).run(4), Some((2, 5)))
  }
}
