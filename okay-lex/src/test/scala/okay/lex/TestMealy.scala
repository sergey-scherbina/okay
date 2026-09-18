package okay.lex

import okay.Optic
import okay.laws.{ArrowLaws, ArrowLawsSuite}

/**
 * THE LAWS, from the shared suite (specs/arrows-plan.md, Decision 2).
 *
 * They used to be stated here, and this file is where the reusable
 * shape came from: a machine's equality can only be observed OVER A
 * SEQUENCE of inputs, because a single step cannot show a state.
 * `ArrowLaws` takes exactly that as its `Observe`, so `Mealy` is now
 * three lines and `Function1`, the Kleisli and `Proc` will each be
 * three of their own instead of copies of these.
 *
 * The sample arrow is STATEFUL on purpose: a machine whose every
 * value is an `arr` satisfies laws that a real one can break.
 */
class TestMealyLaws extends ArrowLawsSuite[Mealy] {
  def laws: ArrowLaws[Mealy] = ArrowLaws(Mealy.mealyArrow, TestMealyLaws.running(0), TestMealyLaws.observe)
}

object TestMealyLaws {
  /** a running sum: the state is the whole point */
  def running(n: Int): Mealy[Int, Int] =
    Mealy(i => { val m = n + i; (running(m), m) })

  def observe: ArrowLaws.Observe[Mealy] = new ArrowLaws.Observe[Mealy] {
    type Out[Y] = Vector[Y]
    def run[X, Y](p: Mealy[X, Y], xs: Seq[X]): Vector[Y] = Mealy.runAll(p, xs)
  }
}

/**
 * A scanner is a Mealy machine, and a Mealy machine is an arrow
 * (specs/optics.md stage 5). What is left here is what the laws do
 * not say: that the machine agrees with the scanner's own driver, and
 * that composing them costs nothing a consumer can see.
 */
class TestMealy extends munit.FunSuite {

  val A: Optic.Arrow[Mealy] & Optic.Choice[Mealy] = Mealy.mealyArrow

  val chars = "{\"a\": [1, 2],\n \"b\": true}"





  test("the door: the machine emits exactly what the scanner's own driver does") {
    // the input ends on a structural character, so `flush` adds nothing
    val viaMachine = Mealy.runString(Mealy.ofScan(Json.scan), chars).flatten
    val viaDriver = Scan.all(Json.scan)(chars).tokens
    assertEquals(viaMachine, viaDriver)
  }

  test("a scanner followed by a token step, in one pass: trivia dropped") {
    val syntax = A.compose(
      A.arr[Vector[Token[Json.K]], Vector[Token[Json.K]]](_.filter(_.channel == Channel.Syntax)),
      Mealy.ofScan(Json.scan))
    val got = Mealy.runString(syntax, chars).flatten
    assertEquals(got, Scan.all(Json.scan)(chars).tokens.filter(_.channel == Channel.Syntax))
    assert(got.nonEmpty)
  }

  test("fold consumes a composed machine without materialising it") {
    val syntax = A.compose(
      A.arr[Vector[Token[Json.K]], Int](_.count(_.channel == Channel.Syntax)),
      Mealy.ofScan(Json.scan))
    val counted = Mealy.fold(syntax, chars)(0)(_ + _)
    assertEquals(counted, Scan.all(Json.scan)(chars).tokens.count(_.channel == Channel.Syntax))
    // and it agrees with the materialising driver, which is the law
    assertEquals(counted, Mealy.runString(syntax, chars).sum)
  }

  test("two machines over ONE input: lex and count newlines side by side") {
    def counting(n: Int): Mealy[Char, Int] =
      Mealy(c => { val m = if c == '\n' then n + 1 else n; (counting(m), m) })
    val both = A.fanout(Mealy.ofScan(Json.scan), counting(0))
    val out = Mealy.runString(both, chars)
    assertEquals(out.flatMap(_._1), Scan.all(Json.scan)(chars).tokens)
    assertEquals(out.last._2, chars.count(_ == '\n'))
  }
}
