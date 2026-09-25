package okay2.lex

import okay2.Optic

/**
 * okay-lex's TestMealy: a scanner is a Mealy machine, and a Mealy machine
 * is an arrow. The Scala 3 core states the arrow laws through a shared
 * `ArrowLawsSuite` okay2 has not ported; the laws a machine can break are
 * stated here directly, observed OVER A SEQUENCE (a single step cannot
 * show a state), on a STATEFUL sample arrow.
 */
class TestMealy extends munit.FunSuite {

  val A: Optic.Arrow[Mealy] with Optic.Choice[Mealy] = Mealy.mealyArrow

  val chars = "{\"a\": [1, 2],\n \"b\": true}"

  /** a running sum: the state is the whole point */
  def running(n: Int): Mealy[Int, Int] = Mealy(i => { val m = n + i; (running(m), m) })
  val xs: Seq[Int] = Seq(3, 1, 4, 1, 5, 9, 2, 6)
  def obs[X, Y](m: Mealy[X, Y], in: Seq[X]): Vector[Y] = Mealy.runAll(m, in)

  test("laws: identity on both sides of compose, associativity, arr of a composition") {
    val f = running(0)
    val g = A.arr[Int, Int](_ * 2)
    val h = running(100)
    assertEquals(obs(A.compose(A.id[Int], f), xs), obs(f, xs))
    assertEquals(obs(A.compose(f, A.id[Int]), xs), obs(f, xs))
    assertEquals(obs(A.compose(h, A.compose(g, f)), xs), obs(A.compose(A.compose(h, g), f), xs))
    assertEquals(obs(A.arr[Int, Int](x => (x + 1) * 2), xs), obs(A.compose(A.arr[Int, Int](_ * 2), A.arr[Int, Int](_ + 1)), xs))
  }

  test("laws: first threads the state through the first component, right skips the Lefts") {
    val pairs = xs.map(x => (x, x.toString))
    assertEquals(obs(A.first[Int, Int, String](running(0)), pairs), obs(running(0), xs).zip(pairs.map(_._2)))
    val eithers: Seq[Either[String, Int]] = Seq(Right(1), Left("a"), Right(2), Left("b"), Right(3))
    assertEquals(obs(A.right[Int, Int, String](running(0)), eithers),
      Vector(Right(1), Left("a"), Right(3), Left("b"), Right(6)))
  }

  test("the door: the machine emits exactly what the scanner's own driver does") {
    val viaMachine = Mealy.runString(Mealy.ofScan(Json.scan), chars).flatten
    assertEquals(viaMachine, Scan.all(Json.scan)(chars).tokens)
  }

  test("a scanner followed by a token step, in one pass: trivia dropped") {
    val syntax = A.compose(A.arr[Vector[Token[Json.K]], Vector[Token[Json.K]]](_.filter(_.channel == Channel.Syntax)), Mealy.ofScan(Json.scan))
    val got = Mealy.runString(syntax, chars).flatten
    assertEquals(got, Scan.all(Json.scan)(chars).tokens.filter(_.channel == Channel.Syntax))
    assert(got.nonEmpty)
  }

  test("fold consumes a composed machine without materialising it") {
    val syntax = A.compose(A.arr[Vector[Token[Json.K]], Int](_.count(_.channel == Channel.Syntax)), Mealy.ofScan(Json.scan))
    val counted = Mealy.fold(syntax, chars)(0)(_ + _)
    assertEquals(counted, Scan.all(Json.scan)(chars).tokens.count(_.channel == Channel.Syntax))
    assertEquals(counted, Mealy.runString(syntax, chars).sum)
  }

  test("two machines over ONE input: lex and count newlines side by side") {
    def counting(n: Int): Mealy[Char, Int] = Mealy(c => { val m = if (c == '\n') n + 1 else n; (counting(m), m) })
    val out = Mealy.runString(A.fanout(Mealy.ofScan(Json.scan), counting(0)), chars)
    assertEquals(out.flatMap(_._1), Scan.all(Json.scan)(chars).tokens)
    assertEquals(out.last._2, chars.count(_ == '\n'))
  }
}
