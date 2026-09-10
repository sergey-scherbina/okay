package okay.lex

import okay.Optic

/**
 * A scanner is a Mealy machine, and a Mealy machine is an arrow
 * (specs/optics.md stage 5). The laws are observed the only way a
 * machine's equality can be observed: over an input.
 */
class TestMealy extends munit.FunSuite {

  val A: Optic.Arrow[Mealy] & Optic.Choice[Mealy] = Mealy.mealyArrow

  val chars = "{\"a\": [1, 2],\n \"b\": true}"

  def same[B](x: Mealy[Char, B], y: Mealy[Char, B], clue: String): Unit =
    assertEquals(Mealy.runString(x, chars), Mealy.runString(y, chars), clue)

  test("arr is functorial: arr(f) then arr(g) IS arr(f then g)") {
    val f = (c: Char) => c.toInt
    val g = (i: Int) => i * 2
    same(A.compose(A.arr(g), A.arr(f)), A.arr(f.andThen(g)), "arr(g) . arr(f)")
  }

  test("identity is an identity on both sides") {
    val m = Mealy.ofScan(Json.scan)
    same(A.compose(A.id[Vector[Token[Json.K]]], m), m, "id . m")
    same(A.compose(m, A.id[Char]), m, "m . id")
  }

  test("first respects arr: first(arr f) IS arr(f on the left of a pair)") {
    val f = (c: Char) => c.isDigit
    val left = A.first[Char, Boolean, Int](A.arr(f))
    val right = A.arr[(Char, Int), (Boolean, Int)]((ci: (Char, Int)) => (f(ci._1), ci._2))
    val input = chars.toVector.zipWithIndex
    assertEquals(Mealy.runAll(left, input), Mealy.runAll(right, input))
  }

  test("composition is associative, over the same input") {
    val m = Mealy.ofScan(Json.scan)
    val f = A.arr[Vector[Token[Json.K]], Int](_.length)
    val g = A.arr[Int, String](_.toString)
    same(A.compose(A.compose(g, f), m), A.compose(g, A.compose(f, m)), "associativity")
  }

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

  test("two machines over ONE input: lex and count newlines side by side") {
    def counting(n: Int): Mealy[Char, Int] =
      Mealy(c => { val m = if c == '\n' then n + 1 else n; (counting(m), m) })
    val both = A.fanout(Mealy.ofScan(Json.scan), counting(0))
    val out = Mealy.runString(both, chars)
    assertEquals(out.flatMap(_._1), Scan.all(Json.scan)(chars).tokens)
    assertEquals(out.last._2, chars.count(_ == '\n'))
  }
}
