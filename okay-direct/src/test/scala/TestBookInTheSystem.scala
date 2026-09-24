package okay

import okay.Direct.*
import okay.Row.at
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 17, COMPILED (docs/continuations/17-in-the-effect-system.md).
 *
 * One question decides what a capture can see: is the handler INSIDE
 * the delimiter or OUTSIDE it? The two orders give different answers
 * to the same question -- and the compiler will not let you confuse
 * them, because the order is visible in the PROMPT'S TYPE.
 */
class TestBookInTheSystem extends munit.FunSuite {

  type P = okay.Pure
  type Row = Delim + State % Int

  // ---- ORDER ONE: the handler is OUTSIDE the delimiter.
  // The delimiter's answer type is Int, so a capture answers Int.

  def outside(using Delim.Prompted[Int]): Int ! Row = direct:
    val x = !Delim.shift[Int, Int, State % Int](k => direct { !k(1) + !k(10) })
    !State.modify[Int](_ + x).at[Row]

  test("handler OUTSIDE: the branches share one running total") {
    val prog: Int ! State % Int = Delim.delimited[Int, State % Int](outside)
    val (s, a) = State.run[Int, Int](0)(prog)
    assertEquals(s, 11, "0 +1 = 1, then 1 +10 = 11: one timeline")
    assertEquals(a, 12, "k(1) answered 1, k(10) answered 11 -- it SAW the first write")
  }

  // ---- ORDER TWO: the handler is INSIDE the delimiter.
  // Now the delimiter's answer type is the HANDLER'S result, (Int, Int),
  // so the capture answers that. This is not the same program with a
  // moved bracket: the body had to be rewritten, and the compiler
  // insisted on it.

  def inside(using Delim.Prompted[(Int, Int)]): Int ! Row = direct:
    val x = !Delim.shift[(Int, Int), Int, State % Int]: k =>
      direct:
        val (s1, a1) = !k(1)
        val (s2, a2) = !k(10)
        (s1 + s2, a1 + a2)
    !State.modify[Int](_ + x).at[Row]

  test("handler INSIDE: each branch re-runs the handler from its own zero") {
    val prog: (Int, Int) ! P =
      Delim.delimited[(Int, Int), P](State.handle[Int](0)(inside))
    val (s, a) = !.run(prog)
    assertEquals(s, 11, "1 and 10, each from a FRESH 0, then summed by the capture")
    assertEquals(a, 11, "the branches never saw each other")
  }

  test("the difference is not the arithmetic: it is what the second branch SAW") {
    // Outside, branch two starts from 1 (what branch one wrote).
    // Inside, branch two starts from 0. Same body shape, same
    // modify, different world -- decided only by handler order.
    val out = State.run[Int, Int](0)(Delim.delimited[Int, State % Int](outside))
    val in = !.run(Delim.delimited[(Int, Int), P](State.handle[Int](0)(inside)))
    assertEquals(out._2, 12, "outside: 1 + 11")
    assertEquals(in._2, 11, "inside: 1 + 10")
    assertNotEquals(out._2, in._2,
      "if these ever agree, the experiment stopped distinguishing the orders")
  }
}
