package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 13, COMPILED (docs/continuations/13-multi-shot.md).
 *
 * What it means that a continuation is a value you may call more than
 * once -- including the two things that go wrong when you do, both
 * asserted here rather than warned about.
 */
class TestBookMultiShot extends munit.FunSuite {

  type Row = Delim + Pure

  // ---- the plain fact

  test("calling the rest twice gives two answers from one past") {
    val r = Delim.delimited[List[Int], Pure]:
      direct:
        val x = !Delim.shift[Int](k => k(1).flatMap(a => k(2).map(b => a ++ b)))
        List(x * 10)
    assertEquals(!.run(r), List(10, 20))
  }

  test("nondeterminism falls out of it: every combination, no backtracking code") {
    val r = Delim.delimited[List[(Int, String)], Pure]:
      direct:
        val n = !Delim.shift[Int](k => k(1).flatMap(a => k(2).map(b => a ++ b)))
        val s = !Delim.shift[String](k => k("a").flatMap(a => k("b").map(b => a ++ b)))
        List((n, s))
    assertEquals(!.run(r), List((1, "a"), (1, "b"), (2, "a"), (2, "b")))
  }

  // ---- WHAT GOES WRONG ONE: effects in the captured part run per call

  test("an effect AFTER the capture point happens once per resumption") {
    var log = List.empty[String]
    val r = Delim.delimited[List[Int], Pure]:
      direct:
        val x = !Delim.shift[Int](k => k(1).flatMap(a => k(2).map(b => a ++ b)))
        log = log :+ s"ran with $x"      // this is IN the continuation
        List(x)
    assertEquals(!.run(r), List(1, 2))
    assertEquals(log, List("ran with 1", "ran with 2"),
      "the side effect did not happen once per resumption")
  }

  test("an effect BEFORE the capture point happens once, whatever k does") {
    var opened = 0
    val r = Delim.delimited[List[Int], Pure]:
      direct:
        opened += 1                      // this is NOT in the continuation
        val x = !Delim.shift[Int](k => k(1).flatMap(a => k(2).map(b => a ++ b)))
        List(x)
    assertEquals(!.run(r), List(1, 2))
    assertEquals(opened, 1, "the part before the capture ran more than once")
  }

  // ---- WHAT GOES WRONG TWO: a `var` written in the continuation

  test("a var written after the capture point is written by EVERY resumption") {
    var last = 0
    val r = Delim.delimited[List[Int], Pure]:
      direct:
        val x = !Delim.shift[Int](k => k(1).flatMap(a => k(2).map(b => a ++ b)))
        last = x
        List(x)
    assertEquals(!.run(r), List(1, 2))
    // not "the answer for this branch" -- the last branch to run wins
    assertEquals(last, 2)
  }

  // ---- and the one that is NOT a problem: zero calls

  test("not calling it at all is an early exit, and nothing after runs") {
    var ran = false
    val r = Delim.delimited[Int, Pure]:
      direct:
        val _ = !Delim.shift[Int](_ => okay.pure(99))   // k dropped
        ran = true
        0
    assertEquals(!.run(r), 99)
    assert(!ran, "the discarded continuation ran anyway")
  }
}
