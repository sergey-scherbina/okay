package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 19, COMPILED (docs/continuations/19-what-a-capture-does.md).
 *
 * The chapter's main suite is TestDelimLimits, which pins what a
 * capture does to state, resources, errors, `finally`, a second
 * machine and depth. This file adds the one case a reader asks about
 * first and that suite does not cover: an ordinary Scala `var`, which
 * is not an effect and therefore obeys none of the rules effects do.
 */
class TestBookCaptureAndTheRest extends munit.FunSuite {

  type P = okay.Pure

  test("a plain var is SHARED by every branch -- it is one heap cell") {
    // Contrast with chapter 17: State's behaviour depended on handler
    // order. A `var` has no handler, so there is no order to choose.
    // Both branches write the same cell, always.
    var seen = List.empty[Int]
    val prog: Int ! P = Delim.delimited[Int, P]:
      direct:
        val x = !Delim.shift[Int, Int, P](k => direct { !k(1) + !k(10) })
        seen = seen :+ x
        x
    assertEquals(!.run(prog), 11)
    assertEquals(seen, List(1, 10),
      "both branches appended to the same list: the var is not forked")
  }

  test("a var written BEFORE the capture is not restored when a branch re-runs") {
    // The continuation is re-entered, not the whole block. Everything
    // the block did before the capture point happened once.
    var before = 0
    var after = 0
    val prog: Int ! P = Delim.delimited[Int, P]:
      direct:
        before += 1
        val x = !Delim.shift[Int, Int, P](k => direct { !k(1) + !k(2) })
        after += 1
        x
    assertEquals(!.run(prog), 3)
    assertEquals(before, 1, "the prefix ran more than once")
    assertEquals(after, 2, "the suffix is the continuation: once per branch")
  }

  test("a loop counter survives a pause, because the continuation holds it") {
    // The reassuring case. A `var` inside the captured region is part
    // of the continuation's closure, so pausing and resuming keeps it.
    def counting(using Delim.Asking[Int, Int, Int, Delim + P]): Int ! Delim + P = direct:
      var acc = 0
      var i = 0
      while i < 3 do
        acc += !Delim.pause(i)
        i += 1
      acc
    val out = !.run(Delim.drive[Int, Int, Int, P](
      !.run(Delim.resumable[Int, Int, Int, P](counting)))(q => okay.pure(q * 10)))
    assertEquals(out, 30, "0*10 + 1*10 + 2*10")
  }
}
