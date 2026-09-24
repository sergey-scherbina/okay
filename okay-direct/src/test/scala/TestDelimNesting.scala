package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * ONE MACHINE, MANY DELIMITERS (delim-nesting, 2026-09-17). The four
 * patterns were each written to run their own machine, which made
 * them individually correct and jointly useless: `resumable` around a
 * `collect` put a SECOND `Delim` in the row, and the inner machine
 * claimed the outer machine's `pause` — a runtime `NoPrompt` for a
 * shape that reads as ordinary code.
 *
 * `scope` / `collecting` / `pausing` install a delimiter and leave
 * the machine alone, so the outermost combinator is the only one that
 * runs. These tests are the difference, both directions.
 */
class TestDelimNesting extends munit.FunSuite {

  type P = okay.Pure

  // ---- a producer that PAUSES in the middle of producing

  def half(using Delim.Asking[String, Int, List[Int], Delim + P]): List[Int] ! Delim + P =
    Delim.collecting[Int, P]:
      direct:
        !Delim.emit(1)
        val more = !Delim.pause("more?")     // crosses the collect's delimiter
        !Delim.emit(more)
        !Delim.emit(3)

  test("a pause crosses an intervening collect, and the list survives it") {
    val start = !.run(Delim.resumable[String, Int, List[Int], P](half))
    assertEquals(start.asking, Some("more?"))
    assertEquals(!.run(Delim.drive(start)(_ => okay.pure(2))), List(1, 2, 3))
    // it is a value: the same pause, answered differently
    assertEquals(!.run(Delim.drive(start)(_ => okay.pure(7))), List(1, 7, 3))
  }

  test("the same dialogue replays from its journal, producer and all") {
    val back = !.run(Delim.replay[String, Int, List[Int], P](half)(List(5)))
    assertEquals(back.finished, Some(List(1, 5, 3)))
  }

  test("THE OLD SHAPE is now a COMPILE error, and the message names the fix") {
    // `collect` (not `collecting`) runs its own machine, and its row
    // would be `Delim + (Delim + P)` — two Delim in one row, the
    // inner machine claiming the outer machine's pause. It used to
    // typecheck and throw NoPrompt; delim-safety stage 0 refuses it.
    val e = compileErrors("""
      okay.Delim.collect[Int, okay.Delim + okay.Pure](okay.Direct.direct {
        !okay.Delim.emit(1)
      })""")
    assert(e.nonEmpty, "the second machine compiled")
    assert(e.contains("collecting"), s"the message does not name the fix: $e")
  }

  // ---- a capture crossing a NESTED SCOPE, through the named door

  test("scope: WHICH delimiter a capture names decides how much it skips") {
    // one shape, three answers, and the only difference is the
    // evidence the capture names and whether it invokes k
    def prog(f: Delim.Prompted[Int] ?=> Delim.Prompted[Int] ?=> Int ! Delim + P): Int =
      !.run(Delim.delimited[Int, P]: (outer: Delim.Prompted[Int]) ?=>
        direct:
          val inner = !Delim.scope[Int, P]: (in: Delim.Prompted[Int]) ?=>
            direct:
              1 + !f(using outer)(using in)
          inner + 1000)

    // invoking k: the OUTER capture holds BOTH tails — the inner
    // scope's `+ 1` and `+ 1000`, and the whole thing re-runs
    assertEquals(prog(o ?=> _ ?=> Delim.shift[Int, Int, P](using o)(k => k(5))), 1006)
    // dropping it, named OUTER: both tails go, the block answers 5
    assertEquals(prog(o ?=> _ ?=> Delim.shift[Int, Int, P](using o)(_ => okay.pure(5))), 5)
    // dropping it, named INNER: only the inner tail goes — the outer
    // `+ 1000` still runs, because it was never captured
    assertEquals(prog(_ ?=> i ?=> Delim.shift[Int, Int, P](using i)(_ => okay.pure(5))), 1005)
  }

  test("scope: the inner evidence is the nearest one, and stays inside") {
    val prog: Int ! P = Delim.delimited[Int, P]:
      direct:
        val inner = !Delim.scope[Int, P]:
          direct:
            1 + !Delim.shift[Int](k => k(5))     // the nearest: the inner scope
        inner + 1000
    assertEquals(!.run(prog), 1006)
  }

  test("exit crosses a nested producer: out of the whole block, with an answer") {
    def run(stopAt: Int): String ! P = Delim.delimited[String, P]: (out: Delim.Prompted[String]) ?=>
      direct:
        val xs = !Delim.collecting[Int, P]:
          direct:
            var i = 0
            while i < 5 do
              if i == stopAt then !Delim.exit(using out)(s"stopped at $i")
              !Delim.emit(i)
              i += 1
        s"collected $xs"
    assertEquals(!.run(run(3)), "stopped at 3")
    assertEquals(!.run(run(9)), "collected List(0, 1, 2, 3, 4)")
  }

  test("delimited still runs the machine: the outermost form is unchanged") {
    val prog: Int ! P = Delim.delimited[Int, P]:
      direct:
        1 + !Delim.shift[Int](k => k(5))
    assertEquals(!.run(prog), 6)
  }
}
