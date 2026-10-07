package okay


import okay.freer.*
import okay.freer.given
import okay.Direct.*
import scala.language.implicitConversions

/**
 * ONE MACHINE, MANY DELIMITERS (delim-nesting, 2026-09-17). The four
 * patterns were each written to run their own machine, which made
 * them individually correct and jointly useless: `resumable` around a
 * `collect` put a SECOND `Shift` in the row, and the inner machine
 * claimed the outer machine's `pause` — a runtime `NoPrompt` for a
 * shape that reads as ordinary code.
 *
 * `scope` / `collecting` / `pausing` install a delimiter and leave
 * the machine alone, so the outermost combinator is the only one that
 * runs. Since shift-merge-guard the machine-starting spellings do the
 * same when the row says a machine runs (`Shift.Machine`).
 */
class TestDelimNesting extends munit.FunSuite {

  type P = okay.freer.Pure

  // ---- a producer that PAUSES in the middle of producing

  def half(using Shift.Asking[String, Int, List[Int], Shift % ? + P]): List[Int] ! Shift % ? + P =
    Shift.collecting[Int, P]:
      direct:
        !Shift.emit(1)
        val more = !Shift.pause("more?")     // crosses the collect's delimiter
        !Shift.emit(more)
        !Shift.emit(3)

  test("a pause crosses an intervening collect, and the list survives it") {
    val start = !.run(Shift.resumable[String, Int, List[Int], P](half))
    assertEquals(start.asking, Some("more?"))
    assertEquals(!.run(Shift.drive(start)(_ => okay.freer.pure(2))), List(1, 2, 3))
    // it is a value: the same pause, answered differently
    assertEquals(!.run(Shift.drive(start)(_ => okay.freer.pure(7))), List(1, 7, 3))
  }

  test("the same dialogue replays from its journal, producer and all") {
    val back = !.run(Shift.replay[String, Int, List[Int], P](half)(List(5)))
    assertEquals(back.finished, Some(List(1, 5, 3)))
  }

  /** THE OLD SHAPE: `collect`, not `collecting` — at this row a machine already runs, so it nests */
  def halfOld(using Shift.Asking[String, Int, List[Int], Shift % ? + P]): List[Int] ! Shift % ? + P =
    Shift.collect[Int, Shift % ? + P]:
      direct:
        !Shift.emit(1)
        val more = !Shift.pause("more?")
        !Shift.emit(more)
        !Shift.emit(3)

  test("THE OLD SHAPE now NESTS: collect at a Shift row stands on the running machine") {
    // its row is `Shift % ? + (Shift % ? + P)`: it used to typecheck and
    // throw NoPrompt (the inner machine claiming the outer pause), then
    // was a compile error (delim-safety stage 0); since
    // shift-merge-guard `Shift.Machine` reads the row and the door
    // pushes its delimiter on the machine outside, as `collecting` does
    val start = !.run(Shift.resumable[String, Int, List[Int], P](halfOld))
    assertEquals(start.asking, Some("more?"))
    assertEquals(!.run(Shift.drive(start)(_ => okay.freer.pure(2))), List(1, 2, 3))
    assertEquals(!.run(Shift.replay[String, Int, List[Int], P](halfOld)(List(5))).finished, Some(List(1, 5, 3)))
  }

  // ---- a capture crossing a NESTED SCOPE, through the named door

  test("scope: WHICH delimiter a capture names decides how much it skips") {
    // one shape, three answers, and the only difference is the
    // evidence the capture names and whether it invokes k
    def prog(f: Shift.Prompted[Int] ?=> Shift.Prompted[Int] ?=> Int ! Shift % ? + P): Int =
      !.run(Shift.delimited[Int, P]: (outer: Shift.Prompted[Int]) ?=>
        direct:
          val inner = !Shift.scope[Int, P]: (in: Shift.Prompted[Int]) ?=>
            direct:
              1 + !f(using outer)(using in)
          inner + 1000)

    // invoking k: the OUTER capture holds BOTH tails — the inner
    // scope's `+ 1` and `+ 1000`, and the whole thing re-runs
    assertEquals(prog(o ?=> _ ?=> Shift.shift[Int, Int, P](using o)(k => k(5))), 1006)
    // dropping it, named OUTER: both tails go, the block answers 5
    assertEquals(prog(o ?=> _ ?=> Shift.shift[Int, Int, P](using o)(_ => okay.freer.pure(5))), 5)
    // dropping it, named INNER: only the inner tail goes — the outer
    // `+ 1000` still runs, because it was never captured
    assertEquals(prog(_ ?=> i ?=> Shift.shift[Int, Int, P](using i)(_ => okay.freer.pure(5))), 1005)
  }

  test("scope: the inner evidence is the nearest one, and stays inside") {
    val prog: Int ! P = Shift.delimited[Int, P]:
      direct:
        val inner = !Shift.scope[Int, P]:
          direct:
            1 + !Shift.shift[Int](k => k(5))     // the nearest: the inner scope
        inner + 1000
    assertEquals(!.run(prog), 1006)
  }

  test("exit crosses a nested producer: out of the whole block, with an answer") {
    def run(stopAt: Int): String ! P = Shift.delimited[String, P]: (out: Shift.Prompted[String]) ?=>
      direct:
        val xs = !Shift.collecting[Int, P]:
          direct:
            var i = 0
            while i < 5 do
              if i == stopAt then !Shift.exit(using out)(s"stopped at $i")
              !Shift.emit(i)
              i += 1
        s"collected $xs"
    assertEquals(!.run(run(3)), "stopped at 3")
    assertEquals(!.run(run(9)), "collected List(0, 1, 2, 3, 4)")
  }

  test("delimited still runs the machine: the outermost form is unchanged") {
    val prog: Int ! P = Shift.delimited[Int, P]:
      direct:
        1 + !Shift.shift[Int](k => k(5))
    assertEquals(!.run(prog), 6)
  }
}
