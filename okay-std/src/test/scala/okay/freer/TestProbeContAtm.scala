package okay.freer


import okay.std.*
import okay.std.given
import okay.{Func}

import ContAtm.*

/** specs/cont-atm.md's probe: D-F answer-type modification on the typed CK + meta machine, no cast */
class TestProbeContAtm extends munit.FunSuite:

  test("answer-type modification: Int → String and Boolean → Int in one reset, strict and lazy") {
    val strict: C[Boolean, Boolean, String] = for
      a <- shift[Int, Int, String](k => k(1).toString)
      b <- shift[Boolean, Boolean, Int](k => if k(true) then 1 else 0)
    yield a > 0 && b
    val lazily: C[Boolean, Boolean, String] = for
      a <- shiftLazy[Int, Int, String](k => call(k, 1)(s => done(s.toString)))
      b <- shiftLazy[Boolean, Boolean, Int](k => call(k, true)(h => done(if h then 1 else 0)))
    yield a > 0 && b
    // k2(true) = 1 > 0 && true; leaf 2 answers 1; k1(1) = 1; leaf 1 answers "1"
    assertEquals((run(strict)(identity), run(lazily)(identity)), ("1", "1"))
  }

  test("k(x + 1) + k(x + 1) chained d deep, strict and lazy, against closures (cont-shift-op's refuting case)") {
    def strict(d: Int): C[Int, Int, Int] =
      (1 to d).foldLeft(pure[Int, Int](0))((m, _) => m.flatMap(x => shift[Int, Int, Int](k => k(x + 1) + k(x + 1))))
    def lazily(d: Int): C[Int, Int, Int] =
      (1 to d).foldLeft(pure[Int, Int](0))((m, _) =>
        m.flatMap(x => shiftLazy[Int, Int, Int](k => call(k, x + 1)(a => call(k, x + 1)(b => done(a + b))))))
    def closures(d: Int): Func[Int, Int, Int] =
      (1 to d).foldLeft[Func[Int, Int, Int]](k => k(0))((m, _) => k => m(x => k(x + 1) + k(x + 1)))
    for d <- 0 to 6 do
      val want = closures(d)(identity)
      assertEquals((reset(strict(d)), reset(lazily(d))), (want, want), s"d = $d")
  }

  test("multi-shot: a lazy k resumed twice collects both worlds") {
    val c: C[List[Int], List[Int], List[Int]] = for
      x <- shiftLazy[Int, List[Int], List[Int]](k => call(k, 1)(a => call(k, 2)(b => done(a ++ b))))
      y <- shiftLazy[Int, List[Int], List[Int]](k => call(k, 10)(a => call(k, 20)(b => done(a ++ b))))
    yield List(x + y)
    assertEquals(run(c)(identity), List(11, 21, 12, 22))
  }

  /** run on a small stack: what the machine holds is on the heap */
  private def onSmallStack[A](body: => A): A =
    var out: Option[A] = None
    var err: Throwable | Null = null
    val t = Thread(null, () => try out = Some(body) catch case e: Throwable => err = e, "small", 256 * 1024)
    t.start()
    t.join()
    if err != null then throw err.nn
    out.get

  test("stack safety: 1M left-nested binds and 1M lazy-k shifts (contAnswer's shape) on a 256 KB thread") {
    val n = 1000000
    val binds = (1 to n).foldLeft(pure[Int, Int](0))((m, _) => m.flatMap(x => pure(x + 1)))
    val shifts = (1 to n).foldLeft(pure[Int, Int](0))((m, _) =>
      m.flatMap(x => shiftLazy[Int, Int, Int](k => call(k, x + 1)(s => done(s + 1)))))
    // each shift adds 1 to the value and 1 to the answer on the way out
    assertEquals(onSmallStack((reset(binds), reset(shifts))), (n, 2 * n))
  }
