package okay.freer


import okay.std.*
import okay.std.given
import okay.{Func}

import DelimAtm.*

/** specs/cont-atm.md stage 2a: the multi-prompt machine typed per installation */
class TestProbeDelimAtm extends munit.FunSuite:

  test("ATM at the nearest delimiter: Int → String and Boolean → Int in one run, strict and lazy") {
    val strict: C[Boolean, Boolean, String] = for
      a <- Cps.shift[Int, Int, String](k => k(1).toString)
      b <- Cps.shift[Boolean, Boolean, Int](k => if k(true) then 1 else 0)
    yield a > 0 && b
    val lazily: C[Boolean, Boolean, String] = for
      a <- Cps.shiftLazy[Int, Int, String]([X] => k => Cps.call[Int, Int, X](k, 1).map(_.toString))
      b <- Cps.shiftLazy[Boolean, Boolean, Int]([X] => k => Cps.call[Boolean, Boolean, X](k, true).map(h => if h then 1 else 0))
    yield a > 0 && b
    assertEquals((Cps.run(strict)(identity), Cps.run(lazily)(identity)), ("1", "1"))
  }

  test("k(x + 1) + k(x + 1) chained d deep, lazy, against closures") {
    def lazily(d: Int): C[Int, Int, Int] =
      (1 to d).foldLeft(pure[Int, Int](0))((m, _) => m.flatMap(x =>
        Cps.shiftLazy[Int, Int, Int]([X] => k =>
          Cps.call[Int, Int, X](k, x + 1).flatMap(a => Cps.call[Int, Int, X](k, x + 1).map(b => a + b)))))
    def closures(d: Int): Func[Int, Int, Int] =
      (1 to d).foldLeft[Func[Int, Int, Int]](k => k(0))((m, _) => k => m(x => k(x + 1) + k(x + 1)))
    for d <- 0 to 6 do assertEquals(Cps.run(lazily(d))(identity), closures(d)(identity), s"d = $d")
  }

  test("a capture by name crosses a level of another prompt, and its k puts that level back") {
    val p = new Prompt[Int]("p")
    val q = new Prompt[String]("q")
    // under p: the length of what q's level answers; under q: the captured value as text
    def prog(body: [W] => Cap[Int, Int] => C[Int, W, W]): C[Int, Int, Int] =
      reset(p)(reset(q)(C.To[Int, Int, String](p, body).map(_.toString)).map(_.length))
    val strict = prog([W] => k => pure[Int, W](runCap(k, 10) + runCap(k, 1000)))
    val lazily = prog([W] => k => C.ResumeCap[Int, Int, W](k, 10).flatMap(a => C.ResumeCap[Int, Int, W](k, 1000).map(b => a + b)))
    // k(10) = "10".length = 2, k(1000) = 4
    assertEquals((run(strict), run(lazily)), (6, 6))
  }

  test("the nearest capture takes the innermost level, whatever the prompts") {
    val p = new Prompt[Int]("p")
    val q = new Prompt[Int]("q")
    val c: C[Int, Int, Int] = reset(p)(reset(q)(Cps.shiftLazy[Int, Int, Int]([X] => k => Cps.call[Int, Int, X](k, 1).map(_ * 100))
      .map(_ + 1)).map(_ + 10))
    // q's level: (1 + 1) * 100 = 200; p's: 200 + 10
    assertEquals(run(c), 210)
  }

  test("multi-shot through a level: a named k resumed twice") {
    val p = new Prompt[List[Int]]("p")
    val q = new Prompt[List[Int]]("q")
    val c: C[List[Int], List[Int], List[Int]] = reset(p)(reset(q)(C.To[Int, List[Int], List[Int]](p, [W] => k =>
      C.ResumeCap[Int, List[Int], W](k, 1).flatMap(a => C.ResumeCap[Int, List[Int], W](k, 2).map(b => a ++ b)))
      .map(x => List(x, -x))).map(_.map(_ * 10)))
    assertEquals(run(c), List(10, -10, 20, -20))
  }

  test("a capture to a prompt that is not installed fails by name") {
    val p = new Prompt[Int]("absent")
    val e = intercept[AtmNoPrompt](run(C.To[Int, Int, Int](p, [W] => _ => pure[Int, W](0))))
    assert(e.getMessage.nn.contains("absent"), e.getMessage)
  }

  private def onSmallStack[A](body: => A): A =
    var out: Option[A] = None
    var err: Throwable | Null = null
    val t = Thread(null, () => try out = Some(body) catch case e: Throwable => err = e, "small", 256 * 1024)
    t.start()
    t.join()
    if err != null then throw err.nn
    out.get

  test("stack safety on 256 KB: 1M lazy shifts, 1M binds, a named capture through 100 000 levels") {
    val n = 1000000
    val shifts = (1 to n).foldLeft(pure[Int, Int](0))((m, _) =>
      m.flatMap(x => Cps.shiftLazy[Int, Int, Int]([X] => k => Cps.call[Int, Int, X](k, x + 1).map(_ + 1))))
    val binds = (1 to n).foldLeft(pure[Int, Int](0))((m, _) => m.flatMap(x => pure(x + 1)))
    val p = new Prompt[Int]("p")
    val levels = 100000
    val inner: C[Int, Int, Int] = C.To[Int, Int, Int](p, [W] => k => C.ResumeCap[Int, Int, W](k, 1))
    val deep = (1 to levels).foldLeft(inner)((c, i) => reset(new Prompt[Int](s"q$i"))(c).map(_ + 1))
    assertEquals(onSmallStack((Cps.run(shifts)(identity), Cps.run(binds)(identity), run(reset(p)(deep)))),
      (2 * n, n, 1 + levels))
  }
