package scala2probe

import okay.scala2.Cps

/** okay.scala2.Cps from Scala 2.13 (specs/scala2-facade.md, stage 2) */
class TestCpsFromScala2 extends munit.FunSuite {

  test("shift captures the continuation up to reset, and may call it twice") {
    val c: Cps[Int, Int, Int] = for {
      a <- Cps.shift[Int, Int, Int](k => k(k(10)))
      b <- Cps.pure[Int, Int](1)
    } yield a + b
    assertEquals(Cps.reset(c), 12)
  }

  test("a continuation not called discards the rest") {
    var rest = false
    val c = Cps.shift[Int, Int, Int](_ => 7).map { x => rest = true; x * 100 }
    assertEquals(Cps.reset(c), 7)
    assert(!rest)
  }

  test("answer-type modification: the continuation answers Int, the block String") {
    val s: String = Cps.reset(Cps.shift[Int, Int, String](k => "k(5)=" + k(5)).map(_ * 2))
    assertEquals(s, "k(5)=10")
  }

  test("run feeds an explicit continuation") {
    assertEquals(Cps.pure[Int, String](4).map(_ + 1).run(n => "got " + n), "got 5")
  }

  test("100 000 binds, left- and right-nested, without a stack overflow") {
    val left = (1 to 100000).foldLeft(Cps.pure[Int, Int](0))((c, _) => c.flatMap(x => Cps.pure[Int, Int](x + 1)))
    assertEquals(Cps.reset(left), 100000)
    def right(n: Int): Cps[Int, Int, Int] =
      if (n == 0) Cps.pure[Int, Int](0) else Cps.pure[Int, Int](n).flatMap(_ => right(n - 1).map(_ + 1))
    assertEquals(Cps.reset(right(100000)), 100000)
  }
}
