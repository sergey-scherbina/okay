package okay2

import StaticFixtures._
import Fetch.{Get, Flag}

/**
 * specs/stack-safety.md, stage 1b: the okay2 core's recursions at a
 * depth a small stack cannot hold. Each test ran RED before its method
 * became a loop.
 */
class TestStackSafetyCore extends munit.FunSuite {

  test("topK: an insertion k deep walks as a loop") {
    val k = 4000
    val xs = (k to 1 by -1).toList
    assertEquals(SmallStack.run()(Aggregator.topK[Int](k).run(xs)), xs)
  }

  type P = Pure

  test("Delim: a shift to a prompt under thousands of other delimiters cuts the chain as a loop") {
    val n = 20000
    // built inside out by a loop, so only the machine can be what overflows
    def nest(outer: Prompt[Int]): Int ! (Delim + P) = {
      var prog: Int ! (Delim + P) = Delim.shift[Int, Int, P](outer)(k => k(1).map(_ + 1))
      for (_ <- 1 to n) prog = Delim.push[Int, P](Delim.prompt[Int])(prog)
      prog
    }
    assertEquals(SmallStack.run()(!.run(Delim.reset[Int, P](p => nest(p)))), 2)
  }

  private val S = implicitly[Selective[({ type L[A] = Static[Fetch, A] })#L]]
  private def get(k: String): Static[Fetch, Int] = Static.op[Fetch, Int](Get(k))
  private val count: Static.To[Fetch, Count] = new Static.To[Fetch, Count] {
    def apply[X](op: Fetch.Op[X]): Count[X] = op match {
      case Get(_) => Count(1, answer[X](1))
      case Flag(_) => Count(1, answer[X](true))
    }
  }
  private val deep = 3000

  test("Static.foldMap: a deep nest in a select's CONDITION") {
    var s: Static[Fetch, Int] = get("a")
    for (_ <- 1 to deep) s = S.select(S.fmap(s, (i: Int) => Left(i): Either[Int, Int]), S.pure((i: Int) => i + 1))
    assertEquals(SmallStack.run()(s.foldMap(count)), Count(1, deep + 1))
  }

  test("Static.foldMap: a deep nest in an application's ARGUMENT") {
    var s: Static[Fetch, Int] = get("a")
    for (_ <- 1 to deep) s = S.app(S.fmap(get("a"), (_: Int) => (x: Int) => x + 1), s)
    assertEquals(SmallStack.run()(s.foldMap(count)), Count(deep + 1, deep + 1))
  }

  test("Static.foldMap: a deep nest in a select's FUNCTION side") {
    var s: Static[Fetch, Int] = get("a")
    for (_ <- 1 to deep) {
      val prev = s
      s = S.select(S.pure(Left(0): Either[Int, Int]), S.fmap(prev, (v: Int) => (_: Int) => v + 1))
    }
    assertEquals(SmallStack.run()(s.foldMap(count)), Count(1, deep + 1))
  }
}
