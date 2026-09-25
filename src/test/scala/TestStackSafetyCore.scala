package okay

import Aggregator as A

/**
 * specs/stack-safety.md, stage 1: the core's recursions at a depth a
 * small stack cannot hold. Each test ran RED before its method moved
 * to a loop or a trampoline.
 */
class TestStackSafetyCore extends munit.FunSuite:

  test("topK: an insertion k deep walks as a loop") {
    // descending input: every new element is the smallest so far, so
    // it is inserted after all k kept ones
    val k = 4000
    val xs = (k to 1 by -1).toList
    assertEquals(SmallStack.run()(A.topK[Int](k).run(xs)), xs)
  }

  type Row = Delim + Pure

  test("Delim: a shift under thousands of pending continuations splits the stack as a loop") {
    // every level leaves a `.map` pending under the prompt, so the n-th
    // shift cuts a chain n segments long
    val n = 20000
    def deep(p: Prompt[Long], i: Int): Long ! Row =
      if i == 0 then okay.pure(0L)
      else Delim.shift[Long, Long, Pure](p)(k => k(i.toLong)).flatMap(x => deep(p, i - 1).map(_ + x))
    assertEquals(SmallStack.run()(!.run(Delim.reset[Long, Pure](p => deep(p, n)))), n.toLong * (n + 1) / 2)
  }

  test("Delim: a shift to a prompt under thousands of other delimiters cuts the chain as a loop") {
    // n prompts pushed inside one another, then a shift to the OUTERMOST:
    // the cut walks past every inner mark
    val n = 20000
    // built inside out by a loop, so only the machine can be what overflows
    def nest(outer: Prompt[Int]): Int ! Row =
      var prog: Int ! Row = Delim.shift[Int, Int, Pure](outer)(k => k(1).map(_ + 1))
      for _ <- 1 to n do prog = Delim.push(Delim.prompt[Int])(prog)
      prog
    assertEquals(SmallStack.run()(!.run(Delim.reset[Int, Pure](p => nest(p)))), 2)
  }

  test("reflect into Eager: a program of many operations reflects without the stack") {
    import Eager.given
    val n = 20000
    def go(i: Int): Int ! Produce = if i == 0 then okay.pure(0) else effect[Produce, Int](i).flatMap(_ => go(i - 1))
    assertEquals(SmallStack.run()(reflect[Eager, Produce, Int](go(n)).runWith), 0)
  }
