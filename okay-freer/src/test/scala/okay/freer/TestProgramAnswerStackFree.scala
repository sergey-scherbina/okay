package okay.freer


/**
 * A strict `k` whose body answers a PROGRAM holds no host stack (cont-program-leaf-always, answered
 * 2026-10-04). `k(x)` runs the rest only to the next capture, whose body answers its program at once, so the
 * call returns, and the trampolined program runs on. Pinned by switch counts: under the tests' room of 64, a
 * strict run per level would switch over 1 500 times in 100 000 levels.
 */
class TestProgramAnswerStackFree extends munit.FunSuite:

  test("runChoice over 100 000 choice points switches no stack") {
    val p = (1 to 100000).foldLeft(pure[Choose, Int](0))((m, _) => m.flatMap(x => Choose(Seq(x + 1)).perform))
    val before = StackSwitch.switches.get()
    assertEquals(runChoice(p).run, Seq(100000))
    assertEquals(StackSwitch.switches.get() - before, 0L)
  }

  test("Prob.runExact over 100 000 distributions switches no stack") {
    val p = (1 to 100000).foldLeft(pure[Dist, Int](0))((m, _) => m.flatMap(x => Prob.dist(x + 1 -> 1.0)))
    val before = StackSwitch.switches.get()
    assertEquals(Prob.runExact(p).run, Map(100000 -> 1.0))
    assertEquals(StackSwitch.switches.get() - before, 0L)
  }

  test("control: an answer-using strict body over a value does switch (the road that still needs the stack)") {
    val p = (1 to 2000).foldLeft(Cps.Pure[Int, Int](0): Int />> Int)((m, _) =>
      m.flatMap(x => Cps.shiftLeaf[Int, Int, Int](k => k(x + 1) + 1)))
    // on a 256 KB thread, which 2 000 such levels outgrow whether the stack is read or counted
    val before = StackSwitch.switches.get()
    assertEquals(SmallStack.run(256)(Cps.reset(p)), 4000)
    assert(StackSwitch.switches.get() - before >= 1)
  }
