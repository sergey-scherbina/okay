package okay2

/** the keyed `reset` nested a hundred thousand deep on a 128 KB stack, and no stack switch taken for it */
class TestResetSmallStack extends munit.FunSuite {

  def nest(n: Int): Int ! Pure =
    if (n == 0) pure(0)
    else reset[Int, Pure](!.tailcall(nest(n - 1)).plus[Shift[Int]].flatMap(x => shift0[Int, Int, Pure](k => k(x + 1))))

  test("100 000 nested resets on 128 KB, no fresh stack") {
    val before = StackSwitch.switches.get
    assertEquals(SmallStack.run(128)(!.run(nest(100000))), 100000)
    // ZERO: the JVM suites run forked and one after another, so the process-wide counter is this test's. The
    // room this replaced switched once and then ran the rest on the 1 GB fresh stack, which is what a
    // "fewer than ten" bound let through
    assertEquals(StackSwitch.switches.get - before, 0L, "a nested reset ran a machine of its own")
  }
}
