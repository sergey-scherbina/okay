package okay.freer


import okay.freer.Row.plus

/** the keyed `reset` nested a hundred thousand deep on a 128 KB stack, and no stack switch taken for it */
class TestResetSmallStack extends munit.FunSuite:

  def nest(n: Int): Int ! Pure =
    if n == 0 then pure(0)
    else reset[Int, Pure](!.tailcall(nest(n - 1)).plus[Shift % Int].flatMap(x => shift0[Int, Int, Pure](k => k(x + 1))))

  test("100 000 nested resets on 128 KB, no fresh stack") {
    val before = StackSwitch.switches.get
    assertEquals(SmallStack.run(128)(!.run(nest(100000))), 100000)
    // the counter is PROCESS-WIDE: a suite running beside this one may switch for its own Cps (one did, in a
    // full gate); the room this replaced switched every ~300 levels, hundreds of times for 100 000
    val switched = StackSwitch.switches.get - before
    assert(switched < 10, s"$switched stack switches: a nested reset ran a machine of its own")
  }
