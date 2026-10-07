package okay.freer



/** cont-js-depth stage 4 on a 128 KB JVM thread: re-execution on, no fresh stack needed for a million strict levels */
class TestContReplaySmallStack extends munit.FunSuite:

  def nest(n: Int): Cps[Int, Int, Int] =
    if n == 0 then Cps.Pure(0)
    else Cps.shiftLeaf[Int, Int, Int](k => k(1) + 1).flatMap(x => nest(n - 1).map(_ + x))

  test("a million nested strict bodies on 128 KB, re-executed, no stack switched") {
    val (on0, room0) = (ContReplay.on, ContReplay.room)
    val before = StackSwitch.switches.get()
    val out =
      try
        ContReplay.on = true
        ContReplay.room = 16   // a cold JVM level is ~2.6 KB (StackSwitch.coldBytesPerLevel): 16 fit 128 KB, 64 do not
        SmallStack(128)(Cps.reset(nest(1000000)))
      finally { ContReplay.on = on0; ContReplay.room = room0 }
    assertEquals(out, Right(2000000))
    assertEquals(StackSwitch.switches.get() - before, 0L)
  }
