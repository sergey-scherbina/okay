package okay

/**
 * specs/cont-stack.md plan stage D3, moved up by the stage-A A/B: on
 * the count road statePara's ~2 000 levels outrun the first room and
 * switch once a run, and that switch — a NEW thread and its cold stack
 * pages — was the whole of its 4.9x (27.3 → 134.8 µs; 28.9 µs with the
 * switch taken away). A switch hands the rest to a PARKED worker whose
 * 1 GB stack is already reserved and warm, and the worker returns to the
 * pool when the program is done with it.
 */
class TestStackPool extends munit.FunSuite:

  /** a program that switches: tail leaves past any first room on a
   * 256 KB thread, recording the thread it finishes on */
  private def program(n: Int, where: collection.mutable.Set[Thread]): Int /> Int =
    (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => Cont.shiftLeaf[Int, Int, Int](k => k(x + 1))))
      .flatMap(x => { where += Thread.currentThread(); Cont.Pure[Int, Int](x) })

  test("consecutive switches reuse one parked worker instead of starting a thread each") {
    val where = collection.mutable.Set.empty[Thread]
    val before = StackSwitch.switches.get()
    for _ <- 1 to 5 do assertEquals(SmallStack.run(256)(reset(program(2000, where))), 2000)
    assert(StackSwitch.switches.get() - before >= 5, "the program never switched")
    assertEquals(where.size, 1, s"finished on ${where.size} different threads: ${where.map(_.getName)}")
    assert(where.head.getName.startsWith("okay-cont-stack"), where.head.getName)
  }

  test("concurrent switching programs each get their answer") {
    val threads = (1 to 8).map: i =>
      var out = -1
      val t = new Thread(null, () => out = reset(program(2000 + i, collection.mutable.Set.empty)), "caller", 256L * 1024)
      (t, () => out, 2000 + i)
    threads.foreach(_._1.start())
    threads.foreach(_._1.join())
    threads.foreach((_, out, want) => assertEquals(out(), want))
  }

  test("an exception after the switch reaches the caller, and the worker survives it") {
    def boom(n: Int): Int /> Int =
      (1 to n).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) =>
        m.flatMap(x => Cont.shiftLeaf[Int, Int, Int](k => if x == n - 1 then throw IllegalStateException("late") else k(x + 1))))
    val e = intercept[IllegalStateException](SmallStack.run(256)(reset(boom(2000))))
    assertEquals(e.getMessage, "late")
    assertEquals(SmallStack.run(256)(reset(program(2000, collection.mutable.Set.empty))), 2000)
  }

  test("the caller's context class loader is the worker's while it runs the rest") {
    val mine = new ClassLoader(getClass.getClassLoader) {}
    var seen: ClassLoader | Null = null
    val p = (1 to 2000).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => Cont.shiftLeaf[Int, Int, Int](k => k(x + 1))))
      .flatMap(x => { seen = Thread.currentThread().getContextClassLoader; Cont.Pure[Int, Int](x) })
    val answer = SmallStack.run(256):
      Thread.currentThread().setContextClassLoader(mine)
      reset(p)
    assertEquals(answer, 2000)
    assert(seen eq mine, s"saw $seen")
  }
