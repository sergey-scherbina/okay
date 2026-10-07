package okay.freer


/**
 * PState's FUNCTION ANSWER applied by a loop (cont-fun-answer): a million `get`/`set` steps on a 128 KB thread,
 * and the run never leaves it. The deepest point of the chain is the final `ret`, so the thread that runs it says
 * whether a fresh stack was taken — a check of this run alone, where the process-wide switch counter is shared
 * with the suites running beside this one.
 */
class TestPStateSmallStack extends munit.FunSuite:

  def steps(n: Int): Cont[Long, Long => (Long, Long), Long => (Long, Long)] =
    (1 to n).foldLeft(PState.get[Long, (Long, Long)])((m, _) => m.flatMap(_ => PState.get.flatMap(s => PState.set(s + 1))))

  test("a million PState steps on 128 KB, on that thread to the end") {
    val p = steps(1000000)
    var last = ""
    val out = SmallStack.run(128)((p / (a => (s2: Long) => { last = Thread.currentThread.getName; (s2, a) }))(0L))
    assertEquals(out, (1000000L, 999999L))
    assertEquals(last, "small-stack", "the chain's end ran on a fresh stack: a level held a host frame")
  }
