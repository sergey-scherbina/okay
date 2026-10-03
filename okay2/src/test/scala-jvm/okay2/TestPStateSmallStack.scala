package okay2

/**
 * PState's function answer applied by a loop: a million steps on a 128 KB thread, and the run never leaves it.
 * The final `ret` is the deepest point of the chain, so the thread that runs it says whether a fresh stack was
 * taken.
 */
class TestPStateSmallStack extends munit.FunSuite {

  type R = (Long, Long)

  def steps(n: Int): Cont[Long, Long => R, Long => R] =
    (1 to n).foldLeft(PState.get[Long, R])((m, _) =>
      m.flatMap(_ => PState.get[Long, R].flatMap(s => PState.set[Long, Long, R](s + 1))))

  test("a million PState steps on 128 KB, on that thread to the end") {
    val p = steps(1000000)
    var last = ""
    val out = SmallStack.run(128)((p / ((a: Long) => (s2: Long) => { last = Thread.currentThread.getName; (s2, a) }))(0L))
    assertEquals(out, (1000000L, 999999L))
    assertEquals(last, "small-stack", "the chain's end ran on a fresh stack: a level held a host frame")
  }
}
