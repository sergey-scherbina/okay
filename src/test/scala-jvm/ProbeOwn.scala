package okay

/** DEBUG-PROBE (schedulers-family): what the owned-worker scheduler
 * actually does on the inside-fork burst — how many workers were
 * activated, how many stepped down, how the 10 000 tasks landed. */
class ProbeOwn extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  private def step(i: Int, work: Int): Int =
    var s = 0; var j = 0
    while j < work do { s += (i ^ j); j += 1 }
    s

  test("inside-fork burst, per-worker counts".ignore) {
    val sch = Schedulers.own.build
    given Scheduler = sch
    val own = sch.asInstanceOf[Schedulers.Owned]
    for work <- List(100, 10000) do
      own.reset()
      val t0 = System.nanoTime()
      val sum = Async.spawn {
        val fs = (0 until 10000).map(i => Async.spawn(async(step(i, work))))
        fs.foldLeft(pure[Async, Long](0L))((acc, f) => acc.flatMap(a => f.joinAsync.map(a + _)))
      }.join()
      val ms = (System.nanoTime() - t0) / 1e6
      println(f"[PROBE] work=$work%5d  ${ms}%8.2f ms  sum=$sum  ${own.stats}")
  }
}
