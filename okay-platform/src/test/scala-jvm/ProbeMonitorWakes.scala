package okay


import okay.freer.*


import okay.std.*
/** DEBUG-PROBE (own-scheduler-monitor): how many wakes does the monitor
 * issue on sequential spawn/join, where no task is ever long? Every
 * one is a child stolen to another core for nothing. */
class ProbeMonitorWakes extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  private def spawnJoin(ops: Int)(using Scheduler): Long =
    def loop(i: Int, sum: Long): Long ! Async =
      if i == ops then pure[Async, Long](sum)
      else async(Async.spawn(async(i))).flatMap(_.joinAsync).flatMap(v => loop(i + 1, sum + v))
    Async.spawn(loop(0, 0L)).join()

  test("monitor wakes on sequential spawn/join".ignore) {
    for (name, b) <- List("unmonitored" -> Schedulers.own.unmonitored, "monitored" -> Schedulers.own) do
      val s = b.build
      try
        given Scheduler = s
        val o = s.asInstanceOf[Schedulers.Owned]
        for _ <- 1 to 2000 do { val _ = spawnJoin(1000) }
        o.reset()
        val t0 = System.nanoTime()
        for _ <- 1 to 2000 do { val _ = spawnJoin(1000) }
        val us = (System.nanoTime() - t0) / 1e3 / 2000
        println(f"[PROBE] $name%-12s ${us}%.1f us per 1000 spawn/joins  ${o.stats}")
      finally s.close()
  }
}
