package okay

import java.util.concurrent.ConcurrentHashMap

/** DEBUG-PROBE (adaptive-outside-long-fibers-serial): the Wrocław shape —
 * eight long CPU fibers forked from OUTSIDE the scheduler and joined —
 * which threads ran them and when each started, relative to the first
 * fork. Run by name; ignored in the gate. */
class ProbeOutsideLong extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")

  private def busy(nanos: Long): Unit =
    val end = System.nanoTime() + nanos
    while System.nanoTime() < end do Thread.onSpinWait()

  private def run(name: String, sch: Schedulers.Running): Unit =
    given Scheduler = sch
    try
      for round <- 1 to 3 do
        Thread.sleep(50) // every worker parked, as after `main`'s own setup
        val starts = ConcurrentHashMap[Int, (Long, Long)]()
        val t0 = System.nanoTime()
        val fs = (0 until 8).map { i =>
          Async.spawn(async {
            starts.put(i, (Thread.currentThread().threadId(), (System.nanoTime() - t0) / 1000000L))
            busy(70000000L)
          })
        }
        fs.foreach(_.join())
        val wall = (System.nanoTime() - t0) / 1000000L
        val ids = (0 until 8).map(starts.get(_)._1).distinct.size
        val at = (0 until 8).map(starts.get(_)._2).mkString(",")
        println(s"[PROBE] $name round $round: wall ${wall} ms, $ids thread(s), starts at ms [$at]")
    finally sch.close()

  test("outside burst of long fibers: threads and start times".ignore) {
    run("adaptive", Schedulers.adaptive.build)
    run("own     ", Schedulers.own.build)
    run("loom    ", new Schedulers.Running {
      val id = 0
      def fork[A](prog: () => A ! Async): Fiber[A] = Schedulers.loom.fork(prog)
      def close(): Unit = ()
    })
  }
}
