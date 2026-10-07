package okay


import okay.freer.*
import java.util.concurrent.atomic.AtomicInteger

/** DEBUG-PROBE (own-scheduler-monitor): is `adaptive`'s blocking ceiling
 * the overflow bound? 64 fibers forked inside a fiber, each 4 x 1 ms
 * blocking calls — the five-way blocking TCP shape — peak concurrency
 * and time per batch, default overflow against overflow = 64. */
class ProbeAdaptiveOverflow extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  private def batch(using Scheduler): (Int, Double) =
    val active = AtomicInteger(); val peak = AtomicInteger()
    val t0 = System.nanoTime()
    Async.spawn {
      async((0 until 64).map(_ => Async.spawn(async {
        var c = 0
        while c < 4 do
          val _ = peak.accumulateAndGet(active.incrementAndGet(), math.max)
          Thread.sleep(1); val _ = active.decrementAndGet(); c += 1
      }))).flatMap(fs => fs.foldLeft(pure[Async, Unit](()))((acc, f) => acc.flatMap(_ => f.joinAsync)))
    }.join()
    (peak.get, (System.nanoTime() - t0) / 1e6)

  test("adaptive overflow ceiling".ignore) {
    val cores = Runtime.getRuntime.availableProcessors()
    for (name, own) <- List("default" -> Schedulers.adaptive, "overflow=64" -> Schedulers.adaptive.watched(overflow = 64)) do
      val s = own.build
      try
        given Scheduler = s
        val runs = (1 to 20).map(_ => batch)
        println(f"[PROBE] $name%-12s cores=$cores peak=${runs.map(_._1).max} ms/batch(min)=${runs.map(_._2).min}%.1f")
      finally s.close()
  }
}
