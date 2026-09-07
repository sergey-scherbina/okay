package okay

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

/** DEBUG-PROBE (hole-scan): how deep is the cliff when a producer is
 * descheduled between claiming a position and publishing it? */
class ProbeHole extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(2, "min")

  test("a held hole hides everything behind it") {
    val b = Segments[Int]()
    val holdMs = 300L
    val gate = java.util.concurrent.CountDownLatch(1)
    val holed = java.util.concurrent.CountDownLatch(1)
    // one producer stops inside the window, once
    val once = AtomicInteger(0)
    b.holeProbe = (_: Long) =>
      if once.compareAndSet(0, 1) then { holed.countDown(); gate.await() }
    val slow = Thread.startVirtualThread(() => { val _ = b.push(-1) })
    holed.await()
    // everything after the hole is published and ready
    var i = 0
    while i < 5000 do { val _ = b.push(i); i += 1 }
    val visibleWhileHoled = { var n = 0; while b.pop() != null do n += 1; n }
    gate.countDown()
    slow.join()
    val visibleAfter = { var n = 0; while b.pop() != null do n += 1; n }
    println(f"[PROBE] behind a held hole the consumer saw $visibleWhileHoled%5d of 5001; after it filled, $visibleAfter%5d")
    assertEquals(visibleWhileHoled + visibleAfter, 5001, "nothing may be lost either way")
  }
}
