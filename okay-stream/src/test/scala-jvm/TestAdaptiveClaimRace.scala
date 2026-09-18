package okay

/**
 * The window `claimPart` opens: the count is published by
 * `open.getAndIncrement()` and the SLOT a moment later, so a producer
 * that shares an existing part — there are more producers than parts —
 * can read that slot while it is still null (channel-lost-part).
 * Every other reader in the class checks for the null and comes back;
 * the thread-local `Home` did not, and answered `.nn` on it.
 *
 * More producers than parts, all taking their FIRST route at once,
 * many rounds.
 */
class TestAdaptiveClaimRace extends munit.FunSuite {

  test("a producer that shares a part never meets a null slot on its first send") {
    val parts = 2
    val producers = 16
    val rounds = 400
    var thrown: Throwable | Null = null
    var r = 0
    while r < rounds && thrown == null do
      val buf = AdaptiveFifo[Long](parts, () => Ring[Long](64), eager = false, first = null)
      val start = java.util.concurrent.CountDownLatch(1)
      val done = java.util.concurrent.CountDownLatch(producers)
      var i = 0
      while i < producers do
        val w = i
        Thread.ofVirtual().start { () =>
          try
            start.await()
            val _ = buf.push(w.toLong)    // the FIRST push takes the route
          catch case t: Throwable => synchronized { if thrown == null then thrown = t }
          finally done.countDown()
        }: Unit
        i += 1
      start.countDown()
      done.await()
      r += 1
    thrown match
      case null => ()
      case t =>
        val frames = t.nn.getStackTrace.nn
        val where = (0 until math.min(3, frames.length)).map(k => String.valueOf(frames(k))).mkString(" <- ")
        fail(s"a first send threw after $r rounds: ${t.nn.getClass.getName}: ${t.nn.getMessage} at $where")
  }
}
