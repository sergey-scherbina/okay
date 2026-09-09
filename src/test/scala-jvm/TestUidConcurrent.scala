package okay

/**
 * The two laws of specs/coordination-free.md stage 1 that need real
 * threads, so they live here rather than beside the rest: core's
 * shared tests run on JS and Native too, and neither has
 * `Thread.ofPlatform` or a `ConcurrentHashMap`.
 *
 * Both run against a FROZEN clock, which is the worst case on
 * purpose: with the physical millisecond never moving, every id in
 * the run has to be separated by the counter alone, so a CAS that is
 * not atomic shows up as a collision rather than as luck.
 */
class TestUidConcurrent extends munit.FunSuite {

  private val Epoch = 1_700_000_000_000L
  private def frozen: () => Long = () => Epoch

  test("no duplicate ids when several threads share one generator") {
    val gen = Uid.at(frozen)
    val out = java.util.concurrent.ConcurrentHashMap[Uid, Boolean]()
    val threads = (0 until 8).map(_ => Thread.ofPlatform().start { () =>
      var i = 0
      while i < 2000 do { out.put(gen.next(), true); i += 1 }
    })
    threads.foreach(_.join())
    assertEquals(out.size, 8 * 2000,
      "ids collided under concurrency, so the counter is not atomic")
  }

  test("no duplicate stamps when several threads share one clock") {
    val c = Hlc.at(frozen)
    val seen = java.util.concurrent.ConcurrentHashMap[Long, Boolean]()
    val threads = (0 until 8).map(_ => Thread.ofPlatform().start { () =>
      var i = 0
      while i < 2000 do { seen.put(c.next().toLong, true); i += 1 }
    })
    threads.foreach(_.join())
    assertEquals(seen.size, 8 * 2000, "stamps collided, so the CAS is wrong")
  }
}
