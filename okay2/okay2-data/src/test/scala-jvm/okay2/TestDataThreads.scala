package okay2

/** The two laws of the clock and the id that need REAL threads — JVM
 * only, as the Scala 3 core keeps its `TestUidConcurrent` (okay2-cross).
 * Both against a FROZEN clock: every value is separated by the counter
 * alone, so a CAS that is not atomic shows up as a collision */
class TestDataThreads extends munit.FunSuite {
  import TestDataClock._

  test("no duplicate stamps when several threads share one clock (a FROZEN clock: the counter alone separates them)") {
    val c = Hlc.at(() => Epoch)
    val seen = new java.util.concurrent.ConcurrentHashMap[Long, Boolean]()
    val threads = (0 until 8).map { _ =>
      val t = new Thread(() => { var i = 0; while (i < 2000) { seen.put(c.next().toLong, true); i += 1 } })
      t.start(); t
    }
    threads.foreach(_.join())
    assertEquals(seen.size, 8 * 2000, "stamps collided, so the CAS is wrong")
  }

  test("no duplicate ids when several threads share one generator (a FROZEN clock)") {
    val gen = Uid.at(() => Epoch)
    val out = new java.util.concurrent.ConcurrentHashMap[Uid, Boolean]()
    val threads = (0 until 8).map { _ =>
      val t = new Thread(() => { var i = 0; while (i < 2000) { out.put(gen.next(), true); i += 1 } })
      t.start(); t
    }
    threads.foreach(_.join())
    assertEquals(out.size, 8 * 2000, "ids collided under concurrency, so the counter is not atomic")
  }
}
