package okay2

object TestDataClock {
  /** a clock the test drives */
  final class Fake(var t: Long) {
    def source: () => Long = () => t
    def advance(by: Long): Unit = t += by
    def stepBack(by: Long): Unit = t -= by
  }
  final val Epoch = 1700000000000L
}

/** The clock's own laws — the Scala 3 core's okay-data TestHlc. `Uid`
 * tests it from above; these test `observe`, the HYBRID part. */
class TestHlc extends munit.FunSuite {
  import TestDataClock._

  val ord: Ordering[Hlc.Stamp] = implicitly[Ordering[Hlc.Stamp]]

  test("a stamp carries its millisecond and its counter, and packs into one Long") {
    val s = Hlc(Epoch, 7)
    assertEquals(s.millis, Epoch)
    assertEquals(s.counter, 7)
    assertEquals(Hlc.fromLong(s.toLong), s)
  }

  test("stamps from one clock never decrease, whatever the physical clock does") {
    val clock = new Fake(Epoch)
    val c = Hlc.at(clock.source)
    var prev = c.next()
    var i = 0
    while (i < 500) {
      if (i % 7 == 0) clock.stepBack(1000) else clock.advance(1)
      val next = c.next()
      assert(ord.gt(next, prev), s"stamp went backwards at $i")
      prev = next
      i += 1
    }
  }

  test("the counter advances inside a millisecond and resets when time moves") {
    val clock = new Fake(Epoch)
    val c = Hlc.at(clock.source)
    assertEquals(c.next().counter, 0)
    assertEquals(c.next().counter, 1)
    assertEquals(c.next().counter, 2)
    clock.advance(1)
    val moved = c.next()
    assertEquals(moved.millis, Epoch + 1)
    assertEquals(moved.counter, 0, "a fresh millisecond restarts the counter")
  }

  test("observe: after seeing a remote stamp, our own stamps sort above it") {
    // two nodes whose physical clocks disagree by an hour
    val ahead = Hlc.at(new Fake(Epoch + 3600000).source)
    val behind = Hlc.at(new Fake(Epoch).source)
    val fromAhead = ahead.next()
    val reply = behind.observe(fromAhead)
    assert(ord.gt(reply, fromAhead), "a reply must sort after the message it answers, even from a slow clock")
    var prev = reply
    var i = 0
    while (i < 50) {
      val n = behind.next()
      assert(ord.gt(n, prev), "the observed height was not kept")
      prev = n
      i += 1
    }
  }

  test("observe with a stamp from the past changes nothing") {
    val c = Hlc.at(new Fake(Epoch).source)
    val mine = c.next()
    val after = c.observe(Hlc(Epoch - 100000, 0))
    assert(ord.gt(after, mine), "observing the past must still move us forward")
    assertEquals(after.millis, Epoch, "and must not adopt the stale millisecond")
  }

  test("a narrow counter borrows a millisecond instead of overflowing") {
    val c = Hlc.at(new Fake(Epoch).source, counterBits = 2)   // 0..3, then borrow
    assertEquals(c.next().counter, 0)
    assertEquals(c.next().counter, 1)
    assertEquals(c.next().counter, 2)
    assertEquals(c.next().counter, 3)
    val borrowed = c.next()
    assertEquals(borrowed.millis, Epoch + 1)
    assertEquals(borrowed.counter, 0)
  }

  test("the packing refuses values it cannot hold") {
    intercept[IllegalArgumentException](Hlc(-1L, 0)): Unit
    intercept[IllegalArgumentException](Hlc(Epoch, -1)): Unit
    intercept[IllegalArgumentException](Hlc(Epoch, Hlc.MaxCounter + 1)): Unit
    intercept[IllegalArgumentException](Hlc(Hlc.MaxMillis + 1, 0)): Unit
  }

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
}
