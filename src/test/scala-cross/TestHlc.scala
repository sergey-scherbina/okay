package okay

import okay.Hlc.{millis, counter}

/**
 * The clock's own laws. `Uid` tests it from above; these test the
 * thing the arc's other half — an LWW register — will lean on, which
 * is `observe`: the HYBRID part, and the part a sortable id never
 * exercises.
 */
class TestHlc extends munit.FunSuite {

  final class Fake(var t: Long):
    def source: () => Long = () => t
    def advance(by: Long): Unit = t += by
    def stepBack(by: Long): Unit = t -= by

  private val Epoch = 1_700_000_000_000L

  test("a stamp carries its millisecond and its counter, and packs into one Long") {
    val s = Hlc(Epoch, 7)
    assertEquals(s.millis, Epoch)
    assertEquals(s.counter, 7)
    assertEquals(Hlc.fromLong(s.toLong), s)
  }

  test("stamps from one clock never decrease, whatever the physical clock does") {
    val clock = Fake(Epoch)
    val c = Hlc.at(clock.source)
    val ord = summon[Ordering[Hlc.Stamp]]
    var prev = c.next()
    var i = 0
    while i < 500 do
      if i % 7 == 0 then clock.stepBack(1000) else clock.advance(1)
      val next = c.next()
      assert(ord.gt(next, prev), s"stamp went backwards at $i")
      prev = next
      i += 1
  }

  test("the counter advances inside a millisecond and resets when time moves") {
    val clock = Fake(Epoch)
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
    val ahead = Hlc.at(Fake(Epoch + 3_600_000).source)
    val behind = Hlc.at(Fake(Epoch).source)
    val ord = summon[Ordering[Hlc.Stamp]]

    val fromAhead = ahead.next()
    val reply = behind.observe(fromAhead)
    assert(ord.gt(reply, fromAhead),
      "a reply must sort after the message it answers, even from a slow clock")

    // and the slow node stays above it from then on, with no more observing
    var prev = reply
    var i = 0
    while i < 50 do
      val n = behind.next()
      assert(ord.gt(n, prev), "the observed height was not kept")
      prev = n
      i += 1
  }

  test("observe with a stamp from the past changes nothing") {
    val clock = Fake(Epoch)
    val c = Hlc.at(clock.source)
    val mine = c.next()
    val ord = summon[Ordering[Hlc.Stamp]]
    val old = Hlc(Epoch - 100_000, 0)
    val after = c.observe(old)
    assert(ord.gt(after, mine), "observing the past must still move us forward")
    assertEquals(after.millis, Epoch, "and must not adopt the stale millisecond")
  }

  test("a narrow counter borrows a millisecond instead of overflowing") {
    val clock = Fake(Epoch)
    val c = Hlc.at(clock.source, counterBits = 2)   // 0..3, then borrow
    assertEquals(c.next().counter, 0)
    assertEquals(c.next().counter, 1)
    assertEquals(c.next().counter, 2)
    assertEquals(c.next().counter, 3)
    val borrowed = c.next()
    assertEquals(borrowed.millis, Epoch + 1)
    assertEquals(borrowed.counter, 0)
  }

  test("the packing refuses values it cannot hold") {
    // `val _ =` because `intercept` ANSWERS the exception, and an
    // answer thrown away is a warning in this build
    val _ = intercept[IllegalArgumentException](Hlc(-1L, 0))
    val _ = intercept[IllegalArgumentException](Hlc(Epoch, -1))
    val _ = intercept[IllegalArgumentException](Hlc(Epoch, Hlc.MaxCounter + 1))
    val _ = intercept[IllegalArgumentException](Hlc(Hlc.MaxMillis + 1, 0))
  }
}
