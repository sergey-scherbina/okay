package okay

/**
 * The unbounded buffer's own tests, below the channel.
 *
 * They exist because the defect that made them was invisible from
 * above: `SentinelChannel` over `Segments` lost 498 consecutive
 * elements under CPU pressure and passed every run on a quiet box.
 * The cause was one line in `segmentFor` — the walk goes forward
 * only, and its hint can be AHEAD, because producers claim positions
 * with a single increment and then publish out of order. A thread
 * holding position 800 could find the hint already at the segment for
 * 1100, and write its element into another position's slot.
 *
 * So the stress here is deliberately many producers and small
 * segments: what has to be provoked is a claim that publishes late
 * while another claim runs ahead of it, across a boundary.
 */
class TestSegments extends munit.FunSuite {

  test("many producers, small segments: every element arrives exactly once") {
    for _ <- 1 to 5 do
      // 16 slots a segment, so 4 producers of 500 cross 125 of them
      val b = Segments[Int](segShift = 4)
      val per = 500
      val ps = (0 until 4).map(w => Thread.ofVirtual().start { () =>
        var i = 0
        while i < per do { val _ = b.push(w * per + i); i += 1 }
      })
      ps.foreach(_.join())
      val got = scala.collection.mutable.ArrayBuffer.empty[Int]
      var spins = 0
      while got.length < 4 * per && spins < 1000000 do
        b.pop() match
          case null => spins += 1
          case v => got += v.nn
      assertEquals(got.length, 4 * per, "elements went missing")
      assertEquals(got.toSet, (0 until 4 * per).toSet, "wrong elements arrived")
  }

  test("a bulk take spans segments and stays in order") {
    val b = Segments[Int](segShift = 4)
    (0 until 100).foreach(i => { val _ = b.push(i) })
    val out = scala.collection.mutable.ArrayBuffer.empty[Int]
    var guard = 0
    while out.length < 100 && guard < 1000 do
      val _ = b.popMany(100)(a => out += a)
      guard += 1
    assertEquals(out.toList, (0 until 100).toList, "a run crossing a boundary lost its order")
  }

  test("interleaved producers and a consumer lose nothing") {
    for _ <- 1 to 5 do
      val b = Segments[Int](segShift = 4)
      val per = 400
      val done = java.util.concurrent.atomic.AtomicInteger(0)
      val ps = (0 until 3).map(w => Thread.ofVirtual().start { () =>
        var i = 0
        while i < per do { val _ = b.push(w * per + i); i += 1 }
        done.incrementAndGet(): Unit
      })
      val got = scala.collection.mutable.ArrayBuffer.empty[Int]
      // BOUNDED: a consumer that waits for an element the buffer lost
      // waits for ever, and a test that can hang is worse than one
      // that fails -- it takes the gate with it
      val deadline = System.currentTimeMillis() + 10000
      val q = Thread.ofVirtual().start { () =>
        while got.length < 3 * per && System.currentTimeMillis() < deadline do
          b.pop() match
            case null => Thread.`yield`()
            case v => got += v.nn
      }
      ps.foreach(_.join()); q.join()
      assertEquals(got.toSet, (0 until 3 * per).toSet, "a concurrent consumer lost elements")
  }
}
