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

  test("consumers taking in bulk: no loss, no duplication, no crash") {
    // The shape that found the defect, and the parameters matter.
    // ONE consumer cannot see it: the read pass re-derived its segment
    // from `headSeg`, and only ANOTHER consumer can advance that hint
    // past a run this one already claimed. The read then walks a later
    // segment and hits a null slot, and the exception dies unseen
    // inside a virtual thread.
    //
    // The numbers were tuned until the test actually caught it, and
    // that took two tries. Two consumers at sixteen slots a segment
    // passed WITH the defect present; so did three, unless the box
    // was loaded — and a test that needs a busy machine is not a
    // test. What catches it is CONTENTION FOR THE HINT: four slots to
    // a segment, so a 64-element run crosses sixteen boundaries, and
    // eight consumers, so someone is nearly always advancing
    // `headSeg` while someone else is in a read pass. At those
    // settings the defect fails this test and the fix passes it in
    // under half a second. The deadline is there so a future
    // regression fails rather than hangs.
    val b0 = Segments[Int](segShift = 2)
    assertEquals(b0.capacity, Int.MaxValue, "an unbounded buffer has no bound")
    for _ <- 1 to 20 do
      val b = Segments[Int](segShift = 2)
      val total = 4000
      val p = Thread.ofVirtual().start { () =>
        var i = 0
        while i < total do { val _ = b.push(i); i += 1 }
      }
      val seen = java.util.concurrent.ConcurrentLinkedQueue[Int]()
      val n = java.util.concurrent.atomic.AtomicInteger(0)
      // a consumer that dies silently turns this into a hang, so it
      // reports instead -- that is exactly how the defect hid
      val failure = java.util.concurrent.atomic.AtomicReference[Throwable | Null](null)
      val deadline = System.currentTimeMillis() + 15000
      val qs = (0 until 8).map(_ => Thread.ofVirtual().start { () =>
        try
          while n.get < total && System.currentTimeMillis() < deadline do
            val took = b.popMany(64)(a => { seen.add(a): Unit; n.incrementAndGet(): Unit })
            if took == 0 then Thread.`yield`()
        catch case e: Throwable => failure.compareAndSet(null, e): Unit
      })
      p.join(); qs.foreach(_.join())
      val err = failure.get
      assert(err == null, s"a consumer threw: $err")
      val got = scala.jdk.CollectionConverters.CollectionHasAsScala(seen).asScala.toList
      assertEquals(got.length, got.toSet.size, "duplicated")
      assertEquals(got.toSet, (0 until total).toSet, "lost")
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
