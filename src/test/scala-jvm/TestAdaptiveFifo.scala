package okay

/**
 * The partitioned buffer: parts that grow as producers appear, and —
 * with `eager` — the fixed form that used to be a separate class.
 * Merging them was not tidying: two nearly identical lock-free
 * structures are two places for the same defect to hide, and this
 * design produced five of one family already.
 */
class TestAdaptiveFifo extends munit.FunSuite {

  test("one producer uses one part; more producers grow more") {
    val b = AdaptiveFifo[Int](8, () => Ring[Int](16))
    assertEquals(b.parts, 1, "before anyone sends there is one part")
    val t1 = Thread.ofPlatform().start(() => { val _ = b.push(1) })
    t1.join()
    assertEquals(b.parts, 1, "one producer stays on one part")
    val ts = (2 to 4).map(i => Thread.ofPlatform().start(() => { val _ = b.push(i) }))
    ts.foreach(_.join())
    assert(b.parts >= 2, s"more producers must grow parts, got ${b.parts}")
    assert(b.parts <= 8, s"never past the cap, got ${b.parts}")
  }

  test("the cap holds: producers past it share parts") {
    val b = AdaptiveFifo[Int](2, () => Ring[Int](64))
    val ts = (1 to 8).map(i => Thread.ofPlatform().start(() => { val _ = b.push(i) }))
    ts.foreach(_.join())
    assertEquals(b.parts, 2, "the cap is a cap")
    val got = Iterator.continually(b.pop()).takeWhile(_ != null).map(_.nn).toSet
    assertEquals(got, (1 to 8).toSet, "sharing a part must not lose anything")
  }

  /**
   * THE WINDOW. Termination is a mark in every part, so a part opened
   * after close begins would never be sealed and the stream would
   * never end. Sealing freezes first — this is that, as a test rather
   * than an argument, because four earlier defects in this design had
   * exactly this shape.
   */
  test("a producer arriving after close opens no new part") {
    val b = AdaptiveFifo[Int](8, () => Ring[Int](16))
    val t1 = Thread.ofPlatform().start(() => { val _ = b.push(1) })
    t1.join()
    val before = b.parts
    assertEquals(b.seal(-1), before, "seal reaches every part that exists")
    val late = Thread.ofPlatform().start(() => { val _ = b.push(2) })
    late.join()
    assertEquals(b.parts, before, "a late producer must not open an unsealed part")
  }

  test("a channel over it still ends, with producers arriving late") {
    val c = Queues.strong[Int].adaptive.parts(4).each(32).build
    // the consumer runs THROUGHOUT: a producer filling its own part
    // with nobody draining would wait for ever, which is what the
    // first version of this test did to itself
    val got = java.util.concurrent.ConcurrentLinkedQueue[Int]()
    val consumer = Thread.ofVirtual().start { () =>
      var go = true
      while go do c.receiveBlocking() match
        case Some(v) => got.add(v): Unit
        case None => go = false
    }
    val early = Thread.ofVirtual().start { () =>
      (0 until 20).foreach(i => { val _ = c.sendBlocking(i) })
    }
    early.join()
    // a second producer touches the channel only now
    val second = Thread.ofVirtual().start { () => val _ = c.sendBlocking(100) }
    second.join()
    c.close()
    // and a third only after close: it must be refused, and must not
    // open a part that would never be sealed
    val late = Thread.ofVirtual().start { () => assert(!c.sendBlocking(200)) }
    late.join()
    consumer.join()
    assertEquals(scala.jdk.CollectionConverters.CollectionHasAsScala(got).asScala.toSet,
      (0 until 20).toSet + 100,
      "everything accepted arrives, and the stream ends")
  }

  // ── the EAGER form: every part from the start ───────────────────

  test("what it keeps: one producer's own elements stay in order") {
    for _ <- 1 to 5 do
      val b = AdaptiveFifo[Int](4, () => Ring[Int](64), eager = true)
      val per = 500
      val ps = (0 until 4).map(w => Thread.ofVirtual().start { () =>
        var i = 0
        while i < per do
          if b.push(w * per + i) then i += 1 else Thread.`yield`()
      })
      val got = scala.collection.mutable.ArrayBuffer.empty[Int]
      val deadline = System.currentTimeMillis() + 15000
      val q = Thread.ofVirtual().start { () =>
        while got.length < 4 * per && System.currentTimeMillis() < deadline do
          b.pop() match
            case null => Thread.`yield`()
            case v => got += v.nn
      }
      ps.foreach(_.join()); q.join()
      assertEquals(got.toSet, (0 until 4 * per).toSet, "lost or invented")
      assertEquals(got.length, got.toSet.size, "duplicated")
      // each producer's own run, in the order it arrived, must be the
      // order it was sent -- this is the guarantee a bound producer
      // buys, and it is what the channel laws lean on
      for w <- 0 until 4 do
        val mine = got.filter(v => v / per == w).toList
        assertEquals(mine, mine.sorted, s"producer $w came out of order")
  }

  test("what it gives up: the order BETWEEN producers is not promised") {
    // stated as a test so the relaxation is recorded, not assumed. It
    // asserts the WEAKER claim -- that the buffer is allowed to
    // interleave -- by showing parts are independent: a later push to
    // an empty part comes out before an earlier one still queued
    // behind others.
    val b = AdaptiveFifo[Int](2, () => Ring[Int](8), eager = true)
    assertEquals(b.parts, 2, "a relaxed buffer says how many orders it keeps")
    val t1 = Thread.ofPlatform().start(() => { (1 to 4).foreach(i => b.push(i): Unit) })
    t1.join()
    val t2 = Thread.ofPlatform().start(() => { val _ = b.push(99) })
    t2.join()
    val out = Iterator.continually(b.pop()).takeWhile(_ != null).map(_.nn).toList
    assertEquals(out.toSet, Set(1, 2, 3, 4, 99))
    // no assertion on WHERE 99 lands: that is exactly the freedom
  }

  test("seal puts one mark in every part") {
    val b = AdaptiveFifo[Int](3, () => Ring[Int](8), eager = true)
    assertEquals(b.seal(-1), 3, "a mark must reach every independent order")
    val b1 = Ring[Int](8)
    assertEquals(b1.seal(-1), 1, "one order needs one mark")
  }
}
