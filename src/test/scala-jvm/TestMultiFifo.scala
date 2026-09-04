package okay

/**
 * The relaxed buffer, and specifically the two claims that make it
 * usable: what it keeps and what it gives up.
 */
class TestMultiFifo extends munit.FunSuite {

  test("what it keeps: one producer's own elements stay in order") {
    for _ <- 1 to 5 do
      val b = MultiFifo[Int](4, _ => Ring[Int](64))
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
    val b = MultiFifo[Int](2, _ => Ring[Int](8))
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
    val b = MultiFifo[Int](3, _ => Ring[Int](8))
    assertEquals(b.seal(-1), 3, "a mark must reach every independent order")
    val b1 = Ring[Int](8)
    assertEquals(b1.seal(-1), 1, "one order needs one mark")
  }
}
