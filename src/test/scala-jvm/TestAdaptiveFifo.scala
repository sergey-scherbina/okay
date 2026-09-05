package okay

/**
 * The buffer that grows parts as producers appear, and the one window
 * that design opens.
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
    val c = Queues.strong[Int].adaptive.parts(4).bounded(32).build
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
}
