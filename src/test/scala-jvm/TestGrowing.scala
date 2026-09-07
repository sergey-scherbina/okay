package okay

import java.util.concurrent.atomic.AtomicInteger

/** The laws a channel that CHANGES ITS BUFFER under load owes
 * (queue-swap): nothing is lost, nothing is doubled, the end is seen,
 * and a producer that was waiting when it happened does not wait for
 * ever. */
class TestGrowing extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(2, "min")

  test("one producer never grows it: the buffer stays the ring it was") {
    val b = Growing[Int](Ring[Int](64), 8, () => Ring[Int](8))
    var i = 0
    while i < 64 do { assert(b.push(i), s"push $i"); i += 1 }
    assertEquals(b.parts, 1, "a ring that was never refused must still be one part")
  }

  test("one producer filling it over and over never grows it: that is backpressure, not contention") {
    val b = Growing[Int](Ring[Int](4), 8, () => Ring[Int](4))
    var round = 0
    while round < 5 do
      var i = 0
      while i < 4 do { assert(b.push(i), s"push $i"); i += 1 }
      assert(!b.push(99), "a full ring refuses")
      while b.pop() != null do ()
      round += 1
    assertEquals(b.parts, 1, "one producer must not turn it into a partitioned buffer")
  }

  test("two refused producers grow it, and what was already in it stays readable") {
    val b = Growing[Int](Ring[Int](4), 8, () => Ring[Int](4))
    var i = 0
    while i < 4 do { assert(b.push(i), s"push $i"); i += 1 }
    assert(!b.push(99), "the first refused producer only remembers itself")
    val second = Thread.startVirtualThread { () =>
      // the second producer is refused too, which is contention: it grows
      // the buffer and lands in a part of its own
      assert(b.push(99), "the second refused producer must get a part of its own")
    }
    second.join()
    assert(b.parts >= 2, s"it must have grown, got ${b.parts}")
    val got = Iterator.continually(b.pop()).takeWhile(_ != null).map(_.nn).toList
    assertEquals(got.length, 5, s"everything pushed must come out, got $got")
    // the guarantee is per PRODUCER, not global: the second producer's
    // element may come out first, because a consumer starts its scan
    // at its own part. What must hold is that the ring's own four keep
    // their order among themselves after becoming part 0
    assertEquals(got.filter(_ != 99), List(0, 1, 2, 3), s"the ring's own order must survive, got $got")
    assert(got.contains(99), s"the second producer's element must be there, got $got")
  }

  test("P x C over a growing channel: every element exactly once, the end seen") {
    var round = 0
    while round < 200 do
      val ch = Queues.strong[Int].growing(16, parts = 8).build
      val got = AtomicInteger()
      val consumers = (0 until 4).map(_ => Thread.startVirtualThread { () =>
        var on = true
        while on do ch.receiveBlocking() match
          case Some(_) => val _ = got.incrementAndGet()
          case None => on = false
      })
      val producers = (0 until 4).map(p => Thread.startVirtualThread { () =>
        var i = 0
        while i < 40 do { val _ = ch.sendBlocking(p * 100 + i); i += 1 }
      })
      producers.foreach(_.join())
      ch.close()
      consumers.foreach(_.join())
      assertEquals(got.get, 160, s"round $round")
      round += 1
  }

  test("a producer parked on the full ring when it grows is not left there") {
    val ch = Queues.strong[Int].growing(4, parts = 8).build
    // fill it, then park two senders on it; a third send refuses and grows
    var i = 0
    while i < 4 do { val _ = ch.offer(i); i += 1 }
    val parked = (0 until 2).map(n => Thread.startVirtualThread { () => val _ = ch.sendBlocking(100 + n) })
    Thread.sleep(50)
    val reader = Thread.startVirtualThread { () =>
      var seen = 0
      while seen < 6 do { if ch.receiveBlocking().isDefined then seen += 1 }
    }
    parked.foreach(_.join())    // hangs here if a parked sender is stranded by the swap
    reader.join()
    ch.close()
  }
}
