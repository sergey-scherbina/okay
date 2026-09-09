package okay

import java.util.concurrent.atomic.AtomicInteger

/** The laws a channel that CHANGES ITS BUFFER under load owes
 * (queue-swap): nothing is lost, nothing is doubled, the end is seen,
 * a producer that was waiting when it happened does not wait for
 * ever -- and EACH PRODUCER'S OWN ORDER SURVIVES THE SWAP.
 *
 * That last one was missing from this list until 2026-09-09, and the
 * omission is exactly how it broke: `growing` gives up FIFO across
 * producers on purpose and says so, while promising per-producer
 * order in the same breath, and no law here asked for it
 * (merge-chunked-order). */
class TestGrowing extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(2, "min")

  test("one producer never grows it: the buffer stays the ring it was") {
    val b = Growing[Int](Ring[Int](64), 8, () => Ring[Int](8))
    var i = 0
    while i < 64 do { assert(b.push(i), s"push $i"); i += 1 }
    assertEquals(b.parts, 1, "a ring that was never refused must still be one part")
  }

  test("one producer, however many pushes, never grows it: one thread is not contention") {
    val b = Growing[Int](Ring[Int](4), 8, () => Ring[Int](4))
    var round = 0
    while round < 200 do        // well past the sampling period
      var i = 0
      while i < 4 do { assert(b.push(i), s"push $i"); i += 1 }
      assert(!b.push(99), "a full ring refuses")
      while b.pop() != null do ()
      round += 1
    assertEquals(b.parts, 1, "one producer must not turn it into a partitioned buffer")
  }

  /** The two laws above drive the BUFFER, from one thread, and that
   * is the wrong layer: they cannot see the path that actually broke
   * it. A channel whose buffer fills parks the sender behind a
   * continuation and runs that continuation from whichever thread
   * frees a slot — the consumer's. `Growing` read the caller's
   * identity there, saw a thread that was not the producer, and grew.
   * Measured before the fix: ten runs in thirty (growing-onep).
   *
   * So the law is stated through a CHANNEL, with a consumer running,
   * and with more elements than the buffer holds so the producer
   * really does park. */
  test("one producer through a CHANNEL never grows it, however often it parks") {
    val Total = 8000
    val Cap = 1024
    var round = 0
    while round < 30 do          // p(miss) per round was ~2/3 before the fix
      round += 1
      type E = Int | Mark
      val buf = Growing[E](Ring[E](Cap), 16, () => Ring[E](Cap / 16))
      val c = SentinelChannel[Int](buf)

      val p = Thread.ofVirtual().start { () =>
        var i = 0
        while i < Total do { val _ = c.sendBlocking(i); i += 1 }
      }
      val closer = Thread.ofVirtual().start { () => p.join(); c.close() }

      val cb = summon[CanBlock]
      var seen = 0
      var go = true
      while go do
        val chunk = cb.block[Either[Throwable, Chunk[Int]]] { k =>
          c.receiveManyAsync(4096)(k); () => ()
        }.fold(throw _, identity)
        if chunk.length == 0 then go = false else seen += chunk.length
      p.join(); closer.join()

      assertEquals(seen, Total, s"round $round: every element exactly once")
      assertEquals(buf.parts, 1,
        s"round $round: ONE producer parked and was resumed by the consumer; " +
        "that resume is not a second producer and must not partition the buffer")
    ()
  }

  test("a second producer grows it, and what was already in it stays readable") {
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

  /**
   * EACH PRODUCER'S OWN ORDER SURVIVES THE SWAP.
   *
   * The promise is `growing`'s own, made where it gives up the other
   * one: "each producer keeps its own order and nothing is promised
   * between them". It is the promise `Source.merge` rests on, and a
   * merge is where it was caught failing -- one source's later chunk
   * came out ahead of its earlier ones (merge-chunked-order).
   *
   * The producers here are plain THREADS, deliberately. A fibre can
   * change carrier threads between sends, so a thread-keyed route
   * could be blamed for the reordering; a thread cannot, so what is
   * left is the swap itself. A producer that is not the one holding
   * part 0 pushed into the ring BEFORE it grew and is routed
   * elsewhere AFTER, while those earlier elements are still in part 0
   * -- and the parts drain independently.
   *
   * Measured before the fix, 300 rounds: the part-0 holder broke 0
   * times and the other producer 73.
   */
  test("each producer's own order survives the swap") {
    var round = 0
    while round < 200 do
      // the SHIPPED construction, not a hand-built mechanism: this is
      // what `Channel(4)` gives every caller since growing became the
      // default (2026-09-08), and the promise is made about that
      val ch = Channel[Int](4)
      val live = AtomicInteger(2)
      // odd elements are one producer's, even the other's, so each
      // producer's own subsequence is recoverable from the drain
      val producers = (0 until 2).map(p => Thread.startVirtualThread { () =>
        var i = p
        var going = true
        while going && i < 400 do { going = ch.sendBlocking(i); i += 2 }
        if live.decrementAndGet() == 0 then ch.close()
      })
      val got = List.newBuilder[Int]
      var open = true
      while open do ch.receiveBlocking() match
        case Some(v) => got += v
        case None => open = false
      producers.foreach(_.join())
      val out = got.result()
      List(0, 1).foreach { p =>
        val own = out.filter(_ % 2 == p)
        assertEquals(own, own.sorted, s"round $round: producer $p came back out of its own order")
      }
      round += 1
  }

  /**
   * The same guarantee stated structurally, so that it does not
   * depend on a race to show: an ADOPTED part is read before any part
   * opened after it.
   *
   * The round-based law above needs the buffer to grow and the parts
   * to be read in an unlucky order, so it fails only most of the
   * time. This one cannot be lucky. Part 0 is handed to the buffer
   * already holding elements -- which is exactly what `growing` does
   * with the ring it swaps out -- and the parts opened afterwards are
   * filled before anything is read. Whatever part the consumer's
   * rotation would have started on, the first element out belongs to
   * the adopted one.
   */
  test("an adopted part is read before the parts opened after it") {
    val adopted = Ring[Int](8)
    var i = 0
    while i < 4 do { assert(adopted.push(i), s"seed $i"); i += 1 }
    val b = AdaptiveFifo[Int](4, () => Ring[Int](8), eager = false, first = adopted)
    // three more producers, each taking a part of its own
    (1 until 4).map(p => Thread.startVirtualThread { () =>
      var j = 0
      while j < 4 do { assert(b.push(100 * p + j), s"part $p push $j"); j += 1 }
    }).foreach(_.join())
    val got = Iterator.continually(b.pop()).takeWhile(_ != null).map(_.nn).toList
    assertEquals(got.length, 16, s"everything pushed must come out, got $got")
    assertEquals(got.take(4), List(0, 1, 2, 3),
      s"the adopted part must be read out first, got $got")
  }

  /**
   * No producer's home is the adopted part, however many producers
   * turn up.
   *
   * The adopted part is read before the parts opened after it, so a
   * producer that made its home there would refill what everyone
   * else waits behind. With more producers than parts they share, and
   * the wrap-around handed out part 0 like any other until this law
   * (merge-chunked-order, 2026-09-09) — an ordering rule turned into
   * a fairness bug exactly when the parts ran out.
   */
  test("no producer makes its home in the adopted part") {
    val adopted = Ring[Int](8)
    assert(adopted.push(0), "seed")
    // three parts for eight producers: they must share, and they must
    // share over parts 1 and 2 only
    val b = AdaptiveFifo[Int](3, () => Ring[Int](8), eager = false, first = adopted)
    val homes = java.util.concurrent.ConcurrentLinkedQueue[Int]()
    (0 until 8).map(_ => Thread.startVirtualThread { () => homes.add(b.route()): Unit })
      .foreach(_.join())
    val seen = homes.toArray.toList.map(_.asInstanceOf[Int])
    assertEquals(seen.length, 8)
    assert(!seen.contains(0), s"the adopted part must be nobody's home, got $seen")
  }
}
