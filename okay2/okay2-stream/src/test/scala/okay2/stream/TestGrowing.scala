package okay2.stream

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import okay2.async.CanBlock
import okay2.platform._

/** okay-stream's TestGrowing (okay2 spec stage 28): the laws a channel
 * that CHANGES ITS BUFFER under load owes — nothing lost or doubled, the
 * end seen, a producer waiting when it happened not left waiting, and
 * each producer's own order surviving the swap (at most one displacement). */
class TestGrowing extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(2, "min")

  private val cb: CanBlock = implicitly[CanBlock]

  private def drain(b: Buffer[Any]): List[Any] = Iterator.continually(b.pop()).takeWhile(_ != null).toList

  test("one producer never grows it: the buffer stays the ring it was") {
    val b = new Growing[Any](new Ring[Any](64), 8, () => new Ring[Any](8))
    var i = 0
    while (i < 64) { assert(b.push(i), s"push $i"); i += 1 }
    assertEquals(b.parts, 1, "a ring that was never refused must still be one part")
  }

  test("one producer, however many pushes, never grows it: one thread is not contention") {
    val b = new Growing[Any](new Ring[Any](4), 8, () => new Ring[Any](4))
    var round = 0
    while (round < 200) {
      var i = 0
      while (i < 4) { assert(b.push(i), s"push $i"); i += 1 }
      assert(!b.push(99), "a full ring refuses")
      while (b.pop() != null) ()
      round += 1
    }
    assertEquals(b.parts, 1, "one producer must not turn it into a partitioned buffer")
  }

  /** through a CHANNEL, with the producer really parking: a resumed send
   * runs on the consumer's thread and must not read as a second producer */
  test("one producer through a CHANNEL never grows it, however often it parks") {
    val Total = 8000
    val Cap = 1024
    var round = 0
    while (round < 30) {
      round += 1
      val buf = new Growing[Any](new Ring[Any](Cap), 16, () => new Ring[Any](Cap / 16))
      val c = new SentinelChannel[Int](buf)
      val p = Thread.ofVirtual().start { () =>
        var i = 0
        while (i < Total) { val _ = c.sendBlocking(i)(cb); i += 1 }
      }
      val closer = Thread.ofVirtual().start { () => p.join(); c.close() }
      var seen = 0
      var go = true
      while (go) {
        val chunk = cb.block[Either[Throwable, Chunk[Int]]] { k => c.receiveManyAsync(4096)(k); () => () }.fold(throw _, identity)
        if (chunk.length == 0) go = false else seen += chunk.length
      }
      p.join(); closer.join()
      assertEquals(seen, Total, s"round $round: every element exactly once")
      assertEquals(buf.parts, 1, s"round $round: one parked-and-resumed producer is not two")
    }
  }

  test("a second producer grows it, and what was already in it stays readable") {
    val b = new Growing[Any](new Ring[Any](4), 8, () => new Ring[Any](4))
    var i = 0
    while (i < 4) { assert(b.push(i), s"push $i"); i += 1 }
    assert(!b.push(99), "the first refused producer only remembers itself")
    val second = Thread.startVirtualThread { () => assert(b.push(99), "the second refused producer gets a part") }
    second.join()
    assert(b.parts >= 2, s"it must have grown, got ${b.parts}")
    val got = drain(b)
    assertEquals(got.length, 5, s"everything pushed must come out, got $got")
    assertEquals(got.filter(_ != 99), List[Any](0, 1, 2, 3), s"the ring's own order must survive, got $got")
    assert(got.contains(99), s"the second producer's element must be there, got $got")
  }

  test("P x C over a growing channel: every element exactly once, the end seen") {
    var round = 0
    while (round < 200) {
      val ch = Queues.strong[Int].growing(16, parts = 8).build
      val got = new AtomicInteger()
      val consumers = (0 until 4).map(_ => Thread.startVirtualThread { () =>
        var on = true
        while (on) ch.receiveBlocking()(cb) match {
          case Some(_) => val _ = got.incrementAndGet()
          case None => on = false
        }
      })
      val producers = (0 until 4).map(p => Thread.startVirtualThread { () =>
        var i = 0
        while (i < 40) { val _ = ch.sendBlocking(p * 100 + i)(cb); i += 1 }
      })
      producers.foreach(_.join())
      ch.close()
      consumers.foreach(_.join())
      assertEquals(got.get, 160, s"round $round")
      round += 1
    }
  }

  test("a producer parked on the full ring when it grows is not left there") {
    val ch = Queues.strong[Int].growing(4, parts = 8).build
    var i = 0
    while (i < 4) { val _ = ch.offer(i); i += 1 }
    val parked = (0 until 2).map(n => Thread.startVirtualThread { () => val _ = ch.sendBlocking(100 + n)(cb) })
    Thread.sleep(50)
    val reader = Thread.startVirtualThread { () =>
      var seen = 0
      while (seen < 6) { if (ch.receiveBlocking()(cb).isDefined) seen += 1 }
    }
    parked.foreach(_.join())   // hangs here if a parked sender is stranded by the swap
    reader.join()
    ch.close()
  }

  test("each producer's own order survives the swap — the shipped default, at most one displacement") {
    var round = 0
    while (round < 200) {
      val ch = Channel[Int](4)
      val live = new AtomicInteger(2)
      val producers = (0 until 2).map(p => Thread.startVirtualThread { () =>
        var i = p
        var going = true
        while (going && i < 400) { going = ch.sendBlocking(i)(cb); i += 2 }
        if (live.decrementAndGet() == 0) ch.close()
      })
      val got = List.newBuilder[Int]
      var open = true
      while (open) ch.receiveBlocking()(cb) match {
        case Some(v) => got += v
        case None => open = false
      }
      producers.foreach(_.join())
      val out = got.result()
      List(0, 1).foreach { p =>
        val own = out.filter(_ % 2 == p)
        val inversions = own.lazyZip(own.drop(1)).count { case (a, b) => a > b }
        assert(inversions <= 1, s"round $round: producer $p had $inversions inversions: ${own.take(40)}")
        assertEquals(own.distinct, own, s"round $round: producer $p duplicated an element")
      }
      round += 1
    }
  }

  /** the mechanism of growing-stale-route with the race taken out: a route
   * taken before the swap names part 0, which after it is the adopted part */
  test("a route taken before the swap does not send later elements into the adopted part") {
    val Cap = 4
    val b = new Growing[Any](new Ring[Any](Cap), 16, () => new Ring[Any](Cap))
    val flag = new AtomicBoolean(false)
    def at(route: Int, v: Int): Boolean = b.pushDecidingAt(route, v, flag, -999) != null
    val filled, grown, pushedEarly, part0Free, done = new CountDownLatch(1)
    @volatile var stale = -1
    val producer = Thread.ofPlatform().start { () =>
      stale = b.route()
      var i = 0
      while (i < Cap) { assert(at(stale, -(i + 1)), s"fill ${i + 1}"); i += 1 }
      assert(!at(stale, -99), "a full ring must refuse")
      filled.countDown(); grown.await()
      assert(at(stale, 27), "27"); assert(at(stale, 31), "31")
      pushedEarly.countDown(); part0Free.await()
      assert(at(stale, 33), "33")
      done.countDown()
    }
    filled.await()
    assertEquals(stale, 0, "an unpartitioned buffer routes everything to 0")
    assertEquals(b.parts, 1, "one producer must not have grown it")
    val other = Thread.ofPlatform().start(() => { val _ = at(0, 2) })
    other.join()
    assert(b.parts > 1, "two producers refused by a full ring must have grown it")
    grown.countDown()
    pushedEarly.await()
    var n = 0
    while (n < Cap) { assert(b.pop() != null, s"the filler ran out at $n"); n += 1 }
    part0Free.countDown()
    done.await()
    producer.join()
    val mine = drain(b).filter(v => v == 27 || v == 31 || v == 33)
    assertEquals(mine, List[Any](27, 31, 33),
      "a route taken before the swap put a later element in the adopted part, ahead of earlier ones")
  }

  test("an adopted part is read before the parts opened after it") {
    val adopted = new Ring[Any](8)
    var i = 0
    while (i < 4) { assert(adopted.push(i), s"seed $i"); i += 1 }
    val b = new AdaptiveFifo[Any](4, () => new Ring[Any](8), eager = false, first = adopted)
    (1 until 4).map(p => Thread.startVirtualThread { () =>
      var j = 0
      while (j < 4) { assert(b.push(100 * p + j), s"part $p push $j"); j += 1 }
    }).foreach(_.join())
    val got = drain(b)
    assertEquals(got.length, 16, s"everything pushed must come out, got $got")
    assertEquals(got.take(4), List[Any](0, 1, 2, 3), s"the adopted part must be read out first, got $got")
  }

  test("no producer makes its home in the adopted part") {
    val adopted = new Ring[Any](8)
    assert(adopted.push(0), "seed")
    val b = new AdaptiveFifo[Any](3, () => new Ring[Any](8), eager = false, first = adopted)
    val homes = new ConcurrentLinkedQueue[Integer]()
    (0 until 8).map(_ => Thread.startVirtualThread { () => val _ = homes.add(b.route()) }).foreach(_.join())
    val seen = homes.toArray.toList.map(_.asInstanceOf[Integer].intValue)
    assertEquals(seen.length, 8)
    assert(!seen.contains(0), s"the adopted part must be nobody's home, got $seen")
  }
}
