package okay

import scala.jdk.CollectionConverters.*

/**
 * ring-channel: the same accounting `TestChannel` puts StmChannel
 * through, aimed at RingChannel — deliberately, because that test is
 * what caught the previous attempt's deadlock (channel-ring-
 * integration). A new implementation earns its place by surviving the
 * test that killed the last one, not by passing new ones written to
 * fit it.
 */
class TestRingChannel extends munit.FunSuite {

  test("send/receive/close, the basics") {
    val c = RingChannel[Int](4)
    assert(c.offer(1)); assert(c.offer(2))
    assertEquals(c.receiveBlocking(), Some(1))
    assertEquals(c.receiveBlocking(), Some(2))
    c.close()
    assertEquals(c.receiveBlocking(), None)
    assert(!c.sendBlocking(3), "a closed channel took an element")
  }

  test("buffered elements still drain after close") {
    val c = RingChannel[Int](8)
    assert(c.offer(1)); assert(c.offer(2)); assert(c.offer(3))
    c.close()
    assertEquals(c.receiveBlocking(), Some(1))
    assertEquals(c.receiveBlocking(), Some(2))
    assertEquals(c.receiveBlocking(), Some(3))
    assertEquals(c.receiveBlocking(), None)
  }

  test("a full channel refuses an offer but takes a blocking send once room opens") {
    val c = RingChannel[Int](2)
    assert(c.offer(1)); assert(c.offer(2))
    assertEquals(c.offer(3), false)
    val t = Thread.ofVirtual().start(() => { val _ = c.sendBlocking(3) })
    assertEquals(c.receiveBlocking(), Some(1))
    t.join()
    assertEquals(c.receiveBlocking(), Some(2))
    assertEquals(c.receiveBlocking(), Some(3))
  }

  test("a receiver parked before anything arrives is woken by the send") {
    val c = RingChannel[Int](4)
    val got = java.util.concurrent.CompletableFuture.supplyAsync(() => c.receiveBlocking())
    Thread.sleep(20)   // let it park
    assert(c.offer(42))
    assertEquals(got.get(10, java.util.concurrent.TimeUnit.SECONDS), Some(42))
  }

  test("a receiver parked before close is woken by the close") {
    val c = RingChannel[Int](4)
    val got = java.util.concurrent.CompletableFuture.supplyAsync(() => c.receiveBlocking())
    Thread.sleep(20)
    c.close()
    assertEquals(got.get(10, java.util.concurrent.TimeUnit.SECONDS), None)
  }

  test("THE accounting test, the one that caught the last attempt") {
    // a producer sends as fast as it can; the consumer drains; the
    // main thread closes at an arbitrary moment. The invariant is the
    // accounting: the receiver's set is EXACTLY the accepted set --
    // no element lost after a true, none delivered after a false
    for round <- 1 to 200 do
      val c = RingChannel[Int](4)
      val accepted = java.util.concurrent.ConcurrentHashMap.newKeySet[Int]()
      val received = scala.collection.mutable.ArrayBuffer.empty[Int]
      val producer = Thread.ofVirtual().start { () =>
        var i = 0
        var on = true
        while on && i < 10000 do
          if c.sendBlocking(i) then accepted.add(i): Unit else on = false
          i += 1
      }
      val consumer = Thread.ofVirtual().start { () =>
        var go = true
        while go do c.receiveBlocking() match
          case Some(v) => received += v
          case None => go = false
      }
      Thread.sleep(0, (round % 7) * 100000)
      c.close()
      producer.join(); consumer.join()
      assertEquals(received.toSet, accepted.asScala.toSet, s"round $round")
      assertEquals(received.toList, received.toList.sorted, s"round $round: order")
  }
}
