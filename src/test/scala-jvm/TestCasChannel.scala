package okay

import scala.jdk.CollectionConverters.*

/** ring-channel: the same accounting, aimed at the unbounded MS
 * implementation. Unbounded means a send never parks, so the
 * interesting races are all on the receive and close sides. */
class TestCasChannel extends munit.FunSuite {

  test("send/receive/close, the basics") {
    val c = CasChannel[Int]()
    assert(c.offer(1)); assert(c.offer(2))
    assertEquals(c.receiveBlocking(), Some(1))
    assertEquals(c.receiveBlocking(), Some(2))
    c.close()
    assertEquals(c.receiveBlocking(), None)
    assert(!c.sendBlocking(3), "a closed channel took an element")
  }

  test("buffered elements still drain after close") {
    val c = CasChannel[Int]()
    (1 to 100).foreach(i => assert(c.offer(i)))
    c.close()
    assertEquals((1 to 100).map(_ => c.receiveBlocking()).toList, (1 to 100).map(Some(_)).toList)
    assertEquals(c.receiveBlocking(), None)
  }

  test("a receiver parked before anything arrives is woken by the send") {
    val c = CasChannel[Int]()
    val got = java.util.concurrent.CompletableFuture.supplyAsync(() => c.receiveBlocking())
    Thread.sleep(20)
    assert(c.offer(42))
    assertEquals(got.get(10, java.util.concurrent.TimeUnit.SECONDS), Some(42))
  }

  test("a receiver parked before close is woken by the close") {
    val c = CasChannel[Int]()
    val got = java.util.concurrent.CompletableFuture.supplyAsync(() => c.receiveBlocking())
    Thread.sleep(20)
    c.close()
    assertEquals(got.get(10, java.util.concurrent.TimeUnit.SECONDS), None)
  }

  test("THE accounting test, unbounded shape") {
    for round <- 1 to 200 do
      val c = CasChannel[Int]()
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

  test("many producers, one consumer: nothing lost or duplicated") {
    val c = CasChannel[Int]()
    val n = 4000
    val ts = (0 until 4).map(p => Thread.ofVirtual().start { () =>
      (0 until n).foreach(i => { val _ = c.sendBlocking(p * n + i) })
    })
    val got = scala.collection.mutable.ArrayBuffer.empty[Int]
    val consumer = Thread.ofVirtual().start { () =>
      var go = true
      while go do c.receiveBlocking() match
        case Some(v) => got += v
        case None => go = false
    }
    ts.foreach(_.join()); c.close(); consumer.join()
    assertEquals(got.length, 4 * n)
    assertEquals(got.toSet, (0 until 4 * n).toSet)
  }
}
