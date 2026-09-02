package okay

import !.*
import scala.jdk.CollectionConverters.*

/** Channels: the concurrency primitive of streams — merge and buffer. */
class TestChannel extends munit.FunSuite {

  test("a channel is a linear async stream: send, close, drain") {
    val c = Channel[Int]()
    assert(c.send(1)); assert(c.send(2)); c.close()
    assertEquals(c.toLazyList.toList, List(1, 2))
    assertEquals(c.receive(), None)
  }

  test("send after close is refused, not thrown: false, the element dropped, the end unchanged") {
    val c = Channel[Int]()
    assert(c.send(1)); c.close()
    assert(!c.send(2), "a closed channel took an element")
    assertEquals(c.receive(), Some(1))
    assertEquals(c.receive(), None)
    assert(!c.send(3))
    assertEquals(c.receive(), None)
  }

  test("the send/close race is exact: every accepted send is received, every refused one is not") {
    // a producer sends as fast as it can; the consumer drains; the
    // main thread closes at an arbitrary moment. The invariant is
    // the accounting: the receiver's set is EXACTLY the accepted set,
    // whatever the interleaving — no element lost after a true, none
    // delivered after a false
    for round <- 1 to 200 do
      val c = Channel[Int](capacity = 4)
      val accepted = java.util.concurrent.ConcurrentHashMap.newKeySet[Int]()
      val received = scala.collection.mutable.ArrayBuffer.empty[Int]
      val producer = Thread.ofVirtual().start { () =>
        var i = 0
        var on = true
        while on && i < 10000 do
          if c.send(i) then accepted.add(i): Unit else on = false
          i += 1
      }
      val consumer = Thread.ofVirtual().start { () =>
        var go = true
        while go do c.receive() match
          case Some(v) => received += v
          case None => go = false
      }
      Thread.sleep(0, (round % 7) * 100000)
      c.close()
      producer.join(); consumer.join()
      assertEquals(received.toSet, accepted.asScala.toSet, s"round $round")
      assertEquals(received.toList, received.toList.sorted, s"round $round: order")
  }

  test("merge combines two async streams by readiness") {
    type S = Produce + Async
    def ticks(xs: List[Int]): Int ! S = xs match
      case Nil => pure(0)
      case x :: t => effect[S, Unit](Async.Run(() => Thread.sleep(1))).flatMap: _ =>
        effect[S, Int](x).flatMap(_ => ticks(t))
    val merged = Channel.merge(ticks(List(1, 3, 5)), ticks(List(2, 4, 6)))
    assertEquals(merged.toLazyList.toList.sorted, List(1, 2, 3, 4, 5, 6))
  }

  test("merge joins two sources of DIFFERENT types, as their union") {
    def ticks[A](xs: List[A]): Source[A] =
      xs.foldRight(pure[Writer % A + Async, Unit](())): (x, rest) =>
        effect[Writer % A + Async, Unit](Async.Run(() => Thread.sleep(1)))
          .flatMap(_ => effect[Writer % A + Async, Unit](Writer(x)))
          .flatMap(_ => rest)

    val merged: Source[Int | String] = ticks(List(1, 3, 5)) merge ticks(List("a", "b"))
    val got = merged.toLazyList.toList
    // interleaved by readiness, but each side keeps its own order
    assertEquals(got.collect { case i: Int => i }, List(1, 3, 5))
    assertEquals(got.collect { case s: String => s }, List("a", "b"))
  }

  test("merge starts its fibers at the first pull, not before") {
    val pulled = java.util.concurrent.atomic.AtomicInteger(0)
    def counted: Source[Int] =
      effect[Writer % Int + Async, Int](Async.Run(() => pulled.incrementAndGet()))
        .flatMap(n => effect[Writer % Int + Async, Unit](Writer(n)))

    val merged = counted merge counted
    Thread.sleep(20)
    assertEquals(pulled.get(), 0, "a source nobody consumes drained anyway")
    assertEquals(merged.toLazyList.toList.length, 2)
    assertEquals(pulled.get(), 2)
  }

  test("merge is CONCURRENT: the sources overlap, chained or not") {
    def slow[A](ms: Long)(as: List[A]): Source[A] =
      as.foldRight(pure[Writer % A + Async, Unit](())): (a, rest) =>
        effect[Writer % A + Async, Unit](Async.Run(() => Thread.sleep(ms)))
          .flatMap(_ => effect[Writer % A + Async, Unit](Writer(a)))
          .flatMap(_ => rest)

    def took(body: => Any): Long =
      val t = System.nanoTime(); body; (System.nanoTime() - t) / 1000000

    // eight sources, three elements each, 100ms an element: 2.4s of
    // work. Chaining merges does not serialize them — each hop is its
    // own fiber, so the chain is a pipeline, not a queue of turns.
    val eight = List.fill(8)(slow(100)(List(1, 2, 3)))
    val t = took(assertEquals(eight.reduce(_ merge _).toLazyList.length, 24))
    assert(t < 1000, s"eight merged sources took ${t}ms — they did not overlap")
  }

  test("merge answers by READINESS: a silent source holds up nobody") {
    val silent: Source[String] =
      effect[Writer % String + Async, Unit](Async.Run(() => Thread.sleep(300)))
        .flatMap(_ => effect[Writer % String + Async, Unit](Writer("late")))
    val t0 = System.nanoTime()
    val first = (Source.of((1 to 10).toList) merge silent).toLazyList.take(10).toList
    val ms = (System.nanoTime() - t0) / 1000000
    assertEquals(first.collect { case i: Int => i }.length, 10)
    assert(ms < 200, s"the fast source waited ${ms}ms on the slow one")
  }

  test("merge is BOUNDED by default: an endless source does not run away") {
    def endless(produced: java.util.concurrent.atomic.AtomicInteger): Source[Int] =
      effect[Writer % Int + Async, Int](Async.Run(() => produced.incrementAndGet()))
        .flatMap(n => effect[Writer % Int + Async, Unit](Writer(n)))
        .flatMap(_ => endless(produced))

    val p = java.util.concurrent.atomic.AtomicInteger(0)
    assertEquals((endless(p) merge Source(0)).toLazyList.take(10).toList.length, 10)
    Thread.sleep(200)   // whatever the fibers were going to do, they have done
    // the default capacity is 64: the producer parks a bounded distance
    // ahead. Unbounded, the same ten pulls measured 1 269 819 produced
    // elements and climbing — which for an endless source is the heap.
    assert(p.get() < 1000, s"the producer ran ${p.get()} elements ahead of ten pulls")
  }

  test("buffer runs the producer ahead, at most capacity elements") {
    val c = Channel.buffer(2)(fibs[Long, Producer].take(10): LazyList[Long])
    assertEquals(c.toLazyList.toList, fibs[Long, LazyList].take(10).toList)
  }
}
