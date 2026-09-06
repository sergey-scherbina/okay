package okay.reactive

import java.util.concurrent.Flow
import okay.*
import okay.given

/**
 * What the bridge does, in terms a caller recognises — the TCK checks
 * the protocol, these check the meaning.
 */
class TestReactive extends munit.FunSuite {

  given Scheduler = Schedulers.loom

  /** collect everything, requesting one at a time */
  private def drain[A](p: Flow.Publisher[A]): List[A] =
    val got = java.util.concurrent.ConcurrentLinkedQueue[A]()
    val done = java.util.concurrent.CountDownLatch(1)
    p.subscribe(new Flow.Subscriber[A] {
      private var s: Flow.Subscription = null
      def onSubscribe(sub: Flow.Subscription): Unit = { s = sub; sub.request(1) }
      def onNext(a: A): Unit = { got.add(a); s.request(1) }
      def onError(e: Throwable): Unit = done.countDown()
      def onComplete(): Unit = done.countDown()
    })
    assert(done.await(10, java.util.concurrent.TimeUnit.SECONDS), "the stream never ended")
    scala.jdk.CollectionConverters.CollectionHasAsScala(got).asScala.toList

  test("a source becomes a publisher, in order") {
    assertEquals(drain(Reactive.publisher(Source.range(0L, 50L))), (0L until 50L).toList)
  }

  test("an empty source completes without an element") {
    assertEquals(drain(Reactive.publisher(Source.range(0L, 0L))), Nil)
  }

  test("demand is respected: nothing arrives before it is asked for") {
    val seen = java.util.concurrent.atomic.AtomicInteger(0)
    val subscribed = java.util.concurrent.CountDownLatch(1)
    var sub: Flow.Subscription = null
    Reactive.publisher(Source.range(0L, 1000L)).subscribe(new Flow.Subscriber[Long] {
      def onSubscribe(s: Flow.Subscription): Unit = { sub = s; subscribed.countDown() }
      def onNext(a: Long): Unit = seen.incrementAndGet(): Unit
      def onError(e: Throwable): Unit = ()
      def onComplete(): Unit = ()
    })
    assert(subscribed.await(5, java.util.concurrent.TimeUnit.SECONDS))
    Thread.sleep(100)
    assertEquals(seen.get, 0, "a publisher may not run ahead of demand")
    sub.request(3)
    Thread.sleep(200)
    assertEquals(seen.get, 3, "exactly what was asked for, no more")
  }

  test("cancelling stops the stream") {
    val seen = java.util.concurrent.atomic.AtomicInteger(0)
    var sub: Flow.Subscription = null
    Reactive.publisher(Source.range(0L, 1000000L)).subscribe(new Flow.Subscriber[Long] {
      def onSubscribe(s: Flow.Subscription): Unit = { sub = s; s.request(5) }
      def onNext(a: Long): Unit = seen.incrementAndGet(): Unit
      def onError(e: Throwable): Unit = ()
      def onComplete(): Unit = ()
    })
    Thread.sleep(200)
    sub.cancel()
    val after = seen.get
    Thread.sleep(200)
    assertEquals(seen.get, after, "nothing arrives after cancel")
  }

  test("an already-failed publisher signals onError without any demand") {
    val err = java.util.concurrent.atomic.AtomicReference[Throwable | Null](null)
    val done = java.util.concurrent.CountDownLatch(1)
    Reactive.failed[Long](RuntimeException("boom")).subscribe(new Flow.Subscriber[Long] {
      def onSubscribe(s: Flow.Subscription): Unit = ()   // asks for nothing
      def onNext(a: Long): Unit = ()
      def onError(e: Throwable): Unit = { err.set(e); done.countDown() }
      def onComplete(): Unit = done.countDown()
    })
    assert(done.await(5, java.util.concurrent.TimeUnit.SECONDS), "terminal signals ignore demand")
    assertEquals(Option(err.get).map(_.getMessage), Some("boom"))
  }

  test("each subscriber gets its own run of the source") {
    // a cold publisher: the source is a program, and running it twice
    // does the work twice -- the library's own re-observation contract
    val runs = java.util.concurrent.atomic.AtomicInteger(0)
    val p = Reactive.publisher(Source.unfold(0L) { i =>
      if i == 0 then runs.incrementAndGet(): Unit
      if i < 5 then Some((i, i + 1)) else None
    })
    assertEquals(drain(p).length, 5)
    assertEquals(drain(p).length, 5)
    assertEquals(runs.get, 2, "two subscribers, two runs")
  }

  // ── the other direction ─────────────────────────────────────────

  test("a publisher becomes a source, in order") {
    val p = Reactive.publisher(Source.range(0L, 200L))
    assertEquals(Reactive.source(p, capacity = 16).toLazyList.toList, (0L until 200L).toList)
  }

  test("a round trip is the identity on elements") {
    val there = Reactive.publisher(Source.range(0L, 100L))
    val back = Reactive.source(there, capacity = 8)
    assertEquals(back.toLazyList.toList, (0L until 100L).toList)
  }

  test("a publisher's failure arrives as the END of the source") {
    // Channel.fail's promise, carried across the bridge: what was
    // already delivered arrives first, and the failure is the end
    // emits from its OWN thread rather than inside `request`: a
    // publisher that calls onNext synchronously from request, with a
    // subscriber that requests from onNext, recurses as deep as the
    // stream is long. The spec allows the shape and asks the
    // publisher to bound it (3.3); the simplest bound is not to do it
    val p: Flow.Publisher[Long] = (s: Flow.Subscriber[? >: Long]) =>
      s.onSubscribe(new Flow.Subscription {
        private val started = java.util.concurrent.atomic.AtomicBoolean(false)
        def request(n: Long): Unit =
          if started.compareAndSet(false, true) then
            Thread.ofPlatform().start(() =>
              var sent = 0L
              while sent < 3 do { s.onNext(sent); sent += 1 }
              s.onError(RuntimeException("boom"))): Unit
        def cancel(): Unit = ()
      })
    val got = scala.collection.mutable.ArrayBuffer.empty[Long]
    val thrown = intercept[RuntimeException] {
      Reactive.source(p, capacity = 8).toLazyList.foreach(got += _)
    }
    assertEquals(thrown.getMessage, "boom")
    assertEquals(got.toList, List(0L, 1L, 2L), "what was delivered arrives before the failure")
  }

  test("an empty publisher becomes an empty source") {
    assertEquals(Reactive.source(Reactive.publisher(Source.range(0L, 0L))).toLazyList.toList, Nil)
  }
}
