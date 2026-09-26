package okay

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

/**
 * `Source.mergeReady` (specs/ready-merge.md): a merge by readiness on
 * one thread of control — the ring holds the sources, an Await is
 * "not ready", and a callback wakes the merge.
 *
 * The parked sources here are `Gate`s: an Await whose callback the TEST
 * holds and fires, so "which source is ready when" is decided by the
 * test and not by a clock — none of these depends on timing.
 */
class TestReadyMerge extends munit.FunSuite {

  private type R = Writer % Int + Async

  private def say(x: Int): Unit ! R = okay.effect[R, Unit](Writer(x))

  private def list(xs: Int*): Source[Int] = Source.of(xs.toList)

  /** an Await the test answers by hand */
  private final class Gate:
    @volatile private var cb: (Either[Throwable, Int] => Unit) | Null = null
    val registered = CountDownLatch(1)
    val cancelled = AtomicBoolean(false)
    def await: Int ! R = okay.effect[R, Int](Async.Await[Int] { k =>
      cb = k
      registered.countDown()
      () => cancelled.set(true)
    })
    def fire(x: Int): Unit = cb.nn(Right(x))
    def fail(e: Throwable): Unit = cb.nn(Left(e))
    /** a source that waits on this gate, then tells what it was given */
    def source: Source[Int] = await.flatMap(say)

  private def collect(s: Source[Int]): Vector[Int] = s.runCollect.runWith

  /** run on a virtual thread, so the test thread can answer the gates */
  private def inBackground(s: Source[Int]): (Thread, () => Vector[Int]) =
    @volatile var out = Vector.empty[Int]
    @volatile var err: Throwable | Null = null
    val t = Thread.ofVirtual().start { () =>
      try out = collect(s) catch case e: Throwable => err = e
    }
    (t, () => { t.join(); if err != null then throw err.nn; out })

  test("no source ever waits: a strict round-robin, and an ended source drops out") {
    val m = Source.mergeReady(list(1, 2, 3, 4), list(10, 20), list(100, 200, 300))
    assertEquals(collect(m), Vector(1, 10, 100, 2, 20, 200, 3, 300, 4))
  }

  test("each source keeps its own order and nothing is lost or invented") {
    val n = 1000
    val a = Source.of(LazyList.range(0, n).map(_ * 2))
    val b = Source.of(LazyList.range(0, n).map(_ * 2 + 1))
    val out = collect(a.mergeReady(b))
    assertEquals(out.filter(_ % 2 == 0), Vector.tabulate(n)(_ * 2))
    assertEquals(out.filter(_ % 2 == 1), Vector.tabulate(n)(_ * 2 + 1))
  }

  test("a parked source does not hold the others") {
    val g = Gate()
    val busy = Source.of(LazyList.range(0, 1000))
    val m = Source.mergeReady(g.source, busy)
    // the gate is answered only once the busy side's LAST element has
    // been consumed: if the parked side held the merge, this never runs
    var out = Vector.empty[Int]
    m.runForeach { x =>
      okay.async {
        out :+= x
        if x == 999 then g.fire(-1)
      }
    }.runWith
    assertEquals(out, Vector.range(0, 1000) :+ -1)
  }

  test("all sources parked: the merge parks, and wakes in the order the sources did") {
    val a, b = Gate()
    val (_, result) = inBackground(Source.mergeReady(a.source, b.source))
    a.registered.await(); b.registered.await()
    b.fire(2)
    a.fire(1)
    assertEquals(result(), Vector(2, 1))
  }

  test("an Await answered during its own registration is kept, and 10^5 of them do not grow the stack") {
    val n = 100000
    def syncs(i: Int): Source[Int] =
      if i == n then okay.pure(())
      else okay.effect[R, Int](Async.Await[Int] { k => k(Right(i)); () => () })
        .flatMap(x => say(x)).flatMap(_ => syncs(i + 1))
    val out = collect(Source.mergeReady(syncs(0), list(-1, -2)))
    assertEquals(out.filter(_ >= 0), Vector.range(0, n))
    assertEquals(out.filter(_ < 0), Vector(-1, -2))
  }

  test("a Run is performed in its source's own turn") {
    val calls = AtomicInteger(0)
    def ran(x: Int): Source[Int] = okay.effect[R, Int](Async.Run(() => { calls.incrementAndGet(); x })).flatMap(say)
    val a = ran(1).flatMap(_ => ran(2))
    val m = Source.mergeReady(a, list(10, 20))
    assertEquals(collect(m), Vector(1, 10, 2, 20))
    assertEquals(calls.get, 2)
  }

  test("a failing source fails the merge and cancels the parked ones") {
    val parked, failing = Gate()
    val (_, result) = inBackground(Source.mergeReady(parked.source, failing.source))
    parked.registered.await(); failing.registered.await()
    failing.fail(RuntimeException("boom"))
    val e = intercept[RuntimeException](result())
    assertEquals(e.getMessage, "boom")
    assert(parked.cancelled.get, "the other parked source's registration must be cancelled")
  }

  test("a throwing Run fails the merge and cancels the parked ones") {
    val parked = Gate()
    val throwing: Source[Int] = okay.effect[R, Int](Async.Run(() => throw RuntimeException("run")))
      .flatMap(say)
    val e = intercept[RuntimeException](collect(Source.mergeReady(parked.source, throwing)))
    assertEquals(e.getMessage, "run")
    assert(parked.cancelled.get)
  }

  test("cancelling the merge while it is parked cancels every parked source") {
    val a, b = Gate()
    val f = summon[Scheduler].fork(() => Source.mergeReady(a.source, b.source).runCollect)
    a.registered.await(); b.registered.await()
    f.cancel()
    assert(a.cancelled.get && b.cancelled.get, s"cancelled: a=${a.cancelled.get} b=${b.cancelled.get}")
  }

  test("stack-safe over 10^6 elements") {
    val n = 500000
    val m = Source.of(LazyList.range(0, n)) mergeReady Source.of(LazyList.range(0, n))
    assertEquals(collect(m).length, 2 * n)
  }

  test("parallelism is the source's choice: a buffered side merges with an unbuffered one") {
    val n = 2000
    val fast = Channel.buffer(16)(LazyList.range(0, n).map(_ * 2)).drained
    val here = Source.of(LazyList.range(0, n).map(_ * 2 + 1))
    val out = collect(fast.mergeReady(here))
    assertEquals(out.filter(_ % 2 == 0), Vector.tabulate(n)(_ * 2))
    assertEquals(out.filter(_ % 2 == 1), Vector.tabulate(n)(_ * 2 + 1))
  }

  test("the merged source is consumed by an iteratee") {
    def sum(acc: Int): Int ! Take % Int = Take.await[Int].flatMap:
      case Some(x) => sum(acc + x)
      case None => okay.pure(acc)
    val m = Source.mergeReady(list(1, 2, 3), list(10, 20))
    assertEquals(pipe(m)(sum(0)).runWith, 36)
  }

  test("a merged source is a value: running it twice merges twice") {
    val m = Source.mergeReady(list(1, 2), list(3))
    assertEquals(collect(m), Vector(1, 3, 2))
    assertEquals(collect(m), Vector(1, 3, 2))
  }
}
