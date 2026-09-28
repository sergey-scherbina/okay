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
    /** counted down by the canceller, whichever thread calls it */
    val gone = CountDownLatch(1)
    def await: Int ! R = okay.effect[R, Int](Async.Await[Int] { k =>
      cb = k
      registered.countDown()
      () => { cancelled.set(true); gone.countDown() }
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

  /** run with runForeach, so what arrived BEFORE a failure is seen */
  private def outAndFailure(s: Source[Int]): (Vector[Int], Option[String]) =
    var out = Vector.empty[Int]
    val err =
      try { s.runForeach(x => okay.async { out :+= x }).runWith; None }
      catch case e: Throwable => Some(e.getMessage)
    (out, err)

  test("DRAIN, THEN FAIL: a failing source drops out, the others run to their end, then the merge fails") {
    val parked, failing = Gate()
    @volatile var result: (Vector[Int], Option[String]) = (Vector.empty, None)
    val t = Thread.ofVirtual().start(() => result = outAndFailure(Source.mergeReady(parked.source, failing.source)))
    parked.registered.await(); failing.registered.await()
    failing.fail(RuntimeException("boom"))
    parked.fire(7)
    t.join()
    assertEquals(result, (Vector(7), Some("boom")))
    assert(!parked.cancelled.get, "a healthy source is not cancelled for another's failure")
  }

  test("a throwing Run drops its source; the others drain first") {
    val throwing: Source[Int] = okay.effect[R, Int](Async.Run(() => throw RuntimeException("run")))
      .flatMap(say)
    assertEquals(outAndFailure(Source.mergeReady(throwing, list(1, 2, 3))), (Vector(1, 2, 3), Some("run")))
  }

  test("a source whose own continuation throws keeps what it told, and the first failure wins") {
    // the element before the throw is delivered: the throw is in the
    // continuation AFTER the tell
    val tellsThenThrows: Source[Int] = say(1).flatMap(_ => throw RuntimeException("first"))
    val laterFailure: Source[Int] = say(2).flatMap(_ => say(3)).flatMap(_ => throw RuntimeException("second"))
    val (out, err) = outAndFailure(Source.mergeReady(tellsThenThrows, laterFailure, list(10, 20)))
    assertEquals(out.sorted, Vector(1, 2, 3, 10, 20))
    assertEquals(err, Some("first"))
  }

  test("cancelling the merge while it is parked cancels every parked source") {
    // a cancel is ASYNCHRONOUS — on Loom it is an interrupt the fiber
    // acts on when it next looks — so the law joins before it reads
    // the flags; the first cut read them at once and missed 297 times
    // in 300 while the cancels were still on their way (it passed the
    // lane's gates by luck and went red in a ci-runner whole build).
    // And it cancels once the merge IS parked; a cancel between two
    // operations, before the park, is the next law's case
    // (specs/ready-merge.md, Decisions).
    val a, b = Gate()
    val parked = CountDownLatch(1)
    val m = ReadyMerge(Seq(a.source, b.source), () => parked.countDown())
    val f = summon[Scheduler].fork(() => m.runCollect)
    parked.await()
    f.cancel()
    assert(f.joinEither().isLeft, "a cancelled merge answers with a failure")
    assert(a.cancelled.get && b.cancelled.get, s"cancelled: a=${a.cancelled.get} b=${b.cancelled.get}")
  }

  test("a cancel between the consumer's operation and the merge's park reaches every parked source, on own and on Loom") {
    // ready-merge-own-cancel-window: the fiber cancels ITSELF inside
    // the consumer's operation for the first element, so the stop lands
    // deterministically between two operations — after `a` and `b` have
    // registered (their turns come right after the tell) and before the
    // merge's park is performed. On `own` (a DriveTask) the drive then
    // stops without running the park and, before the fix, nothing
    // called the merge's canceller: 0 of 200 cancelled. The drive's
    // cancel answers the fiber at once, so a join proves nothing about
    // the sources: the law waits on the cancellers themselves, bounded.
    val schedulers = List("loom" -> summon[Scheduler], "own" -> Schedulers.own.build)
    for (name, sch) <- schedulers; round <- 0 until 200 do
      val a, b = Gate()
      val self = java.util.concurrent.CompletableFuture[Fiber[Unit]]()
      val m = ReadyMerge(Seq(list(1), a.source, b.source))
      val f = sch.fork(() => m.runForeach(_ => okay.effect[Async, Unit](Async.Run(() => self.get().cancel()))))
      self.complete(f): Unit
      assert(f.joinEither().isLeft, s"$name round $round: a cancelled merge answers with a failure")
      val seen = a.gone.await(5, java.util.concurrent.TimeUnit.SECONDS) && b.gone.await(5, java.util.concurrent.TimeUnit.SECONDS)
      assert(seen, s"$name round $round: cancelled a=${a.cancelled.get} b=${b.cancelled.get}")
  }

  test("a cancel while the CONSUMER is working reaches a parked source, on own and on Loom") {
    // ready-merge-cancel-under-consumer-ops: one side stays ready for
    // 200 000 elements, the other is parked on a gate, and the consumer
    // performs an operation per element and cancels ITSELF at the 10th.
    // The drive stops before the consumer's next operation; the merge's
    // code lives inside the consumer's continuation and never parks, so
    // neither its park's canceller nor a Discontinue on the next op can
    // reach the gate. Before the drive's cancel hooks: 50 of 50 missed on
    // `own` (the probe that filed the item). On Loom the interrupt is seen
    // at the merge's first park, after the ready side ends — late, but it
    // arrives.
    val schedulers = List("loom" -> summon[Scheduler], "own" -> Schedulers.own.build)
    for (name, sch) <- schedulers; round <- 0 until 20 do
      val g = Gate()
      val self = java.util.concurrent.CompletableFuture[Fiber[Unit]]()
      val seen = java.util.concurrent.atomic.AtomicInteger(0)
      val m = ReadyMerge(Seq(Source.of(LazyList.range(0, 200000)), g.source))
      val f = sch.fork(() => m.runForeach(_ => okay.effect[Async, Unit](Async.Run(() =>
        if seen.incrementAndGet() == 10 then self.get().cancel()))))
      self.complete(f): Unit
      val _ = f.joinEither()
      assert(g.gone.await(10, java.util.concurrent.TimeUnit.SECONDS),
        s"$name round $round: the parked source was never cancelled (registered=${g.registered.getCount == 0})")
  }

  test("an early stop releases a parked source when the program ends, on Loom too (the blocking handler's frame)") {
    // merge-scopes-everywhere: a Loom fiber runs its program inside
    // `Async.runFiber`, so the scope the merge never exited is
    // released when the fiber's program ends
    for round <- 0 until 20 do
      val g = Gate()
      val m = ReadyMerge(Seq(Source.of(LazyList.range(0, 200000)), g.source))
      val f = summon[Scheduler].fork(() => m.runFoldUntil(using FoldUntil.take[Int](3)))
      assertEquals(f.joinEither().map(_.size), Right(3), s"round $round")
      assert(g.gone.await(10, java.util.concurrent.TimeUnit.SECONDS), s"round $round: the parked source outlived the program")
  }

  test("Source.merge stopped early releases its sides (their channels close, their feeders end), and a full run releases nothing") {
    // merge-scopes-everywhere: before, an early-stopped merge left both
    // feeder fibers parked on a full buffer for good
    for (name, sch) <- List("loom" -> summon[Scheduler], "own" -> Schedulers.own.build) do
      val before = Source.mergeReleases.get
      val m = Source.of(LazyList.from(0)).merge(Source.of(LazyList.from(0)), capacity = 4)
      val f = sch.fork(() => m.runFoldUntil(using FoldUntil.take[Int](5)))
      assertEquals(f.joinEither().map(_.size), Right(5), name)
      assertEquals(Source.mergeReleases.get - before, 1L, s"$name: the early stop released the merge once")
      val full = Source.of(List(1, 2, 3)).merge(Source.of(List(4, 5)))
      val before2 = Source.mergeReleases.get
      assertEquals(sch.fork(() => full.runCollect).joinEither().map(_.sorted), Right(Vector(1, 2, 3, 4, 5)), name)
      assertEquals(Source.mergeReleases.get - before2, 0L, s"$name: a merge that ran to its end released nothing")
  }

  test("an EARLY STOP releases a parked source when the program ends, on own") {
    // the consumer takes three elements and finishes; the gate is still
    // parked. On a drive (own, JS) the program's end runs the merge's
    // cancel hook, which it never exited. On Loom nothing sees the end:
    // specs/ready-merge.md, Decisions ("early stop is not cancellation").
    val sch = Schedulers.own.build
    for round <- 0 until 20 do
      val g = Gate()
      val m = ReadyMerge(Seq(Source.of(LazyList.range(0, 200000)), g.source))
      val f = sch.fork(() => m.runFoldUntil(using FoldUntil.take[Int](3)))
      assertEquals(f.joinEither().map(_.size), Right(3), s"round $round")
      assert(g.gone.await(10, java.util.concurrent.TimeUnit.SECONDS), s"round $round: the parked source outlived the program")
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

  // ── poll, then park (specs/ready-merge.md, the stage) ─────────────
  // A `Channel.drained` side is POLLABLE: its Await carries a poll that
  // answers the ring without registering. These laws hold the channel
  // in the test's hand — sends and the close happen from inside the
  // consumer, at a known element — so nothing depends on timing.

  test("a pollable side that finds nothing while another source is ready is not registered") {
    val ch = Channel[Int](16)
    val registered = AtomicInteger(0)
    val busy = Source.of(LazyList.range(0, 1000))
    var out = Vector.empty[Int]
    ReadyMerge(Seq(ch.drained, busy), onRegister = () => registered.incrementAndGet(): Unit)
      .runForeach { x =>
        okay.async {
          out :+= x
          // the busy side is spent: the ring runs dry, and the side's
          // data is there for the POLL that follows
          if x == 999 then { (1 to 5).foreach(i => assert(ch.offer(-i))); ch.close() }
        }
      }.runWith
    assertEquals(out, Vector.range(0, 1000) ++ Vector(-1, -2, -3, -4, -5))
    assertEquals(registered.get, 0)
  }

  test("a pollable side is registered only once the ring ran dry") {
    val ch = Channel[Int](16)
    val registered = CountDownLatch(1)
    val seen = AtomicInteger(0)
    // read on the merge's own thread AT the registration, not after it
    @volatile var seenAtRegister = -1
    @volatile var out = Vector.empty[Int]
    val t = Thread.ofVirtual().start { () =>
      ReadyMerge(Seq(ch.drained, list(1, 2, 3)),
        onRegister = () => { seenAtRegister = seen.get; registered.countDown() })
        .runForeach(x => okay.async { seen.incrementAndGet(); out :+= x }).runWith
    }
    registered.await()
    // registered AFTER the ready side was consumed, not at the first look
    assertEquals(seenAtRegister, 3)
    assert(ch.offer(7)); ch.close()
    t.join()
    assertEquals(out, Vector(1, 2, 3, 7))
  }

  test("fairness kept: a pollable side's element arriving mid-run is told within two turns of its arrival") {
    val ch = Channel[Int](16)
    var out = Vector.empty[Int]
    ReadyMerge(Seq(ch.drained, Source.of(LazyList.range(0, 100)))).runForeach { x =>
      okay.async {
        out :+= x
        if x == 10 then assert(ch.offer(-42))
        if x == 99 then ch.close()
      }
    }.runWith
    assertEquals(out.filter(_ != -42), Vector.range(0, 100))
    // offered while 10 is being CONSUMED, after that turn's poll; seen by
    // the poll when 11's turn passes, told after 12 — two turns, not one
    assert(out.indexOf(-42) <= out.indexOf(10) + 3, out.take(15).toString)
  }

  test("an idle side whose poll answers the end ends; one whose poll answers a failure drops out — drain, then fail") {
    val busy = Source.of(LazyList.range(0, 1000))
    val ch = Channel[Int](16)
    var out = Vector.empty[Int]
    ReadyMerge(Seq(ch.drained, busy)).runForeach { x =>
      okay.async { out :+= x; if x == 500 then ch.close() }
    }.runWith
    assertEquals(out, Vector.range(0, 1000))

    val boom = RuntimeException("boom")
    val ch2 = Channel[Int](16)
    var out2 = Vector.empty[Int]
    val e = intercept[RuntimeException] {
      ReadyMerge(Seq(ch2.drained, busy)).runForeach { x =>
        okay.async { out2 :+= x; if x == 500 then { ch2.fail(boom); ch2.close() } }
      }.runWith
    }
    assert(e eq boom)
    assertEquals(out2, Vector.range(0, 1000))
  }

  /** a hand-made pollable source: its poll says "nothing yet" `misses`
   * times and then answers; its registration answers at once. What it
   * counts is the LADDER a dry ring climbs before it registers */
  private final class Counted(misses: Int, x: Int):
    val polls = AtomicInteger(0)
    val registered = AtomicInteger(0)
    def source: Source[Int] =
      okay.effect[R, Int](Async.Await[Int](
        k => { registered.incrementAndGet(); k(Right(x)); () => () },
        () => if polls.incrementAndGet() > misses then Right(x) else null)).flatMap(say)

  test("the hybrid wait: data arriving on the yield rung is told without a registration") {
    // 3 polls as the ready side's turns pass, then the dry ring's wait:
    // the default ladder's 100 spins, then its yields — the 120th poll
    // answers on that rung, so nothing was registered
    val c = Counted(120, 7)
    assertEquals(collect(ReadyMerge(Seq(c.source, list(1, 2, 3)))), Vector(1, 2, 3, 7))
    assertEquals(c.registered.get, 0)
    assertEquals(c.polls.get, 121)
  }

  test("the hybrid wait: data that never comes by a poll is registered after the whole ladder") {
    val c = Counted(Int.MaxValue, 7)
    assertEquals(collect(ReadyMerge(Seq(c.source, list(1, 2, 3)))), Vector(1, 2, 3, 7))
    assertEquals(c.registered.get, 1)
    // 3 turn-end polls + the default ladder: 100 spins, 50 yields, 4 sleeps
    assertEquals(c.polls.get, 3 + 100 + 50 + 4)
  }

  test("the wait is a given: Register polls nothing and registers at once; Spin(10) polls ten times") {
    locally {
      given Wait = Wait.Register
      val c = Counted(Int.MaxValue, 7)
      assertEquals(collect(ReadyMerge(Seq(c.source, list(1, 2, 3)))), Vector(1, 2, 3, 7))
      assertEquals((c.registered.get, c.polls.get), (1, 3))
    }
    locally {
      given Wait = Wait.Spin(10)
      val c = Counted(Int.MaxValue, 7)
      assertEquals(collect(ReadyMerge(Seq(c.source, list(1, 2, 3)))), Vector(1, 2, 3, 7))
      assertEquals((c.registered.get, c.polls.get), (1, 3 + 10))
    }
  }

  /** a test's platform: the rungs counted, none of them slept */
  private final class Counting extends Pause:
    var spins, yields, nanos, blocks = 0
    def threads = true
    def spin(): Unit = spins += 1
    def yieldNow(): Unit = yields += 1
    def nano(): Unit = nanos += 1
    def block(): Unit = blocks += 1

  test("the rungs are a given too: a counting platform sees the ladder's and the cycle's exact steps") {
    val ladder = Counting()
    locally {
      given Pause = ladder
      val c = Counted(Int.MaxValue, 7)
      assertEquals(collect(ReadyMerge(Seq(c.source, list(1, 2, 3)))), Vector(1, 2, 3, 7))
      assertEquals((ladder.spins, ladder.yields, ladder.nanos, ladder.blocks), (100, 50, 4, 1))
    }
    val cycle = Counting()
    locally {
      given Pause = cycle
      given Wait = Wait.Cycle(100, 50, 4)
      val c = Counted(Int.MaxValue, 7)
      assertEquals(collect(ReadyMerge(Seq(c.source, list(1, 2, 3)))), Vector(1, 2, 3, 7))
      assertEquals((cycle.spins, cycle.yields, cycle.nanos, cycle.blocks), (400, 200, 4, 1))
      assertEquals(c.polls.get, 3 + 4 * (100 + 50) + 1)
    }
  }

  test("the mechanism is a given: Merge.Shared joins the same multiset through one queue") {
    given Merge = Merge.Shared
    val out = (Source.of(LazyList.range(0, 500)) merge Source.of(LazyList.range(500, 1000))).runCollect.runWith
    assertEquals(out.sorted, Vector.range(0, 1000))
    val chunked = Source.of(LazyList.range(0, 500)).merge(Source.of(LazyList.range(500, 1000)), chunked = true)
      .runCollect.runWith
    assertEquals(chunked.sorted, Vector.range(0, 1000))
  }

  test("a merged source is a value: running it twice merges twice") {
    val m = Source.mergeReady(list(1, 2), list(3))
    assertEquals(collect(m), Vector(1, 3, 2))
    assertEquals(collect(m), Vector(1, 3, 2))
  }
}
