package okay

import !.*

/** Loom-style asynchrony: virtual threads, parked blocking, par/race. */
class TestAsync extends munit.FunSuite {

  test("async ops run in place: run is a relay") {
    val prog: Int ! Async = async(20).flatMap(x => async(x + 22))
    assertEquals(!.run(Async.run[Int, Nothing](prog)), 42)
    assertEquals(prog.runWith, 42)
  }

  test("spawn runs on a virtual thread; blocking parks it") {
    assume(Schedulers.hasVirtualThreads, "this JVM has no virtual threads; auto is Schedulers.platform there")
    val f = Async.spawn:
      async(Thread.currentThread().isVirtual).flatMap: v =>
        async { Thread.sleep(10); v }
    assertEquals(f.join(), true)
  }

  test("par runs both sides at once, on their own virtual threads") {
    // no clock at all: each side signals its own latch and waits for
    // the other's, which only completes if the two really are running
    // together. A ratio against a sequential run kept flaking on a
    // busy machine (0.78 against a 0.75 bound); a handshake cannot.
    import java.util.concurrent.{CompletableFuture, TimeUnit}
    val a = CompletableFuture[Unit]()
    val b = CompletableFuture[Unit]()
    val prog = Async.par(
      async { a.complete(()); b.get(10, TimeUnit.SECONDS); 1 },
      async { b.complete(()); a.get(10, TimeUnit.SECONDS); 2 })
    assertEquals(prog.runWith, (1, 2))
  }

  test("par sees EITHER side fail, and does not wait out the healthy one") {
    // BUGS.md par-right-failure-waits: the two completions used to be
    // registered in a NEST, so nobody was listening to the right side
    // while the left ran. The same failure in the two orders was
    // 0.0007 s and 3.017 s.
    val boom = RuntimeException("boom")
    for (label, prog) <- Seq(
      "left fails" -> Async.par(async[Int](throw boom), async { Thread.sleep(3000); 1 }),
      "right fails" -> Async.par(async { Thread.sleep(3000); 1 }, async[Int](throw boom)))
    do
      val t0 = System.nanoTime()
      assertEquals(intercept[RuntimeException](prog.runWith).getMessage, "boom", label)
      val secs = (System.nanoTime() - t0) / 1e9
      assert(secs < 2, s"$label: the pair waited $secs s for the healthy sibling")
  }

  test("race answers with the faster side") {
    val prog = Async.race(
      async { Thread.sleep(200); "slow" },
      async { Thread.sleep(10); "fast" })
    assertEquals(prog.runWith, "fast")
  }

  test("an async stream: elements awaited, consumed lazily on demand") {
    type S = Produce + Async
    def ticks(n: Int): Int ! S =
      if n == 0 then pure(0)
      else effect[S, Unit](Async.Run(() => Thread.sleep(1))).flatMap: _ =>
        effect[S, Int](n).flatMap(_ => ticks(n - 1))
    assertEquals(ticks(5).toLazyList.toList, List(5, 4, 3, 2, 1))
    assertEquals(ticks(1000).take(3).toList, List(1000, 999, 998))
  }

  test("schedulers: fork-join and plain threads run fibers too") {
    locally:
      given Scheduler = Schedulers.forkJoin()
      assertEquals(Async.par(async(1), async(2)).runWith, (1, 2))
    locally:
      given Scheduler = Schedulers.threads
      assertEquals(Async.spawn(async(3)).join(), 3)
  }

  test("race cancels the loser: a five-second sleeper does not hold us") {
    val t0 = System.nanoTime()
    assertEquals(Async.race(
      async { Thread.sleep(5000); "slow" }, async("fast")).runWith, "fast")
    assert((System.nanoTime() - t0) / 1e9 < 3, "raced past the sleeper")
  }

  test("timeout: the answer in time, or None with the sleeper cancelled") {
    assertEquals(Async.timeout(2000)(async(2)).runWith, Some(2))
    val t0 = System.nanoTime()
    assertEquals(Async.timeout(50)(Async.sleep(5000).map(_ => 1)).runWith, None)
    assert((System.nanoTime() - t0) / 1e9 < 3, "the sleeper did not hold us")
  }

  test("a bracket cancelled by timeout releases its resource, as ZIO's interruption does") {
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    val parked = CountDownLatch(1)
    val onAwait = bracket("r")(_ => parked.countDown())(_ => Async.sleep(5000).map(_ => 1))
    assertEquals(Async.timeout(50)(onAwait).runWith, None)
    assert(parked.await(2, TimeUnit.SECONDS), "a use parked on an Await leaked its resource when cancelled")
    val blocked = CountDownLatch(1)
    val onRun = bracket("r")(_ => blocked.countDown())(_ => async { Thread.sleep(5000); 1 })
    assertEquals(Async.timeout(50)(onRun).runWith, None)
    assert(blocked.await(2, TimeUnit.SECONDS), "a use blocked in a Run leaked its resource when cancelled")
  }

  test("a bracket that loses a race releases its resource") {
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    val lost = CountDownLatch(1)
    val slow = bracket("r")(_ => lost.countDown())(_ => Async.sleep(5000).map(_ => "slow"))
    assertEquals(Async.race(slow, async("fast")).runWith, "fast")
    assert(lost.await(2, TimeUnit.SECONDS), "the race's loser leaked its resource")
  }

  test("joinEither: a fiber's failure comes back as a value") {
    assertEquals(Async.spawn(async(7)).joinEither(), Right(7))
    val boom = RuntimeException("boom")
    assertEquals(Async.spawn(async[Int](throw boom)).joinEither(), Left(boom))
  }

  test("mutual tail recursion trampolines through Async's own driver loop") {
    def isEven(n: Int): Boolean ! Async =
      if n == 0 then pure(true) else !.tailcall(isOdd(n - 1))
    def isOdd(n: Int): Boolean ! Async =
      if n == 0 then pure(false) else !.tailcall(isEven(n - 1))
    assertEquals(Async.spawn(isEven(1000000)).join(), true)
    assertEquals(Async.spawn(isOdd(1000000)).join(), false)
  }

  test("async composes with other effects: telling across suspensions") {
    type F = Async + Writer % String
    val prog: Int ! F =
      effect[F, Unit](Writer("start")).flatMap: _ =>
        effect[F, Int](Async.Run(() => 21)).flatMap: x =>
          effect[F, Unit](Writer("end")).map(_ => x * 2)
    val (ws, a) = !.run(Writer.run[String, Int, Nothing](
      Async.run[Int, Writer % String](prog)))
    assertEquals(ws, Seq("start", "end"))
    assertEquals(a, 42)
  }
}
