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
    val f = Async.spawn:
      async(Thread.currentThread().isVirtual).flatMap: v =>
        async { Thread.sleep(10); v }
    assertEquals(f.join(), true)
  }

  test("par runs both sides at once, on their own virtual threads") {
    // compare, do not guess: an absolute bound is a flake waiting for
    // a busy machine (it found one), so the sequential run is measured
    // right here and parallelism must beat it by a clear margin
    def nap(n: Int): Int ! Async = async { Thread.sleep(200); n }

    val s0 = System.nanoTime()
    assertEquals(nap(1).flatMap(_ => nap(2)).runWith, 2)
    val seqMs = (System.nanoTime() - s0) / 1000000

    val p0 = System.nanoTime()
    assertEquals(Async.par(nap(1), nap(2)).runWith, (1, 2))
    val parMs = (System.nanoTime() - p0) / 1000000

    assert(parMs < seqMs * 0.75,
      s"not parallel: ${parMs}ms against a sequential ${seqMs}ms")
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

  test("joinEither: a fiber's failure comes back as a value") {
    assertEquals(Async.spawn(async(7)).joinEither(), Right(7))
    val boom = RuntimeException("boom")
    assertEquals(Async.spawn(async[Int](throw boom)).joinEither(), Left(boom))
  }

  test("async composes with other effects: telling across suspensions") {
    type F = Async + Writer % String
    val prog: Int ! F =
      effect[F, String](Writer("start")).flatMap: _ =>
        effect[F, Int](Async.Run(() => 21)).flatMap: x =>
          effect[F, String](Writer("end")).map(_ => x * 2)
    val (ws, a) = !.run(Writer.run[String, Int, Nothing](
      Async.run[Int, Writer % String](prog)))
    assertEquals(ws, Seq("start", "end"))
    assertEquals(a, 42)
  }
}
