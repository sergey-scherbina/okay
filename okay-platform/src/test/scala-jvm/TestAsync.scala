package okay


import okay.freer.*


import okay.std.*
import okay.freer.given
import okay.std.given
/** Loom-style asynchrony: virtual threads, parked blocking, par/race. */
class TestAsync extends munit.FunSuite {

  test("async ops run in place: run is a relay") {
    val prog: Int ! Async = async(20).flatMap(x => async(x + 22))
    assertEquals(!.run(Async.run[Int, okay.freer.Pure](prog)), 42)
    assertEquals(prog.runWith, 42)
  }

  test("spawn on loom runs on a virtual thread; blocking parks it") {
    assume(Schedulers.hasVirtualThreads, "this JVM has no virtual threads")
    // loom BY NAME since scheduler-default-flip: the default given is
    // adaptive now, whose fibers run on its own workers
    given Scheduler = Schedulers.loom
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

  test("a bracket cancelled by timeout releases its resource, as ZIO's interruption does — on loom, adaptive and own") {
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    // ON EVERY MEMBER A FIBER CAN BE CANCELLED ON (drive-interrupts-
    // blocking-run, 2026-09-28): this law ran on the default given only,
    // which was Loom, whose cancel interrupts the fiber's thread. On a
    // drive (`own`, `adaptive`) a cancel only stopped the drive between
    // operations, so the use blocked in a Run kept its resource for its
    // whole five seconds — red the moment `adaptive` became the default.
    // One worker, so the fiber after the cancel runs on the SAME thread
    // and would meet an interrupt the drive failed to take back.
    val owned = List("adaptive" -> Schedulers.adaptive.workers(1).build, "own" -> Schedulers.own.workers(1).build)
    try
      for (name, sch) <- ("loom" -> Schedulers.loom) :: owned do
        given Scheduler = sch
        val parked = CountDownLatch(1)
        val onAwait = bracket("r")(_ => parked.countDown())(_ => Async.sleep(5000).map(_ => 1))
        assertEquals(Async.timeout(50)(onAwait).runWith, None, name)
        assert(parked.await(2, TimeUnit.SECONDS), s"$name: a use parked on an Await leaked its resource when cancelled")
        val blocked = CountDownLatch(1)
        val onRun = bracket("r")(_ => blocked.countDown())(_ => async { Thread.sleep(5000); 1 })
        assertEquals(Async.timeout(50)(onRun).runWith, None, name)
        assert(blocked.await(2, TimeUnit.SECONDS), s"$name: a use blocked in a Run leaked its resource when cancelled")
        assertEquals(Async.spawn(async { Thread.sleep(20); 7 }).joinEither(), Right(7),
          s"$name: the next fiber on the worker met an interrupt the cancel left behind")
    finally owned.foreach(_._2.close())
  }

  test("a continuation that throws after an Await answered LATER fails its own fiber, on every drive") {
    import java.util.concurrent.{CompletableFuture, TimeUnit, TimeoutException}
    // drive-resume-throw-lost (2026-09-28): a drive resumed by a callback
    // ran `apply(k(x))`, and `k(x)` — the fiber's own code — was evaluated
    // as the ARGUMENT, before `apply`'s try. A throw there went to
    // whoever called the callback (a finishing child fiber, a timer, a
    // producer) and was lost with it: the fiber never answered, nothing
    // parked, no thread ran it. Found by the default flip: okay-pool's
    // elastic run threw `Cluster.Rescale` in such a continuation and its
    // attempt stayed "running" for good (TestPoolElastic, 0 of 3 on own,
    // drive and adaptive; 3 of 3 on Loom, whose continuation runs inside
    // the blocking handler's loop).
    val owned = List("adaptive" -> Schedulers.adaptive.workers(1).build, "own" -> Schedulers.own.workers(1).build)
    try
      for (name, sch) <- ("drive" -> Schedulers.drive()) :: owned do
        val k = CompletableFuture[Either[Throwable, Int] => Unit]()
        val f = sch.fork(() => Async.await[Int] { cb => k.complete(cb): Unit; () => () }
          .map(x => if x > 0 then throw IllegalStateException(s"thrown after $x") else x))
        val answered = CompletableFuture[Either[Throwable, Int]]()
        f.onComplete(r => { answered.complete(r): Unit })
        // the answer arrives LATER, from this thread: the callback path
        val toCaller = scala.util.Try(k.get(5, TimeUnit.SECONDS)(Right(1)))
        assert(toCaller.isSuccess, s"$name: the fiber's own failure was thrown at the thread that answered it: $toCaller")
        val r = try answered.get(5, TimeUnit.SECONDS) catch case _: TimeoutException => fail(s"$name: the fiber never answered")
        assertEquals(r.left.map(_.getMessage), Left("thrown after 1"), name)
    finally owned.foreach(_._2.close())
  }

  test("a fiber woken from inside another fiber's code runs on its own slice, and that fiber's cancel never reaches it") {
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    // drive-interrupts-blocking-run pinned the NESTED case: fiber A's Run
    // woke fiber B, B's continuation ran inline inside A's slice, and A's
    // cancel had to be kept off B's code. Since ready-merge-side-starves
    // (2026-09-30) B is not resumed on A's thread at all — a wake from
    // inside a running fiber goes home — so the law is the stronger one:
    // A's code finishes first, B runs on a slice of its own, and a cancel
    // of A (answered already) reaches nothing of B's
    val sch = Schedulers.adaptive.workers(1).build
    try
      @volatile var wakeB: (Either[Throwable, Int] => Unit) | Null = null
      val bParked, bSleeping = CountDownLatch(1)
      val b = sch.fork(() => Async.await[Int] { k => wakeB = k; bParked.countDown(); () => () }
        .flatMap(x => async { bSleeping.countDown(); Thread.sleep(300); x + 1 }))
      assert(bParked.await(5, TimeUnit.SECONDS))
      val a = sch.fork(() => async { wakeB.nn(Right(41)); "a" })
      assertEquals(a.joinEither(), Right("a"), "the waking fiber finished its own code first")
      assert(bSleeping.await(5, TimeUnit.SECONDS))
      a.cancel()
      assertEquals(b.joinEither(), Right(42), "the woken fiber was interrupted by another fiber's cancel")
      assertEquals(sch.fork(() => async { Thread.sleep(20); 7 }).joinEither(), Right(7),
        "the worker kept an interrupt the cancel should have taken back")
    finally sch.close()
  }

  test("a bracket cancelled between two non-blocking steps on the callback drive releases") {
    import java.util.concurrent.{CountDownLatch, TimeUnit}
    // Schedulers.own runs a fiber as a callback DRIVE, which a cancel
    // stops between two operations; a virtual thread would only notice
    // the interrupt at a blocking point, and this use never blocks
    val sch = Schedulers.own.workers(2).build
    try
      given Scheduler = sch
      val released = CountDownLatch(1)
      val end = System.nanoTime() + 5_000_000_000L
      def spin(n: Long): Long ! Async = async(System.nanoTime()).flatMap(t => if t > end then pure(n) else spin(n + 1))
      val p = bracket("r")(_ => released.countDown())(_ => spin(0))
      assertEquals(Async.timeout(50)(p).runWith, None)
      assert(released.await(2, TimeUnit.SECONDS), "the drive dropped a cancelled scope without releasing it")
    finally sch.close()
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
    val (ws, a) = !.run(Writer.run[String, Int, okay.freer.Pure](
      Async.run[Int, Writer % String](prog)))
    assertEquals(ws, Seq("start", "end"))
    assertEquals(a, 42)
  }
}
