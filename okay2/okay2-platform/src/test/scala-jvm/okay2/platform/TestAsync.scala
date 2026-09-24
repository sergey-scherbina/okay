package okay2.platform

import java.util.concurrent.{CompletableFuture, CountDownLatch, TimeUnit}
import okay2._
import okay2.async._
import okay2.Stream.FeedInOps

/** Loom-style asynchrony: virtual threads, parked blocking, par/race. */
class TestAsync extends munit.FunSuite {

  test("async ops run in place: run is a relay") {
    val prog: Int ! Async = Async(20).flatMap(x => Async(x + 22))
    assertEquals(Effects.run(Async.run(prog)), 42)
    assertEquals(prog.runWith, 42)
  }

  test("spawn runs on a virtual thread; blocking parks it") {
    assume(Schedulers.hasVirtualThreads, "this JVM has no virtual threads")
    val f = Async.spawn(Async(Thread.currentThread().isVirtual).flatMap(v => Async { Thread.sleep(10); v }))
    assertEquals(f.join(), true)
  }

  test("par runs both sides at once, on their own fibers") {
    // no clock: each side signals its own latch and waits for the other's
    val a = new CompletableFuture[Unit]()
    val b = new CompletableFuture[Unit]()
    val prog = Async.par(
      Async { a.complete(()); b.get(10, TimeUnit.SECONDS); 1 },
      Async { b.complete(()); a.get(10, TimeUnit.SECONDS); 2 })
    assertEquals(prog.runWith, (1, 2))
  }

  test("par sees EITHER side fail, and does not wait out the healthy one") {
    val boom = new RuntimeException("boom")
    for ((label, prog) <- Seq(
      "left fails" -> Async.par(Async[Int](throw boom), Async { Thread.sleep(3000); 1 }),
      "right fails" -> Async.par(Async { Thread.sleep(3000); 1 }, Async[Int](throw boom)))) {
      val t0 = System.nanoTime()
      assertEquals(intercept[RuntimeException](prog.runWith).getMessage, "boom", label)
      val secs = (System.nanoTime() - t0) / 1e9
      assert(secs < 2, s"$label: the pair waited $secs s for the healthy sibling")
    }
  }

  test("race answers with the faster side, and cancels the loser") {
    val prog = Async.race(Async { Thread.sleep(200); "slow" }, Async { Thread.sleep(10); "fast" })
    assertEquals(prog.runWith, "fast")
    val t0 = System.nanoTime()
    assertEquals(Async.race(Async { Thread.sleep(5000); "slow" }, Async("fast")).runWith, "fast")
    assert((System.nanoTime() - t0) / 1e9 < 3, "raced past the sleeper")
  }

  test("an async stream: elements awaited, consumed lazily on demand") {
    type S = Writer[Int] + Async
    def ticks(n: Int): Unit ! S =
      if (n == 0) pure(())
      else Async(Thread.sleep(1)).at[S].flatMap(_ => Writer.tell(n).at[S].flatMap(_ => ticks(n - 1)))
    assertEquals(ticks(5).toLazyList.toList, List(5, 4, 3, 2, 1))
    assertEquals(ticks(1000).toLazyList.take(3).toList, List(1000, 999, 998))
  }

  test("schedulers: every member of the family runs par and spawn/join") {
    val owned = List("own" -> Schedulers.own.workers(4).build, "adaptive" -> Schedulers.adaptive.workers(2).build)
    val family: List[(String, Scheduler)] =
      List("loom" -> Schedulers.loom, "forkJoin" -> Schedulers.forkJoin(), "drive" -> Schedulers.drive(), "threads" -> Schedulers.threads) ++ owned
    try {
      for ((name, sch) <- family if name != "loom" || Schedulers.hasVirtualThreads) {
        implicit val S: Scheduler = sch
        assertEquals(Async.par(Async(1), Async(2)).runWith, (1, 2), name)
        assertEquals(Async.spawn(Async(3)).join(), 3, name)
        assertEquals(Async.spawn(Async[Int](throw new RuntimeException("x"))).joinEither().left.map(_.getMessage), Left("x"), name)
        // a fork from inside a fiber (the owner's own end on `own`)
        assertEquals(Async.spawn(Async.spawn(Async(4)).joinAsync).join(), 4, name)
      }
    } finally owned.foreach(_._2.close())
  }

  test("timeout: the answer in time, or None with the sleeper cancelled") {
    assertEquals(Async.timeout(2000)(Async(2)).runWith, Some(2))
    val t0 = System.nanoTime()
    assertEquals(Async.timeout(50)(Async.sleep(5000).map(_ => 1)).runWith, None)
    assert((System.nanoTime() - t0) / 1e9 < 3, "the sleeper did not hold us")
  }

  test("joinEither: a fiber's failure comes back as a value") {
    assertEquals(Async.spawn(Async(7)).joinEither(), Right(7))
    val boom = new RuntimeException("boom")
    assertEquals(Async.spawn(Async[Int](throw boom)).joinEither(), Left(boom))
  }

  test("mutual tail recursion trampolines through Async's own driver loop") {
    def isEven(n: Int): Boolean ! Async = if (n == 0) pure(true) else !.tailcall(isOdd(n - 1))
    def isOdd(n: Int): Boolean ! Async = if (n == 0) pure(false) else !.tailcall(isEven(n - 1))
    assertEquals(Async.spawn(isEven(1000000)).join(), true)
    assertEquals(Async.spawn(isOdd(1000000)).join(), false)
    locally {
      implicit val S: Scheduler = Schedulers.drive()
      assertEquals(Async.spawn(isEven(1000000)).join(), true)
    }
  }

  test("async composes with other effects: telling across suspensions") {
    type F = Async + Writer[String]
    val prog: Int ! F =
      Writer.tell("start").at[F].flatMap(_ => Async(21).at[F].flatMap(x => Writer.tell("end").at[F].map(_ => x * 2)))
    val (ws, a) = Effects.run(Writer.run(Async.run(prog)))
    assertEquals(ws, Seq("start", "end"))
    assertEquals(a, 42)
  }

  test("Interruptible: a cancel interrupts the lifted action, whatever the scheduler") {
    implicit val S: Scheduler = Schedulers.drive()
    val started = new CountDownLatch(1)
    @volatile var interrupted = false
    val f = Async.spawn(Free.inject[Async, Unit](Interruptible.await { () =>
      started.countDown()
      try Thread.sleep(5000) catch { case _: InterruptedException => interrupted = true }
    }))
    started.await(5, TimeUnit.SECONDS)
    f.cancel()
    Thread.sleep(200)
    assert(interrupted, "the lifted action was not interrupted")
  }

  test("supervised: a child's failure cancels the other nine and leaves the scope") {
    implicit val S: Scheduler = Schedulers.forkJoin()
    val finished = new java.util.concurrent.atomic.AtomicInteger(0)
    val boom = new RuntimeException("boom")
    val t0 = System.nanoTime()
    val out = scala.util.Try(Effects.run(Async.run(Async.supervised[Int] { n =>
      (0 to 9).foreach { i =>
        val _ = if (i == 3) n.fork[Int](Async(throw boom)) else n.fork(Async { Thread.sleep(3000); finished.incrementAndGet() })
      }
      pure[Async, Int](0)
    })))
    val took = (System.nanoTime() - t0) / 1000000
    assert(out.isFailure, s"the scope did not fail: $out")
    assertEquals(out.failed.get.getMessage, "boom")
    assert(took < 1500, s"took ${took}ms — the siblings were waited for, not cancelled")
    assert(finished.get() < 9)
  }

  test("supervised: no failure waits for every child; the body's failure cancels the children") {
    implicit val S: Scheduler = Schedulers.forkJoin()
    val done = new java.util.concurrent.atomic.AtomicInteger(0)
    val out = Effects.run(Async.run(Async.supervised[String] { n =>
      (1 to 5).foreach(_ => { val _ = n.fork(Async { Thread.sleep(50); done.incrementAndGet() }) })
      pure[Async, String]("body")
    }))
    assertEquals(out, "body")
    assertEquals(done.get(), 5, "the scope finished while children were still running")

    val finished = new java.util.concurrent.atomic.AtomicInteger(0)
    val t0 = System.nanoTime()
    val failed = scala.util.Try(Effects.run(Async.run(Async.supervised[Int] { n =>
      val _ = n.fork(Async { Thread.sleep(3000); finished.incrementAndGet() })
      Async[Int](throw new RuntimeException("body"))
    })))
    assert(failed.isFailure)
    assert((System.nanoTime() - t0) / 1000000 < 1500, "the child was waited for")
    assertEquals(finished.get(), 0)
    // and a fiber's value through joinAsync
    assertEquals(Effects.run(Async.run(Async.supervised[Int] { n =>
      val a = n.fork(Async(40)); val b = n.fork(Async(2))
      a.joinAsync.flatMap(x => b.joinAsync.map(y => x + y))
    })), 42)
  }
}
