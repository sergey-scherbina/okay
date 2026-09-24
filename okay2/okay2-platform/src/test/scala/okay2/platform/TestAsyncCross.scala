package okay2.platform

import scala.concurrent.Promise
import okay2._
import okay2.async._

/** the callback-driven surface: Await-based programs driven by
 * runAsync, no CanBlock anywhere; munit awaits the returned Future */
class TestAsyncCross extends munit.FunSuite {

  implicit val ec: scala.concurrent.ExecutionContext = munitExecutionContext

  test("a long chain of Run operations drives in constant stack") {
    def go(n: Int): Int ! Async = if (n == 0) pure(0) else Async(1).flatMap(x => go(n - x).map(_ + x))
    Async.runAsync(go(10000)).map(v => assertEquals(v, 10000))
  }

  test("an Await whose callback fires during registration continues the drive") {
    val prog = await[Int](k => k(21)).map(_ * 2)
    Async.runAsync(prog).map(v => assertEquals(v, 42))
  }

  test("sleep then answer completes via runAsync") {
    Async.runAsync(Async.sleep(50).map(_ => 42)).map(v => assertEquals(v, 42))
  }

  test("runAsync does not block: it returns before the program can finish") {
    var resume: Either[Throwable, Int] => Unit = null
    val f = Async.runAsync(Async.await[Int] { k => resume = k; () => () }.map(_ + 1))
    assert(!f.isCompleted, "runAsync returned a finished future for an unfinished program")
    resume(Right(41))
    f.map(v => assertEquals(v, 42))
  }

  test("race answers the one that finishes, without waiting for the other") {
    val never = Async.await[String](_ => () => ())
    val prog = Async.race(never, Async.sleep(1).map(_ => "fast"))
    Async.runAsync(prog).map(v => assertEquals(v, "fast"))
  }

  test("cancel stops a spawned program before its next operation") {
    @volatile var ran = false
    val f = Async.spawn(Async.sleep(50).map(_ => { ran = true; 1 }))
    f.cancel()
    val p = Promise[Unit]()
    val _ = timer.after(150)(() => { val _ = p.success(()) })
    p.future.map(_ => assert(!ran, "the cancelled program should not resume"))
  }

  test("onComplete observes a fiber") {
    val p = Promise[Int]()
    Async.spawn(Async.sleep(10).map(_ => 7)).onComplete {
      case Right(v) => p.success(v)
      case Left(e) => p.failure(e)
    }
    p.future.map(v => assertEquals(v, 7))
  }

  test("par pairs two answers by completion callbacks, no parking; a child failure cancels the sibling") {
    val prog = Async.par(Async.sleep(20).map(_ => 1), Async.sleep(10).map(_ => 2))
    Async.runAsync(prog).map(v => assertEquals(v, (1, 2))).flatMap { _ =>
      val boom = new RuntimeException("boom")
      Async.runAsync(Async.par(Async[Int](throw boom), Async.sleep(500).map(_ => 2))).failed.map(e => assertEquals(e.getMessage, "boom"))
    }
  }

  test("joinAsync joins a fiber as an operation") {
    val f = Async.spawn(Async.sleep(10).map(_ => 21))
    Async.runAsync(f.joinAsync.map(_ * 2)).map(v => assertEquals(v, 42))
  }

  test("a failure under timeout ends the timeout AT ONCE, with its own exception") {
    val boom = new RuntimeException("boom")
    val started = System.currentTimeMillis()
    Async.runAsync(Async.timeout(2000)(Async[Int](throw boom))).failed.map { e =>
      assertEquals(e.getMessage, "boom")
      assert(System.currentTimeMillis() - started < 1000, "the failure must not wait for the timer")
    }
  }

  test("Retry.async: retries per the policy; a policy exhausted fails with the LAST error") {
    var attempts = 0
    val flaky = Async { attempts += 1; if (attempts < 3) throw new RuntimeException(s"try $attempts") else 42 }
    Async.runAsync(Retry.async(Retry.immediate(5))(flaky)).map { v =>
      assertEquals(v, 42)
      assertEquals(attempts, 3)
    }.flatMap { _ =>
      var tries = 0
      val hopeless = Async[Int] { tries += 1; throw new RuntimeException(s"try $tries") }
      Async.runAsync(Retry.async(Retry.immediate(2))(hopeless)).failed.map { e =>
        assertEquals(e.getMessage, "try 3")
        assertEquals(tries, 3)
      }
    }
  }

  test("Retry.async: the delays are honoured on the platform timer, and zero delays sleep nothing") {
    val started = System.currentTimeMillis()
    val hopeless = Async[Int](throw new RuntimeException("no"))
    Async.runAsync(Retry.async(Retry.constant(30).take(2))(hopeless)).failed.flatMap { _ =>
      assert(System.currentTimeMillis() - started >= 60, "two delays of 30 ms must have passed")
      val t0 = System.currentTimeMillis()
      Async.runAsync(Retry.async(Retry.immediate(3))(hopeless)).failed.map { _ =>
        assert(System.currentTimeMillis() - t0 < 500, "immediate retries must not sleep")
      }
    }
  }

  test("a race of two failures fails instead of hanging; an Await's Left is the error channel") {
    val prog = Async.race(Async[Int](throw new RuntimeException("a")), Async[Int](throw new RuntimeException("b")))
    Async.runAsync(prog).failed.map(e => assert(e.getMessage == "a" || e.getMessage == "b")).flatMap { _ =>
      val boom = new RuntimeException("wire down")
      Async.runAsync(Async.await[Int](k => { k(Left(boom)); () => () })).failed.map(e => assertEquals(e.getMessage, "wire down"))
    }
  }

  test("attempt: a failure as data, on its own fiber") {
    Async.runAsync(Async.attempt(Async[Int](throw new RuntimeException("boom")))).map(r => assertEquals(r.left.map(_.getMessage), Left("boom")))
  }
}

/** fibers per program, retries per policy — the blocking half */
class TestParallel extends munit.FunSuite {

  test("parTraverse: order preserved, fibers really used") {
    val r = parTraverse(1 to 6)(i => Async { Thread.sleep(10); i * 10 }).runWith
    assertEquals(r, (1 to 6).map(_ * 10))
  }

  test("retry: succeeds after failures, per policy; exhausted rethrows") {
    var attempts = 0
    def flaky: Int ! Async = Async { attempts += 1; if (attempts < 3) throw new RuntimeException("flap") else 42 }
    assertEquals(retry(Retry.exponential(1).take(5))(flaky).runWith, 42)
    assertEquals(attempts, 3)
    attempts = 0
    def hopeless: Int ! Async = Async { attempts += 1; throw new RuntimeException("no") }
    val _ = intercept[RuntimeException](retry(Retry.immediate(2))(hopeless).runWith)
    assertEquals(attempts, 3)
  }

  test("policies are streams: exponential sequences, jitter stays bounded") {
    assertEquals(Retry.exponential(10).take(4).toList, List(10L, 20L, 40L, 80L))
    assertEquals(Retry.exponential(10, cap = 35).take(4).toList, List(10L, 20L, 35L, 35L))
    Retry.jittered(Retry.constant(100)).take(50).foreach(d => assert(d >= 50 && d < 150, s"jitter $d out of [50, 150)"))
  }

  test("supervised: the fiber restarts its program and completes") {
    var attempts = 0
    val f = supervised(Retry.immediate(3))(Async { attempts += 1; if (attempts < 2) throw new RuntimeException("boom") else 7 })
    assertEquals(f.join(), 7)
    assertEquals(attempts, 2)
  }

  test("Par.sequence: eight leaves that must meet, do meet; results in order; a failing leaf fails the spine at once") {
    val latch = new java.util.concurrent.CountDownLatch(8)
    def leaf(millis: Long): Boolean ! Async = Async { latch.countDown(); latch.await(millis, java.util.concurrent.TimeUnit.MILLISECONDS) }
    assertEquals(Par.sequence(Seq.fill(8)(leaf(10000))).runWith, Seq.fill(8)(true))
    assertEquals(Par.traverse((1 to 50).toList)(i => Async(i * 2)).runWith, (1 to 50).map(_ * 2))
    assertEquals(Par.sequence(Seq.empty[Int ! Async]).runWith, Seq.empty[Int])
    @volatile var finished = false
    def slow = Async { Thread.sleep(3000); finished = true; 1 }
    def bad: Int ! Async = Async(throw new RuntimeException("boom"))
    for ((label, leaves) <- Seq("failure first" -> Seq(bad, slow), "failure second" -> Seq(slow, bad))) {
      finished = false
      val t0 = System.nanoTime()
      assertEquals(intercept[RuntimeException](Par.sequence(leaves).runWith).getMessage, "boom", label)
      assert((System.nanoTime() - t0) / 1e9 < 2, s"$label: the spine waited for the healthy sibling")
      assertEquals(finished, false, label)
    }
  }
}

/** a ThreadLocal with no public `set` */
class TestScoped extends munit.FunSuite {

  test("current answers the default; where binds for its extent, restores on every exit, nests") {
    val s = Scoped("default")
    assertEquals(s.current, "default")
    var seenInside = ""
    s.where("bound") { seenInside = s.current }
    assertEquals(seenInside, "bound")
    assertEquals(s.current, "default")
    val n = Scoped(0)
    val _ = intercept[RuntimeException] { n.where(1) { throw new RuntimeException("boom") } }
    assertEquals(n.current, 0)
    s.where("outer") {
      assertEquals(s.current, "outer")
      s.where("inner") { assertEquals(s.current, "inner") }
      assertEquals(s.current, "outer")
    }
    assertEquals(s.current, "default")
  }
}
