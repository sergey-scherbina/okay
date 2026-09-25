package okay

import java.util.concurrent.atomic.AtomicInteger
import scala.language.implicitConversions

/**
 * THE OPEN SUPERVISED SCOPE (Async.supervised). The assertions are
 * TIME and COUNTS: siblings sleep 3 s, so a scope that waits for them
 * takes 3 s and a scope that cancels them returns at once.
 */
class TestSupervised extends munit.FunSuite {

  val boom = RuntimeException("boom")

  /** a pool WIDER than any test's fiber count, not the common pool.
   * ci-runner-shape-flakes (2026-09-20): on GitHub's 4-vCPU runner
   * the common pool has parallelism 3, so in the ten-child test the
   * three sleepers took every thread and the child that throws sat
   * in the queue behind them for the full 3 s — "took 3004ms, the
   * siblings were waited for". Reproduced on JDK 21 with
   * -Djava.util.concurrent.ForkJoinPool.common.parallelism=3. The
   * scope's cancellation was never the thing that waited. */
  private val pool = java.util.concurrent.ForkJoinPool(16)
  override def afterAll(): Unit = pool.shutdown()

  def ms[A](f: => A): (A, Long) =
    val t0 = System.nanoTime()
    val a = f
    (a, (System.nanoTime() - t0) / 1000000)

  test("a child's failure cancels the other nine and leaves the scope") {
    given Scheduler = Schedulers.forkJoin(pool)
    val finished = AtomicInteger(0)
    val (out, took) = ms:
      scala.util.Try(!.run(Async.run[Int, Pure](
        Async.supervised: n ?=>
          (0 to 9).foreach: i =>
            val _ =
              if i == 3 then n.fork[Int](async(throw boom))
              else n.fork(async { Thread.sleep(3000); finished.incrementAndGet() })
          okay.pure[Async, Int](0))))
    assert(out.isFailure, s"the scope did not fail: $out")
    assertEquals(out.failed.get.getMessage, "boom")
    assert(took < 1500, s"took ${took}ms — the siblings were waited for, not cancelled")
    assert(finished.get() < 9, s"${finished.get()} siblings ran to completion despite the failure")
  }

  test("no failure: the scope waits for every child and answers the body") {
    given Scheduler = Schedulers.forkJoin(pool)
    val done = AtomicInteger(0)
    val out = !.run(Async.run[String, Pure](
      Async.supervised: n ?=>
        (1 to 5).foreach(_ => { val _ = n.fork(async { Thread.sleep(50); done.incrementAndGet() }) })
        okay.pure[Async, String]("body")))
    assertEquals(out, "body")
    assertEquals(done.get(), 5, "the scope finished while children were still running")
  }

  test("the BODY's failure cancels the children too") {
    given Scheduler = Schedulers.forkJoin(pool)
    val finished = AtomicInteger(0)
    val (out, took) = ms:
      scala.util.Try(!.run(Async.run[Int, Pure](
        Async.supervised: n ?=>
          val _ = n.fork(async { Thread.sleep(3000); finished.incrementAndGet() })
          async[Int](throw boom))))
    assert(out.isFailure, s"the scope did not fail: $out")
    assert(took < 1500, s"took ${took}ms — the child was waited for")
    assertEquals(finished.get(), 0, "the child ran to completion after the body failed")
  }

  test("a fiber's value is available through joinAsync, as usual") {
    given Scheduler = Schedulers.forkJoin(pool)
    val out = !.run(Async.run[Int, Pure](
      Async.supervised: n ?=>
        val a = n.fork(async(40))
        val b = n.fork(async(2))
        a.joinAsync.flatMap(x => b.joinAsync.map(y => x + y))))
    assertEquals(out, 42)
  }
}
