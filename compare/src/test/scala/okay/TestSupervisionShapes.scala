package okay

import scala.language.implicitConversions

/**
 * THE THREE SUPERVISION SHAPES, PINNED BY TIME (2026-09-18).
 *
 * Measured, after a first reading of this that was WRONG in two ways
 * at once:
 *
 *   Async.par          cancels its one sibling          (14 ms)
 *   Async.supervised   cancels a whole scope            ( 2 ms)
 *   Par.traverse       CANCELS -- it is built on `par`  (18 ms)
 *   Parallel.parTraverse  does NOT cancel               (809 ms)
 *
 * The header of `Par.scala` says "they do not cancel the siblings of
 * a leaf that failed" about `parAll`/`parTraverse` in
 * scala-jvm-native/Parallel.scala -- NOT about `Par.traverse`, which
 * is a different function reached through the applicative. Conflating
 * the two is what made `Async.supervised` look like it was closing a
 * gap that `Par.traverse` had. It is not: the gap it closes is an
 * OPEN scope -- fork ad hoc, anywhere in the body -- which neither
 * `par` (exactly two) nor `Par.traverse` (a fixed traversal) offers.
 *
 * Siblings sleep 800 ms, so a shape that cancels returns at once and
 * a shape that waits takes the sleep. */
class TestSupervisionShapes extends munit.FunSuite {

  val boom = RuntimeException("boom")
  val sleep = 800L

  def ms[A](f: => A): (A, Long) =
    val t0 = System.nanoTime()
    val a = f
    (a, (System.nanoTime() - t0) / 1000000)

  /**
   * A sibling that NEVER finishes on its own and says when it is
   * cancelled (load-flakes, 2026-09-23). The first version slept 800 ms
   * and judged "cancelled, not waited for" by ELAPSED time, which a box
   * at load 48-72 read as 422 ms; this one asserts the cancellation
   * itself — `await`'s canceler is what `par`/`supervised` call — and a
   * shape that WAITED would hang the test instead of passing slowly.
   */
  private def neverUnlessCancelled(cancelled: java.util.concurrent.atomic.AtomicInteger,
                                   registered: java.util.concurrent.CountDownLatch): Int ! Async =
    Async.await[Int] { _ =>
      registered.countDown()
      () => cancelled.incrementAndGet(): Unit
    }

  /**
   * The failing branch throws only once every healthy sibling has
   * REGISTERED its canceler (supervision-shapes-race, 2026-09-23). Without
   * this the test raced: under load the failing branch could throw before
   * a sibling reached its `await`, so there was no canceler yet to call,
   * and the count read 0 though the shape was correct — 3 reds of 3 at
   * load ~12 in one worktree, 3 greens of 3 in another a minute later. The
   * wait is BOUNDED: a scheduler that ran the branches in sequence would
   * otherwise hang here instead of failing the count.
   */
  /**
   * The cancellations, once they have ARRIVED: bounded by `within` ms. A
   * cancel that finds a child between its `await`'s registration and the
   * drive storing that registration's canceler is delivered by the child's
   * own drive a moment later (`Drive.op`: `if stopped then cancelReg()`), so
   * the count can trail the scope's answer. A cancel that never comes —
   * a child the scope does not know it has — leaves the count short for good.
   */
  private def arrived(cancelled: java.util.concurrent.atomic.AtomicInteger, expected: Int, within: Long = 5000): Int =
    val until = System.nanoTime() + within * 1000000
    while cancelled.get < expected && System.nanoTime() < until do Thread.sleep(5)
    cancelled.get

  private def failOnce(registered: java.util.concurrent.CountDownLatch): Int ! Async =
    async[Int] { registered.await(10, java.util.concurrent.TimeUnit.SECONDS): Unit; throw boom }

  test("par supervises its ONE sibling: cancelled, not waited for") {
    val cancelled = java.util.concurrent.atomic.AtomicInteger(0)
    val registered = java.util.concurrent.CountDownLatch(1)
    val out = scala.util.Try(!.run(Async.run[(Int, Int), Pure](
      Async.par(neverUnlessCancelled(cancelled, registered), failOnce(registered)))))
    assert(out.isFailure, s"the pair did not fail: $out")
    assertEquals(arrived(cancelled, 1), 1, "the healthy sibling was not cancelled")
  }

  test("supervised supervises a SCOPE: nine siblings cancelled") {
    val cancelled = java.util.concurrent.atomic.AtomicInteger(0)
    val registered = java.util.concurrent.CountDownLatch(9)
    val out = scala.util.Try(!.run(Async.run[Int, Pure](
      Async.supervised: n ?=>
        (0 to 9).foreach: i =>
          val _ =
            if i == 3 then n.fork[Int](failOnce(registered))
            else n.fork(neverUnlessCancelled(cancelled, registered))
        okay.pure[Async, Int](0))))
    assert(out.isFailure, s"the scope did not fail: $out")
    assertEquals(arrived(cancelled, 9), 9, "the scope did not cancel all nine siblings")
  }

  test("a child forked AFTER the scope failed is cancelled too, not left running for good") {
    // the scope's first failure cancels the children it KNOWS; one forked
    // after that (the body is still running) must be cancelled as it
    // joins, or nothing will ever cancel it (supervision-shapes-race)
    val cancelled = java.util.concurrent.atomic.AtomicInteger(0)
    val registered = java.util.concurrent.CountDownLatch(1)
    val out = scala.util.Try(!.run(Async.run[Int, Pure](
      Async.supervised: n ?=>
        val _ = n.fork[Int](async(throw boom))
        Thread.sleep(200)
        val _ = n.fork(neverUnlessCancelled(cancelled, registered))
        okay.pure[Async, Int](0))))
    assert(out.isFailure, s"the scope did not fail: $out")
    assertEquals(arrived(cancelled, 1), 1, "a child forked after the failure was never cancelled")
  }

  test("Par.traverse DOES cancel — it is built on par") {
    val cancelled = java.util.concurrent.atomic.AtomicInteger(0)
    // one registered sibling is enough: the claim is ">= 1"
    val registered = java.util.concurrent.CountDownLatch(1)
    val out = scala.util.Try(!.run(Async.run[Seq[Int], Pure](
      Par.traverse(0 to 9): i =>
        if i == 3 then failOnce(registered)
        else neverUnlessCancelled(cancelled, registered))))
    assert(out.isFailure, s"the traverse did not fail: $out")
    assert(cancelled.get >= 1, "Par.traverse stopped cancelling its siblings")
  }

  test("THE DOCUMENTED LIMIT is Parallel.parTraverse, which cancels nobody") {
    // Par.scala's header states this. A limit with a test is a limit
    // that cannot quietly stop being true: if this starts failing
    // because the siblings ARE cancelled, the header needs rewriting.
    val (out, took) = ms:
      scala.util.Try(!.run(Async.run[Seq[Int], Pure](
        parTraverse(0 to 9): i =>
          if i == 3 then async[Int](throw boom)
          else async { Thread.sleep(sleep); i })))
    assert(out.isFailure, s"the traverse did not fail: $out")
    assert(took >= sleep / 2,
      s"took ${took}ms — the siblings WERE cancelled, so Par.scala's header " +
        "no longer describes parTraverse and must be rewritten")
  }
}
