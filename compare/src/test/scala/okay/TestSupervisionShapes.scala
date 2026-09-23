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

  // Live (flaky-to-integration, 2026-09-23): the verdict is an ELAPSED
  // time — 422 ms at load 48-72, green alone; asserting the cancellation
  // itself instead is backlog supervision-shapes-timing-flake
  test("par supervises its ONE sibling: cancelled, not waited for".tag(new munit.Tag("Live"))) {
    val (out, took) = ms:
      scala.util.Try(!.run(Async.run[(Int, Int), Pure](
        Async.par(async { Thread.sleep(sleep); 1 }, async[Int](throw boom)))))
    assert(out.isFailure, s"the pair did not fail: $out")
    assert(took < sleep / 2, s"took ${took}ms — the healthy sibling was waited for")
  }

  test("supervised supervises a SCOPE: nine siblings cancelled") {
    val (out, took) = ms:
      scala.util.Try(!.run(Async.run[Int, Pure](
        Async.supervised: n ?=>
          (0 to 9).foreach: i =>
            val _ =
              if i == 3 then n.fork[Int](async(throw boom))
              else n.fork(async { Thread.sleep(sleep); i })
          okay.pure[Async, Int](0))))
    assert(out.isFailure, s"the scope did not fail: $out")
    assert(took < sleep / 2, s"took ${took}ms — the scope waited for the siblings")
  }

  test("Par.traverse DOES cancel — it is built on par") {
    val (out, took) = ms:
      scala.util.Try(!.run(Async.run[Seq[Int], Pure](
        Par.traverse(0 to 9): i =>
          if i == 3 then async[Int](throw boom)
          else async { Thread.sleep(sleep); i })))
    assert(out.isFailure, s"the traverse did not fail: $out")
    assert(took < sleep / 2,
      s"took ${took}ms — Par.traverse stopped cancelling its siblings")
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
