package okay.actor

import okay.*
import okay.given

/**
 * AN ACTOR THAT RUNS ON JS (actor-on-js, 2026-09-09).
 *
 * Until this file the module cross-built for three platforms and
 * could be USED on two. The loop read with `receiveBlocking` and ran
 * behaviours with `runWith`, both of which need `CanBlock`, and JS
 * has none — deliberately, since "there is no CanBlock on JS, so a
 * blocking join is a compile error, not a frozen loop". Nothing could
 * spawn an actor there, and nobody noticed because nothing tried.
 *
 * These are the module's own laws written the one way every platform
 * can run them: Await-based programs only, driven by `runAsync`, with
 * munit awaiting the Future. If the loop ever goes back to blocking,
 * this file stops COMPILING for JS — which is why it lives here and
 * not in `scala-jvm`.
 */
class TestActorCross extends munit.FunSuite {

  given scala.concurrent.ExecutionContext = munitExecutionContext

  /** send every element, one accepted at a time */
  private def sendAll(ref: ActorRef[Int], ms: Iterable[Int]): Unit ! Async =
    ms.foldLeft(async(())): (acc, m) =>
      acc.flatMap(_ => ref.tell(m).map(_ => ()))

  test("every accepted message is handled, in order") {
    val seen = scala.collection.mutable.ArrayBuffer.empty[Int]
    val prog =
      for
        ref <- Actor.spawn(0)((n: Int, m: Int) => async { seen += m; n + m })
        _ <- sendAll(ref, 1 to 20)
        _ <- ref.stop()
      yield
        assertEquals(seen.toList, (1 to 20).toList)
        assertEquals(ref.stopped, true)
    Async.runAsync(prog)
  }

  test("state is threaded through the loop") {
    val last = java.util.concurrent.atomic.AtomicInteger(0)
    val prog =
      for
        ref <- Actor.spawn(0)((n: Int, m: Int) => async { val s = n + m; last.set(s); s })
        _ <- sendAll(ref, 1 to 10)
        _ <- ref.stop()
      yield assertEquals(last.get, 55)
    Async.runAsync(prog)
  }

  test("stop drains what was accepted — the law, on whatever platform runs it") {
    val handled = java.util.concurrent.atomic.AtomicInteger(0)
    val prog =
      for
        ref <- Actor.spawn(0, Channel[Int](1024), Supervise.Stop)(
                 (n: Int, _: Int) => async { handled.incrementAndGet(); n + 1 })
        _ <- sendAll(ref, 1 to 300)
        _ <- ref.stop()
      yield assertEquals(handled.get, 300,
        "every accepted message must be handled before stop answers")
    Async.runAsync(prog)
  }

  test("a child is stopped by its parent, and drained too") {
    val handled = java.util.concurrent.atomic.AtomicInteger(0)
    val prog =
      for
        parent <- Actor.spawn(0)((n: Int, m: Int) => async(n + m))
        child <- parent.spawnChild(0, Channel[Int](1024))(
                   (n: Int, _: Int) => async { handled.incrementAndGet(); n + 1 })
        _ <- sendAll(child, 1 to 100)
        _ <- parent.stop()
      yield
        assertEquals(handled.get, 100)
        assertEquals(child.stopped, true)
        assertEquals(parent.stopped, true)
    Async.runAsync(prog)
  }

  test("Supervise.Resume skips exactly the poisonous message") {
    val seen = scala.collection.mutable.ArrayBuffer.empty[Int]
    val prog =
      for
        ref <- Actor.spawn(0, Channel[Int](64), Supervise.Resume)(
                 (n: Int, m: Int) => async {
                   if m == 3 then throw RuntimeException("poison")
                   seen += m
                   n + m
                 })
        _ <- sendAll(ref, 1 to 5)
        _ <- ref.stop()
      yield assertEquals(seen.toList, List(1, 2, 4, 5),
        "the failed message is gone and never retried; the rest are handled")
    Async.runAsync(prog)
  }
}
