package okay


import okay.freer.*
import java.util.concurrent.CountDownLatch

/** `Fiber.isDone` (fiber-is-done): false while the fiber runs, true once
 * it has its answer — a value, a failure, or a cancellation — on every
 * JVM scheduler, and never false again */
class TestFiberIsDone extends munit.FunSuite {

  private val schedulers = List(
    "loom" -> Schedulers.loom, "own" -> Schedulers.own.build, "default" -> summon[Scheduler],
    "threads" -> Schedulers.threads, "drive" -> Schedulers.drive())

  private def eventually(p: => Boolean): Boolean =
    val deadline = System.nanoTime() + 10_000_000_000L
    while !p && System.nanoTime() < deadline do Thread.sleep(1)
    p

  for (name, sch) <- schedulers do
    test(s"$name: running, then done with a value, a failure and a cancellation") {
      val gate = CountDownLatch(1)
      val f = sch.fork(() => async { gate.await(); 42 })
      assert(!f.isDone, s"$name: done before its latch opened")
      gate.countDown()
      assertEquals(f.joinEither(), Right(42))
      assert(f.isDone, s"$name: not done after join")
      assert(f.isDone, s"$name: isDone turned back")

      val failed = sch.fork(() => async[Int](throw IllegalStateException("boom")))
      assert(eventually(failed.isDone), s"$name: a failed fiber is not done")
      assert(failed.joinEither().isLeft)

      val parked = CountDownLatch(1)
      val c = sch.fork(() => async { parked.await(); 1 })
      c.cancel()
      assert(eventually(c.isDone), s"$name: a cancelled fiber is not done")
      parked.countDown()
    }
}
