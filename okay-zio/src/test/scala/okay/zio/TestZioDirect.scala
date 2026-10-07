package okay.zio

import okay.{Async, async, asOkay}
import okay.freer.{!}
import okay.Direct.*
import okay.given
import okay.freer.given
import okay.std.given
import okay.zio.given
import ZioInterop.*
import _root_.zio.{Runtime, Task, Unsafe, ZIO}
import java.util.concurrent.{CancellationException, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Await
import scala.concurrent.duration.*

/** specs/zio-direct-cancel.md: ZIO in okay's direct blocks, and a
 * cancellable way back from ZIO into an okay program */
class TestZioDirect extends munit.FunSuite:

  private def run[A](t: Task[A]): A =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(t).getOrThrowFiberFailure())

  // No `!z` here: ZIO has its own `unary_!` (negation on a
  // ZIO[_, _, Boolean]), and a member beats an extension, so the prefix
  // mark can never reach a ZIO value. `.?` and `.reflect` are the marks.
  test("direct[Task] binds ZIO values with .? and .reflect") {
    val ran = AtomicInteger(0)
    val t: Task[Int] = direct[Task] {
      val _ = ZIO.succeed(ran.incrementAndGet()).?
      val a = ZIO.attempt(20).?
      val b = ZIO.attempt(22).reflect
      a + b
    }
    assertEquals(ran.get, 0, "building the block runs nothing")
    assertEquals(run(t), 42)
    assertEquals(ran.get, 1)
  }

  test("a ZIO failure fails the block and skips the rest") {
    val boom = RuntimeException("boom")
    val after = AtomicInteger(0)
    val t: Task[Int] = direct[Task] {
      val a = ZIO.attempt(1).?
      val _ = ZIO.fail(boom).?
      after.incrementAndGet()
      a
    }
    assertEquals(run(t.either), Left(boom))
    assertEquals(after.get, 0)
  }

  test("binds nest to 100 000 on ZIO's own trampoline") {
    val t = (1 to 100000).foldLeft(ZIO.succeed(0): Task[Int]) { (acc, i) =>
      direct[Task] { acc.? + i }
    }
    assertEquals(run(t), (1 to 100000).sum)
  }

  test("fromZIO parks no thread under the callback runner") {
    val gate = scala.concurrent.Promise[Unit]()
    val z = ZIO.fromFuture(_ => gate.future).as(21)
    val running = Async.runAsyncCancellable(fromZIO(z).map(_ * 2))
    // runAsyncCancellable returned while the ZIO is still pending: the
    // Await registered and gave the thread back
    assert(!running.future.isCompleted)
    gate.success(())
    assertEquals(Await.result(running.future, 5.seconds), 42)
  }

  test("cancelling the okay side interrupts the ZIO fiber") {
    val started = CountDownLatch(1)
    val finalized = CountDownLatch(1)
    val resumed = AtomicInteger(0)
    val z = (ZIO.succeed(started.countDown()) *> ZIO.never)
      .onInterrupt(ZIO.succeed(finalized.countDown()))
      .as(1)
    val running = Async.runAsyncCancellable(fromZIO(z).map { n => resumed.incrementAndGet(); n })
    assert(started.await(5, TimeUnit.SECONDS))
    running.cancel()
    assert(finalized.await(5, TimeUnit.SECONDS), "the ZIO finalizer ran")
    val _ = intercept[CancellationException](Await.result(running.future, 5.seconds))
    assertEquals(resumed.get, 0)
  }

  test("a ZIO failure crosses as the same throwable; runWith still works") {
    val boom = RuntimeException("boom")
    assertEquals(intercept[RuntimeException](fromZIO(ZIO.fail(boom)).runWith), boom)
    assertEquals(fromZIO(ZIO.attempt(21).map(_ * 2)).runWith, 42)
  }

  test("p.asZIO and z.asOkay") {
    val p: Int ! Async = async(40).map(_ + 2)
    assertEquals(run(p.asZIO), 42)
    assertEquals(ZIO.attempt(42).asOkay.runWith, 42)
  }
