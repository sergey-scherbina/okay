package okay.cats

import okay.Async
import okay.given
import okay.freer.given
import okay.std.given
import CatsInterop.*
import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.implicits.global
import java.util.concurrent.{CancellationException, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.*

/** specs/cats-io-async.md: cats IO across without a parked thread, with
 * cancellation both ways */
class TestCatsIOAsync extends munit.FunSuite:

  test("fromIO parks no thread under the callback runner") {
    val gate = Promise[Int]()
    val io = IO.fromFuture(IO.pure(gate.future))
    val running = Async.runAsyncCancellable(fromIO(io).map(_ * 2))
    assert(!running.future.isCompleted)
    gate.success(21)
    assertEquals(Await.result(running.future, 5.seconds), 42)
  }

  test("cancelling the okay side cancels the IO") {
    val started = CountDownLatch(1)
    val finalized = CountDownLatch(1)
    val resumed = AtomicInteger(0)
    val io = (IO(started.countDown()) *> IO.never[Int]).onCancel(IO(finalized.countDown()))
    val running = Async.runAsyncCancellable(fromIO(io).map { n => resumed.incrementAndGet(); n })
    assert(started.await(5, TimeUnit.SECONDS))
    running.cancel()
    assert(finalized.await(5, TimeUnit.SECONDS), "the IO finalizer ran")
    val _ = intercept[CancellationException](Await.result(running.future, 5.seconds))
    assertEquals(resumed.get, 0)
  }

  test("an IO failure crosses as the same throwable; runWith still works") {
    val boom = RuntimeException("boom")
    assertEquals(intercept[RuntimeException](fromIO(IO.raiseError[Int](boom)).runWith), boom)
    assertEquals(fromIO(IO(21).map(_ * 2)).runWith, 42)
  }

  test("toIOAsync runs a callback program without the blocking runner") {
    var resume: Either[Throwable, Int] => Unit = null
    val registered = CountDownLatch(1)
    val io = toIOAsync(Async.await[Int] { k => resume = k; registered.countDown(); () => () }.map(_ * 2))
    val fiber = io.unsafeToFuture()
    assert(registered.await(5, TimeUnit.SECONDS))
    resume(Right(21))
    assertEquals(Await.result(fiber, 5.seconds), 42)
    val boom = RuntimeException("boom")
    assertEquals(toIOAsync(Async.await[Int] { k => k(Left(boom)); () => () }).attempt.unsafeRunSync(), Left(boom))
  }

  test("cancelling the IO cancels the okay Await") {
    val cancelled = AtomicInteger(0)
    var resume: Either[Throwable, Int] => Unit = null
    val resumed = AtomicInteger(0)
    val registered = CountDownLatch(1)
    val io = toIOAsync(Async.await[Int] { k =>
      resume = k
      registered.countDown()
      () => { val _ = cancelled.incrementAndGet() }
    }.map { n => resumed.incrementAndGet(); n })
    val (_, cancel) = io.unsafeToFutureCancelable()
    assert(registered.await(5, TimeUnit.SECONDS))
    Await.result(cancel(), 5.seconds)
    assertEquals(cancelled.get, 1)
    resume(Right(21))
    assertEquals(resumed.get, 0)
  }
