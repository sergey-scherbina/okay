package okay

import okay.Direct.*
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CompletableFuture, CompletionException}

/** specs/direct-foreign-mark.md: a foreign effect marked inside a direct
 * block over an okay program, through a given ForeignEffect */
class TestDirectForeign extends munit.FunSuite:

  // docs/effect-interop.md, "Your own effect": a Java CompletableFuture
  // joins with one given — the effect it becomes, and how to lift it
  given ForeignEffect[CompletableFuture] with
    type G[+X] = Async[X]
    def lift[A](m: CompletableFuture[A]): A ! Async =
      Async.await[A] { k =>
        val _ = m.whenComplete { (a, e) =>
          k(if e == null then Right(a) else Left(unwrap(e)))
        }
        () => { val _ = m.cancel(true) }
      }

  private def unwrap(e: Throwable): Throwable = e match
    case c: CompletionException if c.getCause != null => c.getCause
    case other => other

  test("a Future is marked with .?, .reflect and ! inside a block over Async") {
    val built = AtomicInteger(0)
    val p: Int ! Async = direct {
      val a = Future(20).?
      val b = Future(2).reflect
      val c = !Future(20)
      built.incrementAndGet()
      a + b + c
    }
    assertEquals(built.get, 0, "the block runs when the program does")
    assertEquals(p.runWith, 42)
  }

  test("a failed Future fails the program with the same throwable") {
    val boom = RuntimeException("boom")
    val p: Int ! Async = direct {
      val a = Future.failed[Int](boom).?
      a + 1
    }
    assertEquals(intercept[RuntimeException](p.runWith), boom)
  }

  test("a Future waits by callback: the callback runner parks no thread") {
    val gate = Promise[Int]()
    val p: Int ! Async = direct { gate.future.? * 2 }
    val running = Async.runAsyncCancellable(p)
    assert(!running.future.isCompleted)
    gate.success(21)
    assertEquals(scala.concurrent.Await.result(running.future, scala.concurrent.duration.Duration(5, "s")), 42)
  }

  test("a foreign value whose effect is not in the row is refused") {
    val e = compileErrors("""
      import okay.Direct.*
      import scala.concurrent.Future
      val p: Int ! Writer % String = direct { Future.successful(1).? }
    """)
    assert(e.contains("Writer"), e)
  }

  test("without a ForeignEffect the old refusal stands") {
    val e = compileErrors("""
      import okay.Direct.*
      final class Box[A](val a: A)
      val p: Int ! Async = direct { Box(1).? }
    """)
    assert(e.contains("neither this block"), e)
  }

  test("your own effect: a CompletableFuture joins with one given") {
    val p: Int ! Async = direct {
      val a = CompletableFuture.supplyAsync(() => 20).?
      val b = CompletableFuture.completedFuture(22).reflect
      a + b
    }
    assertEquals(p.runWith, 42)
  }

  test("your own effect: its failure and its cancellation cross") {
    val boom = RuntimeException("boom")
    val failing: Int ! Async = direct { CompletableFuture.supplyAsync[Int](() => throw boom).? }
    assertEquals(intercept[RuntimeException](failing.runWith), boom)
    val never = CompletableFuture[Int]()
    val running = Async.runAsyncCancellable(direct[[X] =>> X ! Async] { never.? + 1 })
    running.cancel()
    assert(never.isCancelled, "cancelling the okay side cancelled the CompletableFuture")
  }
