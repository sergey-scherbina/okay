package okay

import okay.Direct.*
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.atomic.AtomicInteger

/** specs/direct-foreign-mark.md: a foreign effect marked inside a direct
 * block over an okay program, through a given ForeignEffect */
class TestDirectForeign extends munit.FunSuite:

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
