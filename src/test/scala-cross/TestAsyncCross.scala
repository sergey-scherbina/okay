package okay

import !.*
import scala.concurrent.Promise

/**
 * The cross-platform Async surface (specs/cross-platform-async.md):
 * ONE source, run by the JVM suite and by the JS suite under Node —
 * Await-based programs only, driven by runAsync, no CanBlock
 * anywhere. munit awaits the returned Future.
 */
class TestAsyncCross extends munit.FunSuite {

  given scala.concurrent.ExecutionContext = munitExecutionContext

  test("a long chain of Run operations drives in constant stack") {
    def go(n: Int): Int ! Async =
      if n == 0 then pure(0)
      else async(1).flatMap(x => go(n - x).map(_ + x))
    Async.runAsync(go(10000)).map(v => assertEquals(v, 10000))
  }

  test("an Await whose callback fires during registration continues the drive") {
    val prog = await[Int](k => k(21)).map(_ * 2)
    Async.runAsync(prog).map(v => assertEquals(v, 42))
  }

  test("sleep then answer completes via runAsync without blocking the loop") {
    @volatile var interleaved = false
    summon[Timer].after(10)(() => interleaved = true)
    Async.runAsync(Async.sleep(50).map(_ => 42)).map: v =>
      assertEquals(v, 42)
      assert(interleaved, "the timer should have fired while we slept")
  }

  test("race answers the first to finish, the loser is cancelled") {
    val prog = Async.race(
      Async.sleep(100).map(_ => "slow"),
      Async.sleep(10).map(_ => "fast"))
    Async.runAsync(prog).map(v => assertEquals(v, "fast"))
  }

  test("cancel stops a spawned program before its next operation") {
    @volatile var ran = false
    val f = Async.spawn(Async.sleep(50).map(_ => { ran = true; 1 }))
    f.cancel()
    val p = Promise[Unit]()
    summon[Timer].after(150)(() => p.success(()))
    p.future.map(_ => assert(!ran, "the cancelled program should not resume"))
  }

  test("onComplete observes a fiber on every platform") {
    val p = Promise[Int]()
    Async.spawn(Async.sleep(10).map(_ => 7)).onComplete {
      case Right(v) => p.success(v)
      case Left(e) => p.failure(e)
    }
    p.future.map(v => assertEquals(v, 7))
  }
}
