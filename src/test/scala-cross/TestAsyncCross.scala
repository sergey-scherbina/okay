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

  test("par pairs two answers by completion callbacks, no parking") {
    val prog = Async.par(
      Async.sleep(20).map(_ => 1),
      Async.sleep(10).map(_ => 2))
    Async.runAsync(prog).map(v => assertEquals(v, (1, 2)))
  }

  test("a child failure fails par and cancels the sibling") {
    val boom = RuntimeException("boom")
    val prog = Async.par(async[Int](throw boom), Async.sleep(500).map(_ => 2))
    Async.runAsync(prog).failed.map(e => assertEquals(e.getMessage, "boom"))
  }

  test("joinAsync joins a fiber as an operation") {
    val f = Async.spawn(Async.sleep(10).map(_ => 21))
    Async.runAsync(f.joinAsync.map(_ * 2)).map(v => assertEquals(v, 42))
  }

  test("a race of two failures fails instead of hanging") {
    val prog = Async.race(
      async[Int](throw RuntimeException("a")),
      async[Int](throw RuntimeException("b")))
    Async.runAsync(prog).failed.map(e =>
      assert(e.getMessage == "a" || e.getMessage == "b"))
  }

  test("an Await's Left is the error channel: it fails the program") {
    val boom = RuntimeException("wire down")
    val prog = Async.await[Int](k => { k(Left(boom)); () => () })
    Async.runAsync(prog).failed.map(e => assertEquals(e.getMessage, "wire down"))
  }

  test("a channel bridges sent values into an Async stream on every platform") {
    val c = Channel[Int]()
    c.send(1); c.send(2); c.close()
    val ch = summon[Stream[Channel, Async]]
    def drain(acc: List[Int]): List[Int] ! Async =
      ch.uncons(c).flatMap {
        case Some((a, _)) => drain(a :: acc)
        case None => pure(acc.reverse)
      }
    Async.runAsync(drain(Nil)).map(v => assertEquals(v, List(1, 2)))
  }
}
