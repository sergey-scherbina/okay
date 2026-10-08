package okay

import okay.AsyncCont.{async, attempt, await, awaitEither, enter, exit, fork, join, sleep}

/** Async on the machine (cont-first-module): the callback drive, one source for the JVM, JS and Native suites */
class TestAsyncCont extends munit.FunSuite {

  given scala.concurrent.ExecutionContext = munitExecutionContext

  type P[A] = A ! (Async +: Pure)

  test("a long chain of Run operations drives in constant stack") {
    def go(n: Int): P[Int] =
      if n == 0 then pure(0)
      else async(1).flatMap(x => go(n - x).map(_ + x))
    AsyncCont.runAsync(go(10000)).map(v => assertEquals(v, 10000))
  }

  test("an Await whose callback fires during registration continues the drive") {
    val prog: P[Int] = await[Int](k => k(21)).map(_ * 2)
    AsyncCont.runAsync(prog).map(v => assertEquals(v, 42))
  }

  test("an Await answered later re-enters the drive from the callback") {
    var k0: Int => Unit = _ => ()
    val prog: P[Int] = for
      a <- async(1)
      b <- await[Int](k => k0 = k)
      c <- async(b * 10)
    yield a + c
    val f = AsyncCont.runAsync(prog)
    assertEquals(f.value, None)
    k0(4)
    assertEquals(f.value.flatMap(_.toOption), Some(41))
  }

  test("many synchronous Awaits in a row stay in one loop") {
    def go(n: Int, acc: Int): P[Int] =
      if n == 0 then pure(acc) else await[Int](k => k(1)).flatMap(x => go(n - 1, acc + x))
    AsyncCont.runAsync(go(10000, 0)).map(v => assertEquals(v, 10000))
  }

  test("a failed Await fails the future at that operation; the rest does not run") {
    var after = false
    val boom = RuntimeException("boom")
    val prog: P[Int] = awaitEither[Int](k => { k(Left(boom)); () => () }).flatMap(x => async { after = true; x }.at)
    val f = AsyncCont.runAsync(prog)
    assertEquals(f.value.flatMap(_.failed.toOption), Some(boom))
    assert(!after)
  }

  test("a throwing Run fails the future") {
    val boom = RuntimeException("run")
    val f = AsyncCont.runAsync[Int](async[Int](throw boom).at[Async +: Pure])
    assertEquals(f.value.flatMap(_.failed.toOption), Some(boom))
  }

  // ---- cancellation, cancel scopes, attempt (async-cancel)

  test("cancel while parked: the Await is unregistered, the future fails, a late answer resumes nothing") {
    var unregistered = 0
    var k0: Int => Unit = _ => ()
    var after = false
    val prog: P[Int] = awaitEither[Int](k => { k0 = x => k(Right(x)); () => unregistered += 1 })
      .flatMap(x => async { after = true; x }.at)
    val r = AsyncCont.runAsyncCancellable(prog)
    r.cancel()
    r.cancel()   // idempotent
    assertEquals(unregistered, 1)
    assert(r.future.value.flatMap(_.failed.toOption).exists(_.isInstanceOf[java.util.concurrent.CancellationException]))
    k0(1)
    assert(!after, "a cancelled drive resumed")
  }

  test("a scope open when the drive is cancelled is released, once") {
    var released = 0
    val scope = Async.CancelScope(() => released += 1)
    val prog: P[Unit] = enter(scope).flatMap(_ => await[Unit](_ => ()).at)
    val r = AsyncCont.runAsyncCancellable(prog)
    assertEquals(released, 0)
    r.cancel()
    assertEquals(released, 1)
  }

  test("a scope exited is not released; one still open at the end is") {
    var a = 0
    var b = 0
    val sa = Async.CancelScope(() => a += 1)
    val sb = Async.CancelScope(() => b += 1)
    val prog: P[Unit] = for
      _ <- enter(sa)
      _ <- exit(sa)
      _ <- enter(sb)
    yield ()
    val f = AsyncCont.runAsync(prog)
    assertEquals(f.value.flatMap(_.toOption), Some(()))
    assertEquals((a, b), (0, 1))
  }

  test("attempt: a failure is a value and the program goes on; a success is Right") {
    val boom = RuntimeException("inner")
    val prog: P[(Either[Throwable, Int], Either[Throwable, Int])] = for
      bad <- attempt[Int](async[Int](throw boom).at)
      good <- attempt[Int](async(41).at)
    yield (bad, good)
    assertEquals(AsyncCont.runAsync(prog).value.flatMap(_.toOption), Some((Left(boom), Right(41))))
  }

  test("attempt under a cancel: the inner program is cancelled with the outer") {
    var innerUnregistered = 0
    val inner: P[Int] = awaitEither[Int](_ => () => innerUnregistered += 1).at
    val r = AsyncCont.runAsyncCancellable(attempt(inner).at[Async +: Pure])
    r.cancel()
    assertEquals(innerUnregistered, 1)
  }

  // ---- fibers (async-fibers)

  test("par pairs two answers by completion callbacks, no parking") {
    val prog = AsyncCont.par(sleep(20).map(_ => 1), sleep(10).map(_ => 2))
    AsyncCont.runAsync(prog.at[Async +: Pure]).map(v => assertEquals(v, (1, 2)))
  }

  test("a child failure fails par and cancels the sibling") {
    val boom = RuntimeException("boom")
    val prog = AsyncCont.par(async[Int](throw boom).at[Async +: Pure], sleep(500).map(_ => 2))
    AsyncCont.runAsync(prog.at[Async +: Pure]).failed.map(e => assertEquals(e.getMessage, "boom"))
  }

  test("race: the first to succeed wins") {
    val never: P[String] = await[String](_ => ()).at
    val prog = AsyncCont.race(never, sleep(1).map(_ => "fast"))
    AsyncCont.runAsync(prog.at[Async +: Pure]).map(v => assertEquals(v, "fast"))
  }

  test("fork and join: a fiber joined as an operation") {
    val prog: P[Int] = for
      f <- fork(sleep(10).map(_ => 21))
      x <- join(f)
    yield x * 2
    AsyncCont.runAsync(prog).map(v => assertEquals(v, 42))
  }

  test("timeout: None when the program is slower; its own failure comes through at once") {
    val boom = RuntimeException("now")
    val slow = AsyncCont.timeout(10)(sleep(1000).map(_ => 1))
    val failing = AsyncCont.timeout(1000)(async[Int](throw boom).at[Async +: Pure])
    for
      a <- AsyncCont.runAsync(slow.at[Async +: Pure])
      b <- AsyncCont.runAsync(failing.at[Async +: Pure]).failed
    yield
      assertEquals(a, None)
      assertEquals(b.getMessage, "now")
  }
}

