package okay

import okay.AsyncCont.{async, await, awaitEither}

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
}
