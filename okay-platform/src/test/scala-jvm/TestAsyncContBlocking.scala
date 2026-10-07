package okay

import okay.AsyncCont.{async, await}

/** Async on the machine answered in place (cont-first-module): a Run executed, an Await parked */
class TestAsyncContBlocking extends munit.FunSuite {

  test("the blocking handler answers each operation in place, an Await parked until its callback") {
    val prog: Int ! (Async +: Pure) = for
      a <- async(20)
      b <- await[Int](k => Thread.ofVirtual().start(() => { Thread.sleep(5); k(22) }): Unit)
    yield a + b
    assertEquals(AsyncCont.run(prog), 42)
  }

  test("a long chain under the blocking handler runs in constant stack") {
    def go(n: Int): Int ! (Async +: Pure) =
      if n == 0 then pure(0) else async(1).flatMap(x => go(n - x).map(_ + x))
    assertEquals(AsyncCont.run(go(100000)), 100000)
  }

  test("the callback drive under real races: each Await answered from another thread, during or after its registration") {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(4)
    try
      def go(n: Int, acc: Int): Int ! (Async +: Pure) =
        if n == 0 then pure(acc)
        else await[Int](k => pool.execute(() => k(1))).flatMap(x => async(acc + x).flatMap(a => go(n - 1, a)))
      val f = AsyncCont.runAsync(go(5000, 0))
      assertEquals(scala.concurrent.Await.result(f, scala.concurrent.duration.Duration(30, "s")), 5000)
    finally pool.shutdown()
  }
}
