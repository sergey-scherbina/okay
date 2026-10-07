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
}
