package okay

import !.*

/** Loom-style asynchrony: virtual threads, parked blocking, par/race. */
class TestAsync extends munit.FunSuite {

  test("async ops run in place: run is a relay") {
    val prog: Int ! Async = async(20).flatMap(x => async(x + 22))
    assertEquals(!.run(Async.run[Int, Nothing](prog)), 42)
    assertEquals(prog.runWith, 42)
  }

  test("spawn runs on a virtual thread; blocking parks it") {
    val f = Async.spawn:
      async(Thread.currentThread().isVirtual).flatMap: v =>
        async { Thread.sleep(10); v }
    assertEquals(f.join(), true)
  }

  test("par runs both sides at once, on their own virtual threads") {
    val t0 = System.nanoTime()
    val prog = Async.par(
      async { Thread.sleep(200); 1 },
      async { Thread.sleep(200); 2 })
    assertEquals(prog.runWith, (1, 2))
    val ms = (System.nanoTime() - t0) / 1000000
    assert(ms < 390, s"not parallel: took ${ms}ms")
  }

  test("race answers with the faster side") {
    val prog = Async.race(
      async { Thread.sleep(200); "slow" },
      async { Thread.sleep(10); "fast" })
    assertEquals(prog.runWith, "fast")
  }

  test("async composes with other effects: telling across suspensions") {
    type F = Async + Writer % String
    val prog: Int ! F =
      effect[F, String](Writer("start")).flatMap: _ =>
        effect[F, Int](Async.Run(() => 21)).flatMap: x =>
          effect[F, String](Writer("end")).map(_ => x * 2)
    val (ws, a) = !.run(Writer.run[String, Int, Nothing](
      Async.run[Int, Writer % String](prog)))
    assertEquals(ws, Seq("start", "end"))
    assertEquals(a, 42)
  }
}
