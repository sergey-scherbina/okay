package okay

import !.*

/** The resource region: releases at the end of the scope, no matter what. */
class TestResource extends munit.FunSuite {

  test("releases in reverse acquisition order at the end of the scope") {
    var log = List.empty[String]
    def res(n: String) = Resource.acquire { log ::= s"open $n"; n } (r => log ::= s"close $r")
    val prog = res("a").flatMap(a => res("b").map(b => a + b))
    assertEquals(!.run(Resource.run[String, Nothing](prog)), "ab")
    assertEquals(log.reverse, List("open a", "open b", "close b", "close a"))
  }

  test("an abort handled inside the scope still releases") {
    var released = false
    type F = Throws % String + Resource
    val prog: Int ! F =
      effect[F, Unit](Resource.Acquire(() => (), _ => released = true)).flatMap: _ =>
        effect[F, Int](Throws("boom"))
    val either = !.run(Resource.run[Either[String, Int], Nothing](
      runEither[Int, Resource, String](prog)))
    assertEquals(either, Left("boom"))
    assertEquals(released, true)
  }

  test("a JVM exception during a step still releases") {
    var released = false
    val prog: Int ! Resource = Resource.acquire(())(_ => released = true)
      .flatMap(_ => pure[Resource, Int](0).map(_ => throw RuntimeException("boom")))
    intercept[RuntimeException](!.run(Resource.run[Int, Nothing](prog))): Unit
    assertEquals(released, true)
  }

  test("forwarded effects: finalizers travel with the residual") {
    var released = false
    type F = Resource + Produce
    val prog: Int ! F =
      effect[F, Unit](Resource.Acquire(() => (), _ => released = true)).flatMap: _ =>
        effect[F, Int](41).map(_ + 1)
    val residual: Int ! Produce = Resource.run[Int, Produce](prog)
    assertEquals(released, false)
    assertEquals(residual.runWith, 42)
    assertEquals(released, true)
  }

  test("a throw in the continuation AFTER a forwarded effect still releases") {
    // the leak sql-seam found: the residual applies k(y) at the OUTER
    // handler's call site, outside the region's own try — user code
    // composed after a forwarded effect (a .map that throws) must not
    // skip the finalizers
    var released = false
    type F = Resource + Produce
    val prog: Int ! F =
      effect[F, Unit](Resource.Acquire(() => (), _ => released = true)).flatMap: _ =>
        effect[F, Int](41).map(_ => throw RuntimeException("boom"))
    val residual: Int ! Produce = Resource.run[Int, Produce](prog)
    assertEquals(released, false)
    intercept[RuntimeException](residual.runWith): Unit
    assertEquals(released, true)
  }

  test("bracket over any Handler-able row, not only Async") {
    var released = 0
    assertEquals(bracket(41)(_ => released += 1)(r => async(r + 1)).runWith, 42)
    assertEquals(bracket(1)(_ => released += 1)(r => produce(r + 1)).runWith, 2)
    val _ = intercept[RuntimeException]:
      bracket(0)(_ => released += 1)(_ => async[Int](throw RuntimeException("boom"))).runWith
    assertEquals(released, 3)
  }
}
