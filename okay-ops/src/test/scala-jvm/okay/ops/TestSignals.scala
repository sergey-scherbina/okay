package okay.ops

import okay.*
import okay.given
import okay.http.{Http, Request, Response}

/**
 * The JVM stop sequence, fired in-process (specs/ops.md): the same
 * steps the shutdown hook triggers, without killing the test JVM.
 */
class TestSignals extends munit.FunSuite:

  test("stop: readiness off, a delay, then the drain waits for the request in flight") {
    val l = Lifecycle()
    var hold: Either[Throwable, Response] => Unit = null
    val routes = l.route { case _ => Async.await[Response] { k => hold = k; () => () } }
    val pending = Async.spawn(routes(Request.get("/slow")))
    var spins = 0
    while hold == null && spins < 10_000_000 do spins += 1
    assertEquals(l.inFlight, 1)

    // release the request from another thread while the stop is draining
    val releaser = Thread(() => { Thread.sleep(60); hold(Right(Response(200, Nil, Http.one(Array.empty)))) })
    releaser.start()
    val drained = Signals.stop(l, readinessDelayMillis = 10, graceMillis = 2_000)
    assert(drained, "the drain did not see the request finish")
    assert(l.draining)
    assertEquals(l.inFlight, 0)
    assertEquals(pending.join().status, 200)
    releaser.join()
  }

  test("stop: a request that outlives the grace makes the drain answer false, and it is still counted") {
    val l = Lifecycle()
    var hold: Either[Throwable, Response] => Unit = null
    val routes = l.route { case _ => Async.await[Response] { k => hold = k; () => () } }
    val pending = Async.spawn(routes(Request.get("/slow")))
    var spins = 0
    while hold == null && spins < 10_000_000 do spins += 1
    assertEquals(Signals.stop(l, readinessDelayMillis = 0, graceMillis = 60), false)
    assertEquals(l.inFlight, 1)
    hold(Right(Response(200, Nil, Http.one(Array.empty))))
    assertEquals(pending.join().status, 200)
  }
