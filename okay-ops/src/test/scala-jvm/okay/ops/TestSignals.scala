package okay.ops

import okay.*
import okay.given
import okay.http.{Http, Request, Response}
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit.SECONDS

/**
 * The JVM stop sequence, fired in-process (specs/ops.md): the same
 * steps the shutdown hook triggers, without killing the test JVM.
 *
 * BOTH tests used to wait for the spawned request with a bounded spin
 * — `while hold == null && spins < 10_000_000 do spins += 1` — and
 * then assert unconditionally. That loop exits on EITHER condition, so
 * an exhausted budget was indistinguishable from a successful wait:
 * under load the spawned thread did not get a core within ten million
 * iterations, `hold` stayed null, and the next line asserted about a
 * request that had not started. Measured 2026-09-10 at load ~20 on
 * UNMODIFIED master: two failures in three runs, the same rate as in a
 * worktree that had not touched okay-ops — which is how it was
 * established to be the test and not a change.
 *
 * A latch is a real wait, and it gives the happens-before edge the
 * plain `var` never had, so the timeout became a timeout and the
 * cross-thread read became legal in one move.
 */
class TestSignals extends munit.FunSuite:

  test("stop: readiness off, a delay, then the drain waits for the request in flight") {
    val l = Lifecycle()
    val started = CountDownLatch(1)
    var hold: Either[Throwable, Response] => Unit = null
    val routes = l.route { case _ => Async.await[Response] { k => hold = k; started.countDown(); () => () } }
    val pending = Async.spawn(routes(Request.get("/slow")))
    assert(started.await(10, SECONDS), "the request never reached its await")
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
    val started = CountDownLatch(1)
    var hold: Either[Throwable, Response] => Unit = null
    val routes = l.route { case _ => Async.await[Response] { k => hold = k; started.countDown(); () => () } }
    val pending = Async.spawn(routes(Request.get("/slow")))
    assert(started.await(10, SECONDS), "the request never reached its await")
    assertEquals(Signals.stop(l, readinessDelayMillis = 0, graceMillis = 60), false)
    assertEquals(l.inFlight, 1)
    hold(Right(Response(200, Nil, Http.one(Array.empty))))
    assertEquals(pending.join().status, 200)
  }
