package okay.resilience

import okay.*
import okay.given
import okay.http.{Method, Request}

/**
 * The five pieces under an injected clock (specs/resilience.md,
 * stage 0). Everything here completes synchronously or parks on a
 * callback the test holds, so the suite runs unchanged on JS; what
 * needs a real timer or a real fiber is in the JVM suite.
 */
class TestResilience extends munit.FunSuite {

  var now = 0L
  val clock: () => Long = () => now

  /** cross-platform runner: the drive finishes inline */
  def run[A](prog: A ! Async): A =
    Async.runAsync(prog).value match
      case Some(t) => t.get
      case None => fail("the test program did not complete synchronously")

  def refusal[A](prog: A ! Async): Refused =
    Async.runAsync(prog).value match
      case Some(scala.util.Failure(r: Refused)) => r
      case Some(other) => fail(s"expected a Refused, got $other")
      case None => fail("the test program did not complete synchronously")

  // ── breaker ───────────────────────────────────────────────────────

  test("breaker: consecutive failures open it, open refuses without running, a probe closes it") {
    now = 0
    val b = Breaker("db", failures = 2, openMillis = 100, clock)
    var ran = 0
    def call(ok: Boolean): Int ! Async =
      b.protect(okay.async { ran += 1; if ok then 1 else throw RuntimeException("down") })()

    assertEquals(run(call(true)), 1)
    assertEquals(b.stats.state, Breaker.State.Closed)
    assertEquals(intercept[RuntimeException](run(call(false))).getMessage, "down")
    assertEquals(b.stats.consecutiveFailures, 1)
    assertEquals(intercept[RuntimeException](run(call(false))).getMessage, "down")
    assertEquals(b.stats.state, Breaker.State.Open)
    assertEquals(b.stats.opened, 1L)

    // open: not run, and the refusal knows how long
    val r = refusal(call(true))
    assertEquals(ran, 3)
    assertEquals(r.retryAfterMillis, Some(100L))
    assertEquals(b.stats.rejected, 1L)

    now = 60
    assertEquals(refusal(call(true)).retryAfterMillis, Some(40L))

    // the probe fails: open again, for a fresh openMillis from NOW
    now = 100
    assertEquals(intercept[RuntimeException](run(call(false))).getMessage, "down")
    assertEquals(b.stats.state, Breaker.State.Open)
    assertEquals(b.stats.opened, 2L)
    now = 150
    assertEquals(refusal(call(true)).retryAfterMillis, Some(50L))

    // the probe succeeds: closed, the count reset
    now = 200
    assertEquals(run(call(true)), 1)
    assertEquals(b.stats.state, Breaker.State.Closed)
    assertEquals(b.stats.consecutiveFailures, 0)
    assertEquals(b.stats.calls, 5L)
    assertEquals(b.stats.failures, 3L)
  }

  test("breaker: a success in Closed resets the count; the predicate can call a value a failure") {
    now = 0
    val b = Breaker("svc", failures = 2, openMillis = 10, clock)
    def status(code: Int): Int ! Async = b.protect(okay.async(code))(_.exists(_ >= 500))

    assertEquals(run(status(503)), 503)      // a value: answered AND counted
    assertEquals(run(status(200)), 200)
    assertEquals(b.stats.consecutiveFailures, 0)
    assertEquals(run(status(503)), 503)
    assertEquals(run(status(502)), 502)
    assertEquals(b.stats.state, Breaker.State.Open)
    assert(refusal(status(200)).isInstanceOf[Refused.BreakerOpen])
  }

  test("breaker: while the probe is in flight a second call is refused, with no wait to promise") {
    now = 0
    val b = Breaker("p", failures = 1, openMillis = 10, clock)
    assertEquals(intercept[RuntimeException](run(b.protect(okay.async(throw RuntimeException("x")))())).getMessage, "x")
    now = 10
    var release: Either[Throwable, Int] => Unit = null
    val probe = Async.runAsync(b.protect(Async.await[Int] { k => release = k; () => () })())
    assertEquals(b.stats.state, Breaker.State.HalfOpen)
    assertEquals(refusal(b.protect(okay.async(1))()).retryAfterMillis, None)
    release(Right(7))
    assertEquals(probe.value.get.get, 7)
    assertEquals(b.stats.state, Breaker.State.Closed)
  }

  // ── limiter ───────────────────────────────────────────────────────

  test("limiter: burst passes, the next is refused naming the wait, the clock refills, keys are independent") {
    now = 0
    val l = Limiter("api", ratePerSecond = 1, burst = 2, clock = clock)
    def hit(key: String): Int ! Async = l.admit(key)(okay.async(1))

    assertEquals(run(hit("a")), 1)
    assertEquals(run(hit("a")), 1)
    val r = refusal(hit("a"))
    assert(r.isInstanceOf[Refused.Exhausted])
    assertEquals(r.retryAfterMillis, Some(1000L))
    assertEquals(run(hit("b")), 1)          // another key, its own bucket
    now = 500
    assertEquals(refusal(hit("a")).retryAfterMillis, Some(500L))
    now = 1000
    assertEquals(run(hit("a")), 1)
    assertEquals(l.stats.admitted, 4L)
    assertEquals(l.stats.rejected, 2L)
    assertEquals(l.stats.keys, 2)
  }

  test("limiter: full buckets are evicted at the sweep, so keys follow activity") {
    now = 0
    val l = Limiter("peers", ratePerSecond = 1, burst = 1, clock = clock)
    for i <- 1 to 70 do
      now += 2000                             // every earlier bucket is full again
      assertEquals(run(l.admit(s"k$i")(okay.async(i))), i)
    // the sweep at call 64 kept only that call's key; six more since
    assertEquals(l.stats.keys, 7)
  }

  // ── bulkhead ──────────────────────────────────────────────────────

  test("bulkhead: permits hold, the queue parks, beyond the queue is refused, a release wakes the next") {
    val bh = Bulkhead("pool", permits = 1, queue = 1)
    var hold: Either[Throwable, Int] => Unit = null
    val first = Async.runAsync(bh.limit(Async.await[Int] { k => hold = k; () => () }))
    assertEquals(bh.stats.inFlight, 1)

    var secondRan = false
    val second = Async.runAsync(bh.limit(okay.async { secondRan = true; 2 }))
    assertEquals(bh.stats.waiting, 1)
    assert(!secondRan, "the second ran without a permit")

    val r = refusal(bh.limit(okay.async(3)))
    assert(r.isInstanceOf[Refused.BulkheadFull])
    assertEquals(bh.stats.rejected, 1L)

    hold(Right(1))
    assertEquals(first.value.get.get, 1)
    assertEquals(second.value.get.get, 2)
    assertEquals(bh.stats.inFlight, 0)
    assertEquals(bh.stats.waiting, 0)
  }

  test("bulkhead: a failure releases the permit; a cancel while waiting leaves the queue") {
    val bh = Bulkhead("pool", permits = 1, queue = 2)
    assertEquals(intercept[RuntimeException](run(bh.limit(okay.async(throw RuntimeException("boom"))))).getMessage, "boom")
    assertEquals(bh.stats.inFlight, 0)

    var hold: Either[Throwable, Int] => Unit = null
    val first = Async.runAsync(bh.limit(Async.await[Int] { k => hold = k; () => () }))
    val waiter = Async.spawn(bh.limit(okay.async(2)))
    // on the JVM the fiber parks, and later leaves, on its own thread
    def settle(n: Int): Unit =
      var spins = 0
      while bh.stats.waiting != n && spins < 10_000_000 do spins += 1
      assertEquals(bh.stats.waiting, n)
    settle(1)
    waiter.cancel()
    settle(0)
    hold(Right(1))
    assertEquals(first.value.get.get, 1)
    assertEquals(bh.stats.inFlight, 0)
  }

  // ── deadline ──────────────────────────────────────────────────────

  test("deadline: header out is the remaining budget, header in is a local deadline, damage reads as absent") {
    now = 1000
    val d = Deadline.in(500, clock)
    val r = Deadline.carry(Request.get("http://x/", Seq((Deadline.header, "9"))), d, clock)
    assertEquals(r.headers.filter(_._1 == Deadline.header), Seq((Deadline.header, "500")))
    now = 1200
    assertEquals(Deadline.carry(r, d, clock).headers.last, (Deadline.header, "300"))
    now = 2000
    assertEquals(Deadline.carry(r, d, clock).headers.last, (Deadline.header, "0"))

    now = 0
    assertEquals(Deadline.read(Request.get("http://y/", Seq(("X-Deadline-Ms", "250"))), clock), Some(Deadline(250)))
    assertEquals(Deadline.read(Request.get("http://y/", Seq((Deadline.header, "soon"))), clock), None)
    assertEquals(Deadline.read(Request.get("http://y/", Seq((Deadline.header, "-5"))), clock), None)
    assertEquals(Deadline.read(Request.get("http://y/"), clock), None)
  }

  test("deadline: an expired budget refuses before the program starts") {
    now = 100
    var ran = false
    val r = refusal(Deadline.enforce(Deadline(90), clock)(okay.async { ran = true; 1 }))
    assert(!ran)
    r match
      case e: Refused.DeadlineExceeded => assertEquals(e.remainingMillis, -10L)
      case other => fail(s"$other")
  }

  test("every refusal is one type, named by its piece") {
    val all: Seq[Refused] = Seq(
      Refused.BreakerOpen("b", Some(1)), Refused.BulkheadFull("h"),
      Refused.Exhausted("l", "k", Some(2)), Refused.DeadlineExceeded(-3))
    assertEquals(all.map(_.retryAfterMillis), Seq(Some(1L), None, Some(2L), None))
    assert(all.forall(_.getMessage.nonEmpty))
    val _ = Method.Get   // the http import is the deadline's; keep it honest on both platforms
  }
}
