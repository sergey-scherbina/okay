package okay.resilience

import okay.*
import okay.given

/**
 * The guards around a STREAMING seam (specs/resilience.md,
 * resilient-transport). `Resilient.http` fits a request-in,
 * response-out shape; the repo's other seams — `okay.llm.Transport`,
 * `okay.mcp.Link`, `okay.cluster.Remote` — post and then TELL their
 * answer, so their programs carry a Writer beside Async. The thing
 * worth proving is that a guard spans the whole stream and not just
 * its first operation.
 */
class TestGuarded extends munit.FunSuite:

  var now = 0L
  val clock: () => Long = () => now

  /** the shape of every streaming seam here: tells its lines */
  type Lines = Writer % String

  def run[A](prog: A ! (Lines + Async)): (Vector[String], A) =
    val collected = Writer.run[String, A, Async](prog)
    Async.runAsync(collected).value match
      case Some(t) => val (ls, a) = t.get; (ls.toVector, a)
      case None => fail("the test program did not complete synchronously")

  def refusal[A](prog: A ! (Lines + Async)): Refused =
    Async.runAsync(Writer.run[String, A, Async](prog)).value match
      case Some(scala.util.Failure(r: Refused)) => r
      case Some(other) => fail(s"expected a Refused, got $other")
      case None => fail("the test program did not complete synchronously")

  /** lift each half into the row once, so the seams below read plainly */
  def act(f: => Unit): Unit ! (Lines + Async) = !.widen[Unit, Async, Lines](okay.async(f))
  def say(line: String): Unit ! (Lines + Async) = !.widen[Unit, Lines, Async](Writer.tell(line))
  def park(register: (Either[Throwable, Unit] => Unit) => Unit): Unit ! (Lines + Async) =
    !.widen[Unit, Async, Lines](Async.await[Unit] { k => register(k); () => () })

  /** a seam that tells three lines, optionally failing part-way */
  def stream(failAfter: Int = -1): Unit ! (Lines + Async) =
    def line(i: Int): Unit ! (Lines + Async) =
      act(if failAfter == i then throw java.io.IOException(s"wire died at $i") else ()).flatMap(_ => say(s"line $i"))
    line(1).flatMap(_ => line(2)).flatMap(_ => line(3))

  test("the whole stream is told, and the guards leave it alone when nothing refuses") {
    val breaker = Breaker("llm", failures = 2, openMillis = 1000, clock)
    val bulkhead = Bulkhead("llm", permits = 2)
    val limiter = Limiter("llm", ratePerSecond = 100, burst = 100, clock = clock)
    val (lines, _) = run(Resilient.guarded(stream(),
      breaker = Some(breaker), bulkhead = Some(bulkhead), limiter = Some(limiter)))
    assertEquals(lines, Vector("line 1", "line 2", "line 3"))
    assertEquals(breaker.stats.calls, 1L)
    assertEquals(limiter.stats.admitted, 1L)
    assertEquals(bulkhead.stats.inFlight, 0)      // released after the LAST line, not the first
  }

  test("a failure PART-WAY through the stream is the breaker's failure, and the lines before it stand") {
    val breaker = Breaker("llm", failures = 1, openMillis = 1000, clock)
    val e = intercept[java.io.IOException](run(Resilient.guarded(stream(failAfter = 2), breaker = Some(breaker))))
    assertEquals(e.getMessage, "wire died at 2")
    assertEquals(breaker.stats.failures, 1L)
    assertEquals(breaker.stats.state, Breaker.State.Open)
    // and the next call is refused without the seam being touched
    assert(refusal(Resilient.guarded(stream(), breaker = Some(breaker))).isInstanceOf[Refused.BreakerOpen])
  }

  test("THE POINT: the permit spans the whole stream, not its first line") {
    val bulkhead = Bulkhead("llm", permits = 1)
    var release: Either[Throwable, Unit] => Unit = null
    // a seam that tells one line, then parks mid-stream
    val parking: Unit ! (Lines + Async) =
      say("first").flatMap(_ => park(k => release = k)).flatMap(_ => say("last"))

    val inFlight = Async.runAsync(Writer.run[String, Unit, Async](
      Resilient.guarded(parking, bulkhead = Some(bulkhead))))
    assertEquals(bulkhead.stats.inFlight, 1, "the first line already came out; the permit must still be held")

    // the permit is taken, so a second caller is refused
    assert(refusal(Resilient.guarded(stream(), bulkhead = Some(bulkhead))).isInstanceOf[Refused.BulkheadFull])

    release(Right(()))
    val (lines, _) = inFlight.value.get.get
    assertEquals(lines.toVector, Vector("first", "last"))
    assertEquals(bulkhead.stats.inFlight, 0)      // released only now
  }

  test("the limiter refuses before the seam is touched at all") {
    val limiter = Limiter("llm", ratePerSecond = 1, burst = 1, clock = clock)
    var started = 0
    def counted: Unit ! (Lines + Async) = act(started += 1).flatMap(_ => stream())
    val _ = run(Resilient.guarded(counted, limiter = Some(limiter)))
    assertEquals(started, 1)
    val r = refusal(Resilient.guarded(counted, limiter = Some(limiter)))
    assertEquals(r.retryAfterMillis, Some(1000L))
    assertEquals(started, 1, "a refused call never reached the seam")
  }

  test("no guards at all is the program unchanged") {
    assertEquals(run(Resilient.guarded(stream()))._1, Vector("line 1", "line 2", "line 3"))
  }
