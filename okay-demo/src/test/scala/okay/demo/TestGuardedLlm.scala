package okay.demo

import okay.*
import okay.given
import okay.resilience.{Breaker, Limiter, Refused}

/**
 * The arc's worked instance (demo-guarded-llm): the demo's outbound
 * model call really is guarded, and the guards really do reach
 * `/metrics`. Eight pieces of machinery landed with tests of their
 * own today; this is the one test that says a SERVICE uses them.
 *
 * Nothing here touches a wire or a clock: the transport under test
 * is a fake that counts, and the guards are driven to their
 * refusals by their own contracts.
 */
class TestGuardedLlm extends munit.FunSuite:

  given Timer = summon[Timer]

  /**
   * A clock PER TEST, not per suite. The first draft shared one `now`
   * across the suite; a test that left it at 30 000 made the next
   * one's `now = 200` run time BACKWARD, the refill went negative, and
   * the token never came back. The failure looked like a limiter bug
   * and was a fixture bug — the same shape as any test that depends on
   * ambient state.
   */
  final class Clock:
    var now = 0L
    val get: () => Long = () => now
    def set(t: Long): Unit = now = t

  def run[A](prog: A ! (Writer % String + Async)): (Vector[String], A) =
    Async.runAsync(Writer.run[String, A, Async](prog)).value match
      case Some(t) => val (ls, a) = t.get; (ls.toVector, a)
      case None => fail("the test program did not complete synchronously")

  def refusal[A](prog: A ! (Writer % String + Async)): Refused =
    Async.runAsync(Writer.run[String, A, Async](prog)).value match
      case Some(scala.util.Failure(r: Refused)) => r
      case Some(other) => fail(s"expected a Refused, got $other")
      case None => fail("the test program did not complete synchronously")

  /** a model wire that tells two lines, or dies */
  final class Fake(var dies: Boolean = false) extends okay.llm.Transport:
    var calls = 0
    def post(url: String, headers: Map[String, String], body: String)
    : Unit ! (Writer % String + Async) =
      !.widen[Unit, Async, Writer % String](okay.async {
        calls += 1
        if dies then throw java.io.IOException("model unreachable")
      }).flatMap(_ => !.widen[Unit, Writer % String, Async](Writer.tell("data: {}")))
        .flatMap(_ => !.widen[Unit, Writer % String, Async](Writer.tell("data: [DONE]")))

  /** the demo's own wiring, with guards a test can drive */
  def guardedBy(inner: okay.llm.Transport, breaker: Breaker, limiter: Limiter): okay.llm.Transport =
    new okay.llm.Transport:
      def post(url: String, headers: Map[String, String], body: String)
      : Unit ! (Writer % String + Async) =
        okay.resilience.Resilient.guarded(inner.post(url, headers, body),
          breaker = Some(breaker), limiter = Some(limiter), key = "anthropic")

  test("the demo's own guards exist, are named, and are the ones /metrics is given") {
    // not a tautology: this is what catches someone wiring a guard and
    // forgetting to publish it, which is how a breaker goes unnoticed
    assertEquals(ChatDemo.llmBreaker.name, "anthropic")
    assertEquals(ChatDemo.llmLimiter.name, "anthropic")
    val metrics = Async.run(ChatDemo.opsRoutes()(okay.http.Request.get("/metrics"))
      .flatMap(okay.http.Http.text)).runWith
    assert(metrics.contains("""okay_breaker_state{name="anthropic"}"""), metrics)
    assert(metrics.contains("""okay_limiter_keys{name="anthropic"}"""), metrics)
  }

  test("a dead model opens the circuit, and the wire is not touched again while it is open") {
    val clock = Clock()
    val wire = Fake(dies = true)
    val breaker = Breaker("anthropic", failures = 2, openMillis = 30_000, clock.get)
    val limiter = Limiter("anthropic", ratePerSecond = 100, burst = 100, clock = clock.get)
    val t = guardedBy(wire, breaker, limiter)

    for _ <- 1 to 2 do
      intercept[java.io.IOException](run(t.post("https://api", Map.empty, "{}"))): Unit
    assertEquals(breaker.stats.state, Breaker.State.Open)
    assertEquals(wire.calls, 2)

    val r = refusal(t.post("https://api", Map.empty, "{}"))
    assert(r.isInstanceOf[Refused.BreakerOpen])
    assertEquals(r.retryAfterMillis, Some(30_000L))
    assertEquals(wire.calls, 2, "an open circuit must not reach the model")

    // and it heals: one probe after the open window, then closed
    clock.set(30_000)
    wire.dies = false
    assertEquals(run(t.post("https://api", Map.empty, "{}"))._1, Vector("data: {}", "data: [DONE]"))
    assertEquals(breaker.stats.state, Breaker.State.Closed)
  }

  test("the token bucket bounds a runaway loop before it spends the quota") {
    val clock = Clock()
    val wire = Fake()
    val breaker = Breaker("anthropic", failures = 5, openMillis = 30_000, clock.get)
    val limiter = Limiter("anthropic", ratePerSecond = 5, burst = 3, clock = clock.get)
    val t = guardedBy(wire, breaker, limiter)

    for _ <- 1 to 3 do run(t.post("https://api", Map.empty, "{}")): Unit
    assertEquals(wire.calls, 3)
    val r = refusal(t.post("https://api", Map.empty, "{}"))
    assert(r.isInstanceOf[Refused.Exhausted])
    assertEquals(wire.calls, 3, "a refused call never reached the model")
    assertEquals(limiter.stats.rejected, 1L)

    clock.set(200)                             // one token back at 5/s
    assertEquals(run(t.post("https://api", Map.empty, "{}"))._2, ())
    assertEquals(wire.calls, 4)
  }

  test("the guard is transparent when nothing refuses: every line of the answer arrives") {
    val clock = Clock()
    val wire = Fake()
    val t = guardedBy(wire, Breaker("anthropic", 5, 30_000, clock.get),
      Limiter("anthropic", ratePerSecond = 100, burst = 100, clock = clock.get))
    assertEquals(run(t.post("https://api", Map.empty, "{}"))._1, Vector("data: {}", "data: [DONE]"))
  }
