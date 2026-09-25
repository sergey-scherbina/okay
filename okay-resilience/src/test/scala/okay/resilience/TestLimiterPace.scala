package okay.resilience

/** Limiter.pace: a blocking caller's interval (limiter-blocking-pace) */
class TestLimiterPace extends munit.FunSuite:

  final class Clocked(perMinute: Int):
    var now = 0L
    val slept = Vector.newBuilder[Long]
    val limiter = Limiter("pace", perMinute / 60.0, burst = 1, clock = () => now)
    def pace(): Long = limiter.pace()(ms => { slept += ms; now += ms })

  test("sixty a minute is one a second: the first at once, then an interval") {
    val c = Clocked(60)
    assertEquals(Vector(c.pace(), c.pace(), c.pace()), Vector(0L, 1000L, 1000L))
    assertEquals(c.slept.result(), Vector(1000L, 1000L))
  }

  test("time already spent counts: a slow caller is never made to wait") {
    val c = Clocked(60)
    assertEquals(c.pace(), 0L)
    c.now += 5000
    assertEquals(c.pace(), 0L)
  }

  test("never refused, whatever maxWaitMillis says; and counted as admitted and delayed") {
    var now = 0L
    val l = Limiter("pace", 1.0 / 60, burst = 1, maxWaitMillis = 0, clock = () => now)
    assertEquals(l.pace()(ms => now += ms), 0L)
    assertEquals(l.pace()(ms => now += ms), 60000L)
    assertEquals(l.stats, Limiter.Stats(keys = 1, admitted = 2, delayed = 1, rejected = 0))
  }

  test("a silly rate still waits at least a millisecond between two calls") {
    val c = Clocked(1_000_000)
    assertEquals(c.pace(), 0L)
    assertEquals(c.pace(), 1L)
  }
