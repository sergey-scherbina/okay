package okay.diagnose

import java.time.Duration

class TestLateOrLost extends munit.FunSuite {
  test("LateOrLost: on time, late with the snapshot taken at the first deadline, lost") {
    def sleeper(ms: Long) = { val t = new Thread(() => Thread.sleep(ms)); t.start(); t }
    assertEquals(LateOrLost.join(sleeper(1), Duration.ofSeconds(5), Duration.ofSeconds(5))("s"), LateOrLost.Outcome.OnTime)
    LateOrLost.join(sleeper(300), Duration.ofMillis(20), Duration.ofSeconds(5))("snap") match
      case LateOrLost.Outcome.Late(at) => assert(at.contains("TIMED_WAITING") && at.contains("snap"), at)
      case other => fail(s"expected Late: $other")
    val parked = new Thread(() => java.util.concurrent.locks.LockSupport.park())
    parked.setDaemon(true); parked.start()
    LateOrLost.join(parked, Duration.ofMillis(20), Duration.ofMillis(50))("snap") match
      case LateOrLost.Outcome.Lost(at) => assert(at.contains("WAITING"), at)
      case other => fail(s"expected Lost: $other")
  }
}
