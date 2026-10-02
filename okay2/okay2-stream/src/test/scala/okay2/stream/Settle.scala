package okay2.stream

import java.util.concurrent.atomic.AtomicInteger

/**
 * Wait for a counter that a feeder bumps to STAND STILL: unchanged over `quietMs`, at most `deadlineMs`.
 * A feeder parks once its buffer is full, but on a loaded box the catch-up to "full" can outlast any fixed
 * sleep between two reads (okay2-joinwithin-settle-flake: a 50 ms sleep red twice in full gates, 10/10
 * green alone). So the wait is for the property, and the test then bounds the count — an unparked endless
 * side would be at millions by the deadline. Answers whether it stood still, and the last count.
 */
object Settle {
  def await(counter: AtomicInteger, quietMs: Long = 50, deadlineMs: Long = 10000): (Boolean, Int) = {
    val deadline = System.nanoTime() + deadlineMs * 1000000L
    var last = counter.get
    var still = false
    while (!still && System.nanoTime() < deadline) {
      Thread.sleep(quietMs)
      val now = counter.get
      still = now == last
      last = now
    }
    (still, last)
  }
}
