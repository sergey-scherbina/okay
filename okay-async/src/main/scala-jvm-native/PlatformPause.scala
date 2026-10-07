package okay


import java.util.concurrent.locks.LockSupport

/** the platform's primitives of a wait (`Pause`): producers are threads
 * here, a yield is `Thread.yield` (~125 ns on this box), a nano-sleep is
 * `parkNanos` (10-12 us: the timer's floor, the window in which two
 * producers make ~50 chunks). A spin is a plain re-poll — no
 * `onSpinWait`, for Native's portability (AdaptiveFifo's reason) */
private[okay] object PlatformPause extends Pause:
  def threads: Boolean = true
  def spin(): Unit = ()
  def yieldNow(): Unit = Thread.`yield`()
  def nano(): Unit = LockSupport.parkNanos(1000L)
  def block(): Unit = ()
