package okay.resilience

import okay.Timer
import java.util.concurrent.atomic.AtomicReference

/**
 * A timer the TEST fires, never the wall clock. `after` only arms the
 * callback; `fireAll` runs whatever is still armed and answers how many
 * there were, so "nothing else started" is asserted against the timer
 * firing as late as it possibly can, not against the box being fast
 * enough (hedge-timed-flake, 2026-09-09).
 *
 * One copy for the module's suites (faults-replay-wall-clock, 2026-09-28):
 * TestResilienceTimed and TestHedgeStart each carried their own, and
 * TestFaults' replay law is the third user — a tool three tests need is
 * a test tool, not a nested class (AGENTS.md, "a test's failure carries
 * its diagnosis": what is invented while debugging goes beside the
 * tests, not into the one that needed it).
 */
final class ManualTimer extends Timer:
  private val armed = AtomicReference(Vector.empty[() => Unit])
  def after(millis: Long)(k: () => Unit): () => Unit =
    armed.updateAndGet(_ :+ k)
    () => { armed.updateAndGet(_.filterNot(_ eq k)); () }
  /** run every callback still armed; answers how many there were */
  def fireAll(): Int =
    val ks = armed.getAndSet(Vector.empty)
    ks.foreach(_())
    ks.size
  /** how many callbacks are armed and not yet fired or cancelled */
  def pending: Int = armed.get.size
