package okay

import java.util.concurrent.locks.LockSupport

/**
 * The consumer's WAIT, one rung at a time (poll-then-park's hybrid,
 * specs/ready-merge.md): a merge whose ring ran dry spins, then yields,
 * then sleeps briefly, and only then registers and blocks. Every rung
 * costs the consumer alone; the producer pays only for the last one.
 * Measured on this box (JDK 26, macOS): `Thread.yield` 125 ns,
 * `parkNanos(1)` 10-12 us — the timer's floor, and the window in which
 * two producers get ~50 chunks ahead, so the merge wakes into batches.
 */
private[okay] object Wait:
  /** producers are threads: a poll can find what the last one missed */
  final val Threads = true
  def yieldNow(): Unit = Thread.`yield`()
  def sleepBriefly(): Unit = LockSupport.parkNanos(1000L)
