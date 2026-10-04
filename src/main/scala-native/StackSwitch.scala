package okay

import java.util.concurrent.atomic.AtomicLong

/** A fresh stack for a deep direct-style Cont program, on Scala Native: counted levels, then a 1 GB thread. */
private[okay] object StackSwitch:

  /** bytes one level takes cold — the JVM's measured constant (an interpreted level, the largest there is), kept
   * for one runner on every platform: what the 1 GB room is derived from */
  val coldBytesPerLevel: Long = 2600L

  /** levels the caller's stack is asked to hold before the switch.
   * SMALL here, unlike the JVM's: a first room derived from the thread
   * this object initialises on (the main thread's 8 MB) is wrong for
   * every other thread. `okay.cont.room` overrides. 16 since Cont runs on the frame machine (cont-on-frames,
   * 2026-10-01): a strict `k` is a nested machine run — `force`, the
   * machine's entry, its loop, the clause — several frames a level where
   * the old runner took one, and 64 cold levels overflowed a 128 KB
   * thread before the first read (TestContStackNative). */
  val firstRoom: Int =
    val fromProperty = System.getProperty("okay.cont.room")
    if fromProperty != null then fromProperty.toInt else 16

  /** cold levels in three quarters of the 1 GB stack, the rest for what a thread starts with */
  private val bigStack = 1L << 30
  private val bigRoom = (bigStack / 4 * 3 / coldBytesPerLevel).toInt

  /** at the end of a room, levels this stack still takes: none is known here (nothing reads the stack) */
  def more(): Int = 0

  /** switches made, for the tests */
  val switches: AtomicLong = AtomicLong()

  /** the rest on a PARKED worker's 1 GB stack (`StackPool`: reused, so
   * its thread is started and its pages warm — a new thread each time
   * was statePara's 4.9x on the count road) */
  def fresh[R](body: Int => R): R =
    switches.incrementAndGet()
    StackPool.run(bigStack)(() => body(bigRoom))
