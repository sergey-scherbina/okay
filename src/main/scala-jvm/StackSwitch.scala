package okay

import java.util.concurrent.atomic.AtomicLong

/**
 * A fresh stack for a deep direct-style Cont program (specs/cont-stack.md Layer 2): the runner counts
 * levels per stack and, at zero, runs the rest on a parked worker's 1 GB platform thread. The first room is
 * the VM's default thread stack over a cold level, halved; a smaller explicit stack sets `-Dokay.cont.room`.
 */
private[okay] object StackSwitch:

  /** bytes one level takes in a cold JVM (interpreted frames; measured
   * ~1.2 KB): what the first room is derived from */
  val coldBytesPerLevel: Long = 1200L

  /** the VM's default thread stack, in bytes, or 1 MB when the VM
   * cannot be asked (not HotSpot, module not readable) */
  val defaultStackBytes: Long =
    try
      val bean = java.lang.management.ManagementFactory.getPlatformMXBean(classOf[com.sun.management.HotSpotDiagnosticMXBean])
      bean.getVMOption("ThreadStackSize").getValue.toLong * 1024
    catch case _: Throwable => 1L << 20

  /** levels the caller's stack is asked to hold before the first look;
   * `-Dokay.cont.room=N` overrides */
  val firstRoom: Int = Integer.getInteger("okay.cont.room", math.max(64L, defaultStackBytes / coldBytesPerLevel / 2).toInt)

  /** a platform thread's stack past the switch, and the levels it is
   * counted for, at a generous 2 KB each */
  private val bigStack = 1L << 30
  private val bigRoom = (bigStack / 2048).toInt

  /** switches made, for the tests: a program the stack could hold must
   * make none */
  val switches: AtomicLong = AtomicLong()

  /** the rest on a PARKED worker's 1 GB stack (`StackPool`: reused, so
   * its thread is started and its pages warm — a new thread each time
   * was statePara's 4.9x on the count road) */
  def fresh[R](body: Int => R): R =
    switches.incrementAndGet()
    StackPool.run(bigStack)(() => body(bigRoom))
