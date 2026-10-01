package okay

import java.util.concurrent.atomic.AtomicLong

/**
 * A FRESH STACK for the rest of a direct-style Cont program, and the
 * room a stack is counted for (specs/cont-stack.md Layer 2), on the JVM.
 *
 * `Cont`'s runner counts how many more nested levels the current stack
 * takes; at zero it calls `fresh`, which runs the rest on a new thread
 * and waits for the answer. No exception, no replay: the waiting frames,
 * the bodies' own included, stay where they are. The stack is COUNTED,
 * never read (cont-core-design, 2026-10-01): the exact road that read
 * it through `StackRoom` and granted more levels left the runner.
 *
 * The fresh stack is a PLATFORM thread with a 1 GB stack, on every
 * JDK (Decision 8). The first cut used a virtual thread on 21+, and
 * measured on one JDK it costs 15–20x more a level: a waiting virtual
 * thread freezes its frames into a heap chunk that HotSpot refuses
 * when humongous, which caps a segment at 64 levels, and the price is
 * the hop (park, unpark, thaw — 14 µs), paid every 64 levels instead
 * of every ~500 000. A platform thread starts and joins in 33 µs at
 * 1 MB and at 1 GB alike: the reservation costs nothing until touched.
 *
 * THE FIRST ROOM is what the CALLER's stack is asked to hold before
 * the switch: the VM's default thread stack (`ThreadStackSize`,
 * 2 MB on macOS arm64, 1 MB on Linux x64) over the cold constant,
 * halved for the caller's own frames. It holds for every thread of the
 * default size, the launcher's main thread included. The WRITTEN
 * BOUND: a thread made with an explicit SMALLER stack must set
 * `-Dokay.cont.room`; the count cannot see its size.
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
