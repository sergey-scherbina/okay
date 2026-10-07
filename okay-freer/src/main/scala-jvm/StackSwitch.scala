package okay.freer

import okay.*

import java.util.concurrent.atomic.AtomicLong

/**
 * A fresh stack for a deep direct-style Cont program (specs/cont-stack.md Layer 2): the runner counts
 * levels per stack and, at zero, runs the rest on a parked worker's 1 GB platform thread. The first room is
 * the VM's default thread stack over a cold level, halved; a smaller explicit stack sets `-Dokay.cont.room`.
 */
private[okay] object StackSwitch:
  /** a fresh stack is this platform's answer to a deep strict `k` (`Cont.Mode.Auto`); re-execution only when asked
   * for (`Cont.Mode.Replay`, `-Dokay.cont.mode=replay`; cont-safe-mode) */
  val replayByDefault: Boolean = false

  /** bytes one level takes in a cold JVM, what every room is derived from. A strict `k` on `Delimited` is a
   * nested run, several frames a level: interpreted, 367 levels fit 1 MB, 784 fit 2 MB and 1 617 fit 4 MB,
   * ~2 520 B a level over ~124 KB fixed (macOS arm64, JDK 26; a default JVM's first, cold run is the same).
   * 1 200 B, measured on the λ$ runner, let the first room overflow a 1 MB stack and the 1 GB room overflow
   * at ~426 000 levels (cont-stack-cold-bytes-per-level, TestColdRoom) */
  val coldBytesPerLevel: Long = 2600L

  /** the VM's default thread stack, in bytes, or 1 MB when the VM
   * cannot be asked (not HotSpot, module not readable) */
  val defaultStackBytes: Long =
    try
      val bean = java.lang.management.ManagementFactory.getPlatformMXBean(classOf[com.sun.management.HotSpotDiagnosticMXBean])
      bean.getVMOption("ThreadStackSize").getValue.toLong * 1024
    catch case _: Throwable => 1L << 20

  /** left below the pointer that no grant reaches: room for the fattest frame a body is expected to have,
   * over the guard zones `StackRoom.floor` already excludes */
  val margin: Long = 64L * 1024

  /** a grant smaller than this switches instead: a few levels a read is all read and no work */
  private val minGrant = 16

  /** the first room where the stack is READ: small, since every room after it is read (32 cold levels,
   * ~83 KB), so a thread of any size past that holds it */
  private val readFirstRoom = 32

  /** levels the caller's stack is asked to hold before the first look; `-Dokay.cont.room=N` overrides.
   * Where `StackRoom` reads (JDK 22+ with native access), a small first room and then `more`; where it
   * cannot, a guess from the VM's default thread size, halved for the caller's own frames */
  /** whether the end of a room reads the stack: where `StackRoom` can, unless `-Dokay.cont.read=false` */
  val reads: Boolean = StackRoom.readable && System.getProperty("okay.cont.read") != "false"

  val firstRoom: Int = Integer.getInteger("okay.cont.room",
    if reads then readFirstRoom else math.max(64L, defaultStackBytes / coldBytesPerLevel / 2).toInt)

  /**
   * THE HOST STACK, KNOWN EXACTLY WHERE IT CAN BE (operator, 2026-10-04; specs/cont-stack.md): at the end of
   * a room, the levels this stack still takes, or 0 to switch. Where `StackRoom` reads, half of what is left
   * between the pointer and the floor (the guard zones excluded) over `margin`, at a cold level's size: a
   * level up to twice that still fits, and the next room is read again. Where it cannot read, 0 — the count
   * road switches at the end of its room.
   */
  def more(): Int =
    val sp = if reads then StackRoom.sp() else -1L
    if sp < 0 then 0
    else
      val floor = StackRoom.floor()
      if floor <= 0 then 0
      else
        val free = sp - floor - margin
        val grant = if free <= 0 then 0L else free / 2 / coldBytesPerLevel
        if grant < minGrant then 0 else math.min(grant, Int.MaxValue.toLong).toInt

  /** a platform thread's stack past the switch, and the levels it is counted for: cold levels in three
   * quarters of it, the rest for what a thread starts with */
  private val bigStack = 1L << 30
  private val bigRoom = (bigStack / 4 * 3 / coldBytesPerLevel).toInt

  /** switches made, for the tests: a program the stack could hold must
   * make none */
  val switches: AtomicLong = AtomicLong()

  /** the rest on a PARKED worker's 1 GB stack (`StackPool`: reused, so
   * its thread is started and its pages warm — a new thread each time
   * was statePara's 4.9x on the count road) */
  def fresh[R](body: Int => R): R =
    switches.incrementAndGet()
    StackPool.run(bigStack)(() => body(if reads then readFirstRoom else bigRoom))

  /** levels one stack is given in all, read or counted: past it the rest goes to a fresh stack even where the
   * stack reads room left. A GC scans one thread's stack with one worker, so a million levels on ONE stack
   * read 2.2x the time of the same levels over four (cont-stack-exact-first, GC 47 ms a collection against 14) */
  val levelsPerStack: Int = bigRoom
