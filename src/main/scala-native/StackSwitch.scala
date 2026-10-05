package okay

import java.util.concurrent.atomic.AtomicLong
import scala.scalanative.unsafe.*

/**
 * A fresh stack for a deep direct-style Cont program on Scala Native, and how much room the current one has:
 * READ, always (specs/cont-stack.md, road 3; operator, 2026-10-04: the host stack only where its bound is known).
 *
 * The runtime keeps a `ThreadInfo` per thread for its own StackOverflowError (nativelib `nativeThreadTLS.h`,
 * `stackOverflowGuards.c`): the stack's bounds and its guard page. `scalanative_currentThreadInfo()` hands it
 * out, and the address of a `stackalloc` is the stack pointer, so a read is a TLS access and no system call.
 * The struct is spelled out here as Scala Native 0.5.12 lays it out (`stackSize`, `maxStackSize`, `stackTop`,
 * `stackBottom`, `stackGuardPage`, `isMainThread`, …). THE NAMES ARE UPSIDE DOWN against the header's own
 * comment: the runtime asserts `stackBottom > stackTop` (`nativeThreadTLS.c`, `setupCurrentThreadInfo`), so
 * `stackTop` is the LOWEST address and the guard page sits at the low end. `TestContStackNative` checks that
 * a `stackalloc` address lies inside the bounds, which guards the layout against a runtime bump. This reader
 * was taken out by cont-core-design (2026-10-01) and brought back by stack-host-three.
 *
 * The fresh stack is a platform thread with a 1 GB stack: the javalib passes the size to
 * `pthread_attr_setstacksize`, and pages are committed only as touched.
 */
private[okay] object StackSwitch:
  /** a fresh stack is this platform's answer to a deep strict `k`; re-execution (ContReplay, cont-js-depth stage 4)
   * only with -Dokay.cont.replay=true */
  val replayByDefault: Boolean = java.lang.Boolean.getBoolean("okay.cont.replay")

  /** `ThreadInfo` as nativelib 0.5.12 lays it out; only the first six fields are read */
  private type ThreadInfo = CStruct6[CSize, CSize, Ptr[Byte], Ptr[Byte], Ptr[Byte], CBool]

  @extern
  private object rt:
    def scalanative_currentThreadInfo(): Ptr[ThreadInfo] = extern

  /** bytes one level takes cold — the JVM's measured constant (an interpreted level, the largest there is), kept
   * for one runner on every platform: what every grant is sized by */
  val coldBytesPerLevel: Long = 2600L

  /** left above the guard page that no grant reaches */
  val margin: Long = 64L * 1024

  /** a grant smaller than this switches instead */
  private val minGrant = 16

  /** levels the caller's stack is asked to hold before the first read. Small: a read is a few ns here, and the
   * stack it is read from may be any thread's. `okay.cont.room` overrides */
  val firstRoom: Int =
    val fromProperty = System.getProperty("okay.cont.room")
    if fromProperty != null then fromProperty.toInt else 16

  /** cold levels in three quarters of the 1 GB stack, the rest for what a thread starts with */
  private val bigStack = 1L << 30
  private val bigRoom = (bigStack / 4 * 3 / coldBytesPerLevel).toInt

  /** levels one stack is given in all, read or not (the JVM's reason: a GC scans one thread's stack serially) */
  val levelsPerStack: Int = bigRoom

  /** switches made, for the tests */
  val switches: AtomicLong = AtomicLong()

  /** the stack pointer: where this frame's own allocation went */
  private def sp(): Long = stackalloc[Byte]().toLong

  /** (top, floor, sp) of the current thread, for the layout guard in `TestContStackNative`: a `stackalloc`
   * address must lie between the bounds the runtime reports, or the struct above is not the runtime's */
  def probe(): (Long, Long, Long) =
    val ti = rt.scalanative_currentThreadInfo()
    if ti == null then (-1L, -1L, -1L) else (ti._4.toLong, ti._5.toLong, sp())

  /**
   * At the end of a room, the levels this stack still takes, or 0 to switch: half of what is left between the
   * pointer and the guard page over `margin`, at a cold level's size (the JVM's rule, over the runtime's own
   * bounds). A level up to twice that size still fits, and the next room is read again.
   */
  def more(): Int =
    val ti = rt.scalanative_currentThreadInfo()
    if ti == null then 0
    else
      val floor = ti._5.toLong // the guard page, at the low end: no frame reaches it
      val here = sp()
      if floor <= 0 || here <= floor then 0
      else
        val free = here - floor - margin
        val grant = if free <= 0 then 0L else free / 2 / coldBytesPerLevel
        if grant < minGrant then 0 else math.min(grant, Int.MaxValue.toLong).toInt

  /** the rest on a PARKED worker's 1 GB stack (`StackPool`: reused, so its thread is started and its pages
   * warm — a new thread each time was statePara's 4.9x on the count road), from a small first room that is
   * then read */
  def fresh[R](body: Int => R): R =
    switches.incrementAndGet()
    StackPool.run(bigStack)(() => body(firstRoom))
