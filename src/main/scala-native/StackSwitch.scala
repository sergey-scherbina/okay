package okay

import java.util.concurrent.atomic.AtomicLong
import scala.scalanative.unsafe.*

/**
 * A FRESH STACK for the rest of a direct-style Cont program, and how
 * much room the current one has (specs/cont-stack.md Layers 2 and 3),
 * on Scala Native — where the room is EXACT, always, and in the core.
 *
 * The runtime keeps a `ThreadInfo` per thread for its own
 * StackOverflowError (nativelib `nativeThreadTLS.h`,
 * `stackOverflowGuards.c`): the stack's top and bottom, its guard
 * page, its maximum size, the main thread's from the OS soft limit.
 * `scalanative_currentThreadInfo()` hands it out, and the address of a
 * `stackalloc` is the stack pointer — no pthread call, no per-OS
 * layout. The struct is spelled out here as Scala Native 0.5.12 lays
 * it out (`stackSize`, `maxStackSize`, `stackTop`, `stackBottom`,
 * `stackGuardPage`, `isMainThread`, …). NAMES ARE THE RUNTIME'S, AND
 * THEY ARE UPSIDE DOWN against the header's own comment: the code
 * asserts `stackBottom > stackTop` (`nativeThreadTLS.c`,
 * `setupCurrentThreadInfo`) — `stackTop` is the LOWEST address, where
 * the stack grows to, `stackBottom` the highest, and the guard page
 * sits at the low end. Measured 2026-09-25 before this was read: the
 * first cut took `stackTop` for the highest address and the layout
 * test put the pointer 1 MB above it. `TestContStackNative` checks
 * that a `stackalloc` address lies inside the bounds, which is what
 * guards the layout against a runtime bump.
 *
 * The fresh stack is a platform thread with a 1 GB stack: the javalib
 * passes the size to `pthread_attr_setstacksize` (page-aligned, plus
 * its guard pages), and pages are committed only as touched.
 */
private[okay] object StackSwitch:

  /** `ThreadInfo` as nativelib 0.5.12 lays it out; only the first six
   * fields are read */
  private type ThreadInfo = CStruct6[CSize, CSize, Ptr[Byte], Ptr[Byte], Ptr[Byte], CBool]

  @extern
  private object rt:
    def scalanative_currentThreadInfo(): Ptr[ThreadInfo] = extern

  /** bytes one level takes cold — the JVM's measured constant, kept
   * for one runner on every platform; the exact read corrects it at
   * the first exhaustion */
  val coldBytesPerLevel: Long = 1200L

  /** left above the guard page that no grant reaches */
  val margin: Long = 64L * 1024

  /** levels the caller's stack is asked to hold before the first look.
   * SMALL here, unlike the JVM's: a read on Native is a TLS access and
   * a `stackalloc`, a few ns, so the exact road can start at once —
   * and a first room derived from the thread this object initialises
   * on (the main thread's 8 MB) is wrong for every other thread. The
   * JVM cannot afford that, its read is a syscall. `okay.cont.room`
   * overrides. */
  val firstRoom: Int =
    val fromProperty = System.getProperty("okay.cont.room")
    if fromProperty != null then fromProperty.toInt else 64

  private val bigStack = 1L << 30
  private val bigRoom = (bigStack / 2048).toInt

  /** switches made, for the tests */
  val switches: AtomicLong = AtomicLong()

  /** the stack pointer: where this frame's own allocation went */
  private def sp(): Long = stackalloc[Byte]().toLong

  /** (top, floor, sp) of the current thread, for the layout guard in
   * `TestContStackNative`: a `stackalloc` address must lie between the
   * bounds the runtime reports, or the struct above is not the
   * runtime's */
  def probe(): (Long, Long, Long) =
    val ti = rt.scalanative_currentThreadInfo()
    if ti == null then (-1L, -1L, -1L) else (ti._4.toLong, ti._5.toLong, sp())

  /** at exhaustion: how many more levels THIS stack takes, or 0 to
   * switch — the same arithmetic as the JVM's, over the runtime's own
   * bounds */
  def more(g: Cont.Gauge): Int =
    val ti = rt.scalanative_currentThreadInfo()
    if ti == null then 0
    else
      val top = ti._4.toLong // `stackBottom`: the highest address (see above)
      val floor = ti._5.toLong // the guard page, at the low end: no frame reaches it
      val here = sp()
      if top <= 0 || floor <= 0 || here <= floor then 0
      else
        if g.top == top && g.granted > 0 && g.mark > here then
          val per = (g.mark - here) / g.granted
          if per > g.worst then g.worst = per
        // ONE SLICE OF `margin` BYTES A GRANT, never the whole room: the
        // estimate behind a grant is the worst level seen so far, and a
        // fatter body below it overshoots. A slice of `margin` at that
        // estimate leaves the margin itself to absorb up to a 2x
        // overshoot before the floor, and the next exhaustion measures
        // the fatter level and raises `worst`. Measured before this
        // rule: a first grant of the whole 2 MB at the cold constant
        // overflowed on 20 000 cold levels. Cost: a read per 64 KB of
        // stack, ~200 warm levels, ~1.6 ns a level.
        val avail = here - floor - margin
        val grant = if avail <= 0 then 0 else (math.min(avail, margin) / g.worst).toInt
        g.top = top
        g.mark = here
        g.granted = grant
        grant

  /** the rest on a PARKED worker's 1 GB stack (`StackPool`: reused, so
   * its thread is started and its pages warm — a new thread each time
   * was statePara's 4.9x on the count road) */
  def fresh[R](body: Int => R): R =
    switches.incrementAndGet()
    StackPool.run(bigStack)(() => body(bigRoom))
