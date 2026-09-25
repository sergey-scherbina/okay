package okay

import java.util.concurrent.atomic.AtomicLong

/**
 * A FRESH STACK for the rest of a direct-style Cont program, and how
 * much room the current one has (specs/cont-stack.md Layers 2 and 3),
 * on the JVM.
 *
 * `Cont`'s runner counts, in a field of the continuation it hands a
 * shift's body, how many more nested levels the current stack takes.
 * At zero it asks `more`: the stack pointer and the thread's bounds,
 * when `StackRoom` can read them (the JDK 22+ variant, with native
 * access), decide a GRANT — more levels on this stack — and only a
 * stack with nothing left calls `fresh`, which runs the rest on a new
 * thread and waits for the answer. No exception, no replay: the
 * waiting frames, the bodies' own included, stay where they are.
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
 * the first look: the VM's default thread stack (`ThreadStackSize`,
 * 2 MB on macOS arm64, 1 MB on Linux x64) over the cold constant,
 * halved for the caller's own frames. It holds for every thread of the
 * default size, the launcher's main thread included. The WRITTEN
 * BOUND: a thread made with an explicit SMALLER stack must set
 * `-Dokay.cont.room`; the count cannot see its size, and the exact
 * reader only looks at exhaustion.
 */
private[okay] object StackSwitch:

  /** bytes one level takes in a cold JVM (interpreted frames; measured
   * ~1.2 KB), the estimate every grant starts from — a measured
   * `Gauge.worst` only ever raises it */
  val coldBytesPerLevel: Long = 1200L

  /** left below the pointer that no grant reaches: HotSpot's yellow
   * zone plus the fattest frame a body is expected to have. A single
   * opaque frame over this is the layer's other written bound. */
  val margin: Long = 64L * 1024

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
   * counted for at a generous 2 KB each when `more` cannot read it */
  private val bigStack = 1L << 30
  private val bigRoom = (bigStack / 2048).toInt

  /** switches made, for the tests: a program the stack could hold must
   * make none */
  val switches: AtomicLong = AtomicLong()

  /**
   * At exhaustion: how many more levels THIS stack takes, or 0 to
   * switch. Exact where `StackRoom` reads — the grant is the bytes
   * left over the most bytes a level has taken in this run
   * (`Gauge.worst`, measured from the pointer's fall since the last
   * grant, cold constant before that) — and 0 where it cannot: the
   * count road switches at the first room, as the spec's matrix says.
   */
  def more(g: Cont.Gauge): Int =
    val sp = StackRoom.sp()
    if sp < 0 then 0
    else
      val top = StackRoom.top()
      val floor = StackRoom.floor()
      if top <= 0 || floor <= 0 || sp <= floor then 0
      else
        if g.top == top && g.granted > 0 && g.mark > sp then
          val per = (g.mark - sp) / g.granted
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
        val avail = sp - floor - margin
        val grant = if avail <= 0 then 0 else (math.min(avail, margin) / g.worst).toInt
        g.top = top
        g.mark = sp
        g.granted = grant
        grant

  def fresh[R](body: Int => R): R =
    switches.incrementAndGet()
    var out: Either[Throwable, R] | Null = null
    val th = new Thread(null, () => out = try Right(body(bigRoom)) catch case e: Throwable => Left(e), "okay-cont-stack", bigStack)
    th.start()
    th.join()
    out match
      case Right(r) => r
      case Left(e) => throw e
      case null => throw IllegalStateException("okay: a Cont stack switch finished without an answer")
