package okay2

import java.util.concurrent.atomic.AtomicLong

/**
 * A FRESH STACK for the rest of a direct-style Cont program, and how
 * much room the current one has (specs/cont-stack.md Layers 2 and 3)
 * — the Scala 3 core's `StackSwitch` in Scala 2, on the JVM. The
 * runner counts the levels the current stack takes in a field of the
 * continuation it hands a shift's body; at zero `more` asks `StackRoom`
 * (−1 here: the count road, see StackRoom) and `fresh` hands the rest to
 * a parked worker with a 1 GB stack (`StackPool`) and waits. No
 * exception unwinds, nothing runs twice.
 */
private[okay2] object StackSwitch {

  /** bytes one level takes in a cold JVM. NOT the Scala 3 core's
   * 1.2 KB: a Scala 2 level is 11 frames (`$adapted` wrappers, `at`,
   * two `Function1` specialisation bridges) against 8, and with the
   * first room derived from 1.2 KB — 873 levels on a 2 MB thread —
   * TestContStack overflowed BEFORE the first look, 2026-09-25: 873 cold
   * levels plus HotSpot's 384 KB guard-and-shadow zone are more than
   * 2 MB — and so did 436 levels at 2.4 KB, on the FIRST test of a
   * fresh JVM: a fully cold Scala 2 level is ~4 KB (a probe that
   * bisected the levels a 2 MB thread holds read 2 473 — 689 B a
   * level — but only after its own first, coldest, attempts had
   * warmed the runner; the coldest attempt is the one that counts).
   * 4.8 KB gives ~218 levels on a 2 MB thread, 870 KB cold, which
   * fits with the zone and the caller's frames; past them the rest
   * runs on the pool's 1 GB worker at full speed. */
  val coldBytesPerLevel: Long = 4800L

  /** left below the pointer that no grant reaches */
  val margin: Long = 64L * 1024

  /** the VM's default thread stack, in bytes, or 1 MB when unknown */
  val defaultStackBytes: Long =
    try {
      val bean = java.lang.management.ManagementFactory.getPlatformMXBean(classOf[com.sun.management.HotSpotDiagnosticMXBean])
      bean.getVMOption("ThreadStackSize").getValue.toLong * 1024
    } catch { case _: Throwable => 1L << 20 }

  /** levels the caller's stack is asked to hold before the first look;
   * `-Dokay.cont.room=N` overrides (the same property as the Scala 3
   * core's) */
  val firstRoom: Int = Integer.getInteger("okay.cont.room", math.max(64L, defaultStackBytes / coldBytesPerLevel / 2).toInt)

  private val bigStack = 1L << 30
  private val bigRoom = (bigStack / 2048).toInt

  /** switches made, for the tests */
  val switches: AtomicLong = new AtomicLong()

  /** at exhaustion: more levels for THIS stack, or 0 to switch — one
   * margin-sized slice at the worst bytes a level seen, where the stack
   * can be read; 0 where it cannot */
  def more(g: ContImpl.Gauge): Int = {
    val sp = StackRoom.sp()
    if (sp < 0) 0
    else {
      val top = StackRoom.top()
      val floor = StackRoom.floor()
      if (top <= 0 || floor <= 0 || sp <= floor) 0
      else {
        if (g.top == top && g.granted > 0 && g.mark > sp) {
          val per = (g.mark - sp) / g.granted
          if (per > g.worst) g.worst = per
        }
        val avail = sp - floor - margin
        val grant = if (avail <= 0) 0 else (math.min(avail, margin) / g.worst).toInt
        g.top = top
        g.mark = sp
        g.granted = grant
        grant
      }
    }
  }

  def fresh[R](body: Int => R): R = {
    switches.incrementAndGet()
    StackPool.run(bigStack)(() => body(bigRoom))
  }
}
