package okay2

import java.util.concurrent.atomic.AtomicLong
import scala.scalanative.unsafe._

/**
 * The Scala 3 core's Native `StackSwitch` in Scala 2: the room is
 * EXACT, from the runtime's own `ThreadInfo` (`stackBottom` is the
 * HIGHEST address, `stackTop` the lowest — the runtime's naming, upside
 * down against its header comment), and the address of a `stackalloc`
 * is the pointer; the switch is a parked worker with a 1 GB stack
 * (`StackPool`). The struct is spelled out as Scala Native 0.5.12 lays
 * it out; `TestContStackNative` guards the layout.
 */
private[okay2] object StackSwitch {

  private type ThreadInfo = CStruct6[CSize, CSize, Ptr[Byte], Ptr[Byte], Ptr[Byte], CBool]

  @extern
  private object rt {
    def scalanative_currentThreadInfo(): Ptr[ThreadInfo] = extern
  }

  val coldBytesPerLevel: Long = 1200L
  val margin: Long = 64L * 1024

  /** small: a read here is a TLS access, so the exact road starts at once */
  val firstRoom: Int = {
    val p = System.getProperty("okay.cont.room")
    if (p != null) p.toInt else 64
  }

  private val bigStack = 1L << 30
  private val bigRoom = (bigStack / 2048).toInt

  val switches: AtomicLong = new AtomicLong()

  private def sp(): Long = stackalloc[Byte]().toLong

  /** (top, floor, sp), for the layout guard */
  def probe(): (Long, Long, Long) = {
    val ti = rt.scalanative_currentThreadInfo()
    if (ti == null) (-1L, -1L, -1L) else (ti._4.toLong, ti._5.toLong, sp())
  }

  def more(g: ContImpl.Gauge): Int = {
    val ti = rt.scalanative_currentThreadInfo()
    if (ti == null) 0
    else {
      val top = ti._4.toLong
      val floor = ti._5.toLong
      val here = sp()
      if (top <= 0 || floor <= 0 || here <= floor) 0
      else {
        if (g.top == top && g.granted > 0 && g.mark > here) {
          val per = (g.mark - here) / g.granted
          if (per > g.worst) g.worst = per
        }
        val avail = here - floor - margin
        val grant = if (avail <= 0) 0 else (math.min(avail, margin) / g.worst).toInt
        g.top = top
        g.mark = here
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
