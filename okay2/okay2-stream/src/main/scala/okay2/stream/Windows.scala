package okay2.stream

import scala.collection.mutable
import okay2._

/** one closed window: everything that fell in `[start, end)` under one key, aggregated */
final case class Pane[+K, +O](start: Long, end: Long, key: K, value: O)

/**
 * EVENT-TIME WINDOWS: keyed panes over an `Aggregator`, closed by a
 * watermark. Windowing here is a function of the data's own time —
 * `at(a)` — and of nothing else, so a run is reproducible and no clock
 * is read. The watermark is bounded out-of-orderness: the greatest
 * event time seen, minus `lateness`, and it never goes back. A window
 * is emitted when the watermark reaches its end; an element that
 * arrives after every window it belongs to has closed is DROPPED and
 * counted.
 */
final class Windows[K, A, Acc, O](size: Long, slide: Long, lateness: Long,
                                  key: A => K, at: A => Long,
                                  agg: Aggregator[A, Acc, O]) {
  require(size > 0 && slide > 0, "a window has a positive size and slide")
  require(size % slide == 0, "the size must be a whole number of slides")

  private val panes = mutable.HashMap.empty[K, mutable.LongMap[Acc]]
  private val panesPer = (size / slide).toInt
  private val closing = mutable.ArrayBuffer.empty[Long]
  private val emptied = mutable.ArrayBuffer.empty[K]
  private var maxSeen = Long.MinValue
  private var swept = Long.MinValue
  private var late = 0L

  /** start from a watermark another operator already reached — only before the first element */
  def seed(maxEventTime: Long): Unit = {
    require(maxSeen == Long.MinValue, "a window is seeded before its first element")
    maxSeen = maxEventTime
  }

  /** the greatest event time seen minus `lateness`; monotone */
  def watermark: Long = if (maxSeen == Long.MinValue) Long.MinValue else maxSeen - lateness

  /** elements that arrived after every window they belong to had closed */
  def dropped: Long = late

  /** how many panes are open */
  def live: Int = {
    var n = 0
    for ((_, byWindow) <- panes) n += byWindow.size
    n
  }

  /** fold one element into every window it belongs to, then emit the windows its arrival closed */
  def add(a: A)(emit: Pane[K, O] => Unit): Unit = {
    val ts = at(a)
    if (ts > maxSeen) maxSeen = ts
    val mark = watermark
    var start = firstStart(ts)
    var entered = 0
    var byWindow: mutable.LongMap[Acc] = null
    var i = 0
    while (i < panesPer) {
      if (start + size > mark) {
        if (byWindow == null) byWindow = panes.getOrElseUpdate(key(a), mutable.LongMap.empty[Acc])
        val id = start / slide
        byWindow.update(id, agg.add(byWindow.getOrElse(id, agg.init), a))
        entered += 1
      }
      start += slide
      i += 1
    }
    if (entered == 0) late += 1
    advance(mark)(emit)
  }

  /** the end of the input closes everything still open */
  def close()(emit: Pane[K, O] => Unit): Unit = {
    sweep(Long.MaxValue)(emit)
    swept = Long.MaxValue
  }

  private def firstStart(ts: Long): Long = ts - Math.floorMod(ts, slide) - size + slide

  private def advance(mark: Long)(emit: Pane[K, O] => Unit): Unit =
    if (mark >= swept) {
      sweep(mark)(emit)
      swept = mark - Math.floorMod(mark, slide) + slide
    }

  private def sweep(mark: Long)(emit: Pane[K, O] => Unit): Unit = {
    for ((k, byWindow) <- panes) {
      byWindow.foreachKey(id => if (id * slide + size <= mark) closing += id)
      var i = 0
      while (i < closing.length) {
        val id = closing(i)
        byWindow.remove(id).foreach(acc => emit(Pane(id * slide, id * slide + size, k, agg.present(acc))))
        i += 1
      }
      closing.clear()
      if (byWindow.isEmpty) emptied += k
    }
    var j = 0
    while (j < emptied.length) { panes.remove(emptied(j)); j += 1 }
    emptied.clear()
  }
}

object Windows {

  /** windows that follow one another: every element in exactly one */
  def tumbling[K, A, Acc, O](size: Long, lateness: Long)(key: A => K)(at: A => Long)(agg: Aggregator[A, Acc, O]): Windows[K, A, Acc, O] =
    new Windows(size, size, lateness, key, at, agg)

  /** windows that overlap: every element in `size / slide` of them */
  def sliding[K, A, Acc, O](size: Long, slide: Long, lateness: Long)(key: A => K)(at: A => Long)(agg: Aggregator[A, Acc, O]): Windows[K, A, Acc, O] =
    new Windows(size, slide, lateness, key, at, agg)

  /** the same operator as a pipeline stage: it awaits elements and
   * tells closed panes. It takes the PARAMETERS rather than a
   * `Windows`: a Stage is a VALUE, and driving the same value twice
   * must not share one pane map between the runs — the state is
   * allocated on the first element */
  def stage[K, A, Acc, O](size: Long, slide: Long, lateness: Long)(key: A => K)(at: A => Long)(agg: Aggregator[A, Acc, O]): Stage[A, Pane[K, O], Unit] = {
    type Out = Pane[K, O]
    def tellAll(buf: mutable.ArrayBuffer[Out], i: Int): Stage[A, Out, Unit] =
      if (i >= buf.length) { buf.clear(); pure(()) }
      else Stage.tell[A, Out](buf(i)).flatMap(_ => tellAll(buf, i + 1))

    def go(w: Windows[K, A, Acc, O], buf: mutable.ArrayBuffer[Out]): Stage[A, Out, Unit] =
      Stage.await[A, Out].flatMap {
        case Some(a) =>
          val win = if (w == null) new Windows(size, slide, lateness, key, at, agg) else w
          val out = if (buf == null) mutable.ArrayBuffer.empty[Out] else buf
          win.add(a)(p => { out += p; () })
          tellAll(out, 0).flatMap(_ => go(win, out))
        case None =>
          if (w == null) pure(())
          else {
            val out = if (buf == null) mutable.ArrayBuffer.empty[Out] else buf
            w.close()(p => { out += p; () })
            tellAll(out, 0)
          }
      }

    go(null, null)
  }
}
