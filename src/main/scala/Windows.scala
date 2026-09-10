package okay

import scala.collection.mutable

/**
 * One closed window: everything that fell in `[start, end)` under one
 * key, aggregated.
 */
final case class Pane[+K, +O](start: Long, end: Long, key: K, value: O)

/**
 * EVENT-TIME WINDOWS (specs/event-time-windows.md): keyed panes over
 * an `Aggregator`, closed by a watermark, in the core and on every
 * platform.
 *
 * WHY IT IS HERE. specs/aggregators.md has the arithmetic and a
 * sliding window by ELEMENT COUNT over a `Group`; a stream job asks
 * for neither. docs/benchmarks.md §20 ran the same event-time job on
 * okay and on Apache Flink, and the honest half of that comparison was
 * not the ratio: Flink's stage is one line
 * (`.window(TumblingEventTimeWindows.of(…))`) and okay's was fifty,
 * written inside the benchmark because the core had nowhere to put
 * them. This is those fifty lines, generalised over the key and given
 * the late-event rule in ONE place instead of once per user.
 *
 * WHAT IT DOES NOT DO: read a clock. Windowing here is a function of
 * the data's own time — `at(a)` — and of nothing else, so a run is
 * reproducible and `Async` never appears. That is also what let §20
 * assert two engines EQUAL rather than close.
 *
 * The watermark is bounded out-of-orderness: the greatest event time
 * seen, minus `lateness`, and it never goes back. A window is emitted
 * when the watermark reaches its end; an element that arrives after
 * every window it belongs to has closed is DROPPED and counted, never
 * folded into a window that has already been reported.
 *
 * @param size     the window's width, in the same unit as `at`
 * @param slide    how far one window starts after the previous (= size
 *                 for tumbling windows; smaller for sliding, and then
 *                 each element enters `size / slide` panes)
 * @param lateness how far out of order elements may arrive
 * @param key      the pane's key — one aggregate per key per window
 * @param at       the element's EVENT time
 * @param agg      what a window computes: the same `Aggregator` that
 *                 folds a Chunk locally and an `AggregateFunction` on
 *                 a Flink cluster (okay-flink's `toFlink`)
 */
final class Windows[K, A, Acc, O](size: Long, slide: Long, lateness: Long,
                                  key: A => K, at: A => Long,
                                  agg: Aggregator[A, Acc, O]) {
  require(size > 0 && slide > 0, "a window has a positive size and slide")
  require(size % slide == 0, "the size must be a whole number of slides")

  /** panes: one hash by key, then the window index. No tuple is
   * allocated on the hot path and the key need not be dense — §20's
   * hand-written operator packed both into one Long, which is faster
   * and only possible for small integer keys */
  private val panes = mutable.HashMap.empty[K, mutable.LongMap[Acc]]
  private val panesPer = (size / slide).toInt
  private val closing = mutable.ArrayBuffer.empty[Long]
  private val emptied = mutable.ArrayBuffer.empty[K]
  private var maxSeen = Long.MinValue
  private var swept = Long.MinValue
  private var late = 0L

  /** the greatest event time seen minus `lateness`; monotone */
  def watermark: Long = if maxSeen == Long.MinValue then Long.MinValue else maxSeen - lateness

  /** elements that arrived after every window they belong to had closed */
  def dropped: Long = late

  /** how many panes are open — the operator's live state */
  def live: Int =
    var n = 0
    for (_, byWindow) <- panes do n += byWindow.size
    n

  /**
   * Fold one element into every window it belongs to, then emit the
   * windows that its arrival closed.
   */
  def add(a: A)(emit: Pane[K, O] => Unit): Unit =
    val ts = at(a)
    if ts > maxSeen then maxSeen = ts
    val mark = watermark
    var start = firstStart(ts)
    var entered = 0
    var byWindow: mutable.LongMap[Acc] | Null = null // looked up once, and only if a window is open
    var i = 0
    while i < panesPer do
      // a window whose end the watermark has already passed is closed:
      // it has been reported, and nothing may be added to it
      if start + size > mark then
        if byWindow == null then byWindow = panes.getOrElseUpdate(key(a), mutable.LongMap.empty[Acc])
        val m = byWindow.nn
        val id = start / slide
        m.update(id, agg.add(m.getOrElse(id, agg.init), a))
        entered += 1
      start += slide
      i += 1
    if entered == 0 then late += 1
    advance(mark)(emit)

  /** the end of the input closes everything still open */
  def close()(emit: Pane[K, O] => Unit): Unit =
    sweep(Long.MaxValue)(emit)
    swept = Long.MaxValue

  /** the first of the windows a timestamp falls in */
  private def firstStart(ts: Long): Long =
    ts - Math.floorMod(ts, slide) - size + slide

  /** windows end on multiples of the slide, so between two boundaries
   * nothing can close and a sweep would scan for nothing */
  private def advance(mark: Long)(emit: Pane[K, O] => Unit): Unit =
    if mark >= swept then
      sweep(mark)(emit)
      swept = mark - Math.floorMod(mark, slide) + slide

  private def sweep(mark: Long)(emit: Pane[K, O] => Unit): Unit =
    for (k, byWindow) <- panes do
      byWindow.foreachKey(id => if id * slide + size <= mark then closing += id)
      var i = 0
      while i < closing.length do
        val id = closing(i)
        byWindow.remove(id).foreach(acc =>
          emit(Pane(id * slide, id * slide + size, k, agg.present(acc))))
        i += 1
      closing.clear()
      if byWindow.isEmpty then emptied += k
    // a key that never returns must not keep an empty map for ever
    var j = 0
    while j < emptied.length do { panes.remove(emptied(j)): Unit; j += 1 }
    emptied.clear()
}

object Windows {

  /** windows that follow one another: every element in exactly one */
  def tumbling[K, A, Acc, O](size: Long, lateness: Long)
                            (key: A => K)(at: A => Long)
                            (agg: Aggregator[A, Acc, O]): Windows[K, A, Acc, O] =
    new Windows(size, size, lateness, key, at, agg)

  /** windows that overlap: every element in `size / slide` of them */
  def sliding[K, A, Acc, O](size: Long, slide: Long, lateness: Long)
                           (key: A => K)(at: A => Long)
                           (agg: Aggregator[A, Acc, O]): Windows[K, A, Acc, O] =
    new Windows(size, slide, lateness, key, at, agg)

  /**
   * The same operator as a pipeline stage: it awaits elements and
   * tells closed panes, so a window composes with `through` like any
   * other stage (specs/stage-pipeline.md).
   *
   * It takes the PARAMETERS rather than a `Windows`, and that is not
   * an accident: a Stage is a VALUE, and driving the same value twice
   * must not share one pane map between the runs. The state is
   * allocated on the first element, exactly as `Stage.chunked`
   * allocates its buffer (chunk-stack-safety, 2026-09-03).
   */
  def stage[K, A, Acc, O](size: Long, slide: Long, lateness: Long)
                         (key: A => K)(at: A => Long)
                         (agg: Aggregator[A, Acc, O]): Stage[A, Pane[K, O], Unit] =
    type Out = Pane[K, O]
    def tellAll(buf: mutable.ArrayBuffer[Out], i: Int): Stage[A, Out, Unit] =
      if i >= buf.length then { buf.clear(); pure(()) }
      else Stage.tell[A, Out](buf(i)).flatMap(_ => tellAll(buf, i + 1))

    def go(w: Windows[K, A, Acc, O] | Null, buf: mutable.ArrayBuffer[Out] | Null)
    : Stage[A, Out, Unit] =
      Stage.await[A, Out].flatMap {
        case Some(a) =>
          val win = if w == null then new Windows(size, slide, lateness, key, at, agg) else w.nn
          val out = if buf == null then mutable.ArrayBuffer.empty[Out] else buf.nn
          win.add(a)(p => { out += p; () })
          tellAll(out, 0).flatMap(_ => go(win, out))
        case None =>
          if w == null then pure(())
          else
            val out = if buf == null then mutable.ArrayBuffer.empty[Out] else buf.nn
            w.nn.close()(p => { out += p; () })
            tellAll(out, 0)
      }

    go(null, null)
}
