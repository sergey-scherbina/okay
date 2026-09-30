package okay

import scala.collection.mutable

/**
 * The event-time WINDOWED join by key of two unbounded, unordered
 * streams (specs/stream-join.md, stage 2): a row matches, ON ARRIVAL,
 * every row of the other side with its key whose event time is within
 * `within` of its own; it is then held for the rows still to come, and
 * evicted once the watermark has passed `at + within` — no row arriving
 * after that can be within reach. The symmetric hash join (Wilschut &
 * Apers 1991) with `Windows`' eviction rule; Flink's interval join and
 * Kafka Streams' KStream-KStream join are this shape.
 *
 * The watermark is the MIN of the two sides' (Flink's rule for a
 * two-input operator): a side's is the greatest event time it has
 * shown minus `lateness`, nothing before its first row, everything
 * once it has ENDED. So a side far ahead in event time cannot make the
 * other side's rows late, and the interleaving of two live sides
 * changes nothing about which pairs are told — only a row behind the
 * joint watermark is late, dropped and COUNTED, never joined. The price
 * is that a side that stays silent holds the watermark, and the other
 * side's rows, until it speaks or ends — Flink's idle-source problem,
 * named here rather than solved.
 *
 * A side that ends frees the OTHER side's store: nothing will arrive
 * to match it. Nothing here reads a clock.
 */
final class WindowJoin[K, A, B](within: Long, lateness: Long, atL: A => Long, atR: B => Long) {
  require(within >= 0 && lateness >= 0, "a window's reach and lateness are not negative")

  private final class Held[X](val at: Long, val x: X)
  private val lefts = mutable.HashMap.empty[K, mutable.ArrayBuffer[Held[A]]]
  private val rights = mutable.HashMap.empty[K, mutable.ArrayBuffer[Held[B]]]
  private var maxL = Long.MinValue
  private var maxR = Long.MinValue
  private var endedL = false
  private var endedR = false
  private var late = 0L
  private var swept = Long.MinValue
  private var live = 0

  private def mark(max: Long, ended: Boolean): Long =
    if ended then Long.MaxValue
    else if max == Long.MinValue then Long.MinValue
    else max - lateness

  /** the joint watermark: the smaller of the two sides', monotone */
  def watermark: Long = math.min(mark(maxL, endedL), mark(maxR, endedR))

  /** rows that arrived behind the watermark — dropped, and counted */
  def dropped: Long = late

  /** rows held now, both sides — the operator's live state */
  def held: Int = live

  /** nothing can be produced any more: a side has ended and every row
   * of the other side that could still have matched it is gone */
  def exhausted: Boolean = (endedL && lefts.isEmpty) || (endedR && rights.isEmpty)

  /** the rows of one key that no future row can reach, out */
  private def trim[X](buf: mutable.ArrayBuffer[Held[X]], wm: Long): Unit =
    if wm != Long.MinValue then
      val before = buf.length
      buf.filterInPlace(h => h.at + within >= wm)
      live -= before - buf.length

  private def trimAll[X](store: mutable.HashMap[K, mutable.ArrayBuffer[Held[X]]], wm: Long): Unit =
    store.filterInPlace((_, buf) => { trim(buf, wm); buf.nonEmpty })

  /** a sweep of every key once per `within` of watermark advance: a
   * key that never returns must not keep its rows for ever */
  private def advance(wm: Long): Unit =
    if wm != Long.MinValue && (swept == Long.MinValue || wm - swept >= within || wm == Long.MaxValue) && wm != swept then
      trimAll(lefts, wm); trimAll(rights, wm)
      swept = wm

  private def clear[X](store: mutable.HashMap[K, mutable.ArrayBuffer[Held[X]]]): Unit =
    for (_, buf) <- store do live -= buf.length
    store.clear()

  private def arrive[X, Y](k: K, t: Long, x: X, mine: mutable.HashMap[K, mutable.ArrayBuffer[Held[X]]],
                           theirs: mutable.HashMap[K, mutable.ArrayBuffer[Held[Y]]], theirsEnded: Boolean)
                          (emit: Y => Unit): Unit =
    val wm = watermark
    if t < wm then late += 1
    else
      theirs.get(k) match
        case Some(buf) =>
          trim(buf, wm)
          if buf.isEmpty then theirs.remove(k): Unit
          else
            var i = 0
            while i < buf.length do
              val h = buf(i)
              if math.abs(t - h.at) <= within then emit(h.x)
              i += 1
        case None => ()
      // held for the other side's rows to come — unless none can
      if !theirsEnded then
        val buf = mine.getOrElseUpdate(k, mutable.ArrayBuffer.empty)
        trim(buf, wm)
        buf += new Held(t, x)
        live += 1
    advance(watermark)

  def left(k: K, a: A)(emit: ((K, (A, B))) => Unit): Unit =
    val t = atL(a)
    if t > maxL then maxL = t
    arrive[A, B](k, t, a, lefts, rights, endedR)(b => emit((k, (a, b))))

  def right(k: K, b: B)(emit: ((K, (A, B))) => Unit): Unit =
    val t = atR(b)
    if t > maxR then maxR = t
    arrive[B, A](k, t, b, rights, lefts, endedL)(a => emit((k, (a, b))))

  /** the left side ended: its watermark is everything, and no held
   * right row will ever be reached */
  def leftEnd(): Unit =
    endedL = true
    clear(rights)
    advance(watermark)

  def rightEnd(): Unit =
    endedR = true
    clear(lefts)
    advance(watermark)
}

object WindowJoin {
  /** one arrival of the merged sides: a row, or a side's end */
  type Event[K, A, B] = Either[Option[(K, A)], Option[(K, B)]]

  import !.*

  /**
   * The join as a pipeline stage over the two sides' MERGED events —
   * `Left(Some(row))` / `Right(Some(row))`, `Left(None)` / `Right(None)`
   * for a side's end. Takes the parameters, never an instance: a Stage
   * is a value, and driving it twice must not share one store
   * (`Windows.stage`'s rule).
   */
  def stage[K, A, B](within: Long, lateness: Long)(atL: A => Long, atR: B => Long)
  : Stage[Event[K, A, B], (K, (A, B)), Unit] =
    type In = Event[K, A, B]
    type Out = (K, (A, B))
    def tellAll(buf: mutable.ArrayBuffer[Out], i: Int): Stage[In, Out, Unit] =
      if i >= buf.length then { buf.clear(); pure(()) }
      else Stage.tell[In, Out](buf(i)).flatMap(_ => tellAll(buf, i + 1))

    def go(j: WindowJoin[K, A, B] | Null, buf: mutable.ArrayBuffer[Out] | Null): Stage[In, Out, Unit] =
      Stage.await[In, Out].flatMap {
        case Some(ev) =>
          val join = if j == null then new WindowJoin(within, lateness, atL, atR) else j.nn
          val out = if buf == null then mutable.ArrayBuffer.empty[Out] else buf.nn
          ev match
            case Left(Some((k, a))) => join.left(k, a)(o => { out += o; () })
            case Right(Some((k, b))) => join.right(k, b)(o => { out += o; () })
            case Left(None) => join.leftEnd()
            case Right(None) => join.rightEnd()
          if join.exhausted then tellAll(out, 0)
          else tellAll(out, 0).flatMap(_ => go(join, out))
        case None => pure(())
      }

    go(null, null)
}
