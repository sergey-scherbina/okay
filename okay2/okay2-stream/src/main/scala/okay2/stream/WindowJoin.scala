package okay2.stream

import scala.collection.mutable
import okay2._

/**
 * The event-time WINDOWED join by key of two unbounded, unordered
 * streams — the Scala 2 twin of the core's `WindowJoin`
 * (specs/stream-join.md, stage 2). A row matches, ON ARRIVAL, every row
 * of the other side with its key whose event time is within `within`
 * of its own, is held for the rows still to come, and is evicted once
 * the watermark has passed `at + within`. The watermark is the MIN of
 * the two sides' (a side's: its greatest event time minus `lateness`,
 * nothing before its first row, everything once it has ended), so a
 * side racing ahead cannot make the other side's rows late. A row
 * behind it is dropped and COUNTED. A side that ends frees the OTHER
 * side's store. Nothing here reads a clock.
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
    if (ended) Long.MaxValue
    else if (max == Long.MinValue) Long.MinValue
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

  private def trim[X](buf: mutable.ArrayBuffer[Held[X]], wm: Long): Unit =
    if (wm != Long.MinValue) {
      val before = buf.length
      buf.filterInPlace(h => h.at + within >= wm)
      live -= before - buf.length
    }

  private def trimAll[X](store: mutable.HashMap[K, mutable.ArrayBuffer[Held[X]]], wm: Long): Unit =
    store.filterInPlace((_, buf) => { trim(buf, wm); buf.nonEmpty })

  /** a sweep of every key once per `within` of watermark advance: a
   * key that never returns must not keep its rows for ever */
  private def advance(wm: Long): Unit =
    if (wm != Long.MinValue && (swept == Long.MinValue || wm - swept >= within || wm == Long.MaxValue) && wm != swept) {
      trimAll(lefts, wm); trimAll(rights, wm)
      swept = wm
    }

  private def clear[X](store: mutable.HashMap[K, mutable.ArrayBuffer[Held[X]]]): Unit = {
    store.foreach { case (_, buf) => live -= buf.length }
    store.clear()
  }

  private def arrive[X, Y](k: K, t: Long, x: X, mine: mutable.HashMap[K, mutable.ArrayBuffer[Held[X]]],
                           theirs: mutable.HashMap[K, mutable.ArrayBuffer[Held[Y]]], theirsEnded: Boolean)
                          (emit: Y => Unit): Unit = {
    val wm = watermark
    if (t < wm) late += 1
    else {
      theirs.get(k) match {
        case Some(buf) =>
          trim(buf, wm)
          if (buf.isEmpty) { theirs.remove(k); () }
          else {
            var i = 0
            while (i < buf.length) {
              val h = buf(i)
              if (math.abs(t - h.at) <= within) emit(h.x)
              i += 1
            }
          }
        case None => ()
      }
      if (!theirsEnded) {
        val buf = mine.getOrElseUpdate(k, mutable.ArrayBuffer.empty)
        trim(buf, wm)
        buf += new Held(t, x)
        live += 1
      }
    }
    advance(watermark)
  }

  def left(k: K, a: A)(emit: ((K, (A, B))) => Unit): Unit = {
    val t = atL(a)
    if (t > maxL) maxL = t
    arrive[A, B](k, t, a, lefts, rights, endedR)(b => emit((k, (a, b))))
  }

  def right(k: K, b: B)(emit: ((K, (A, B))) => Unit): Unit = {
    val t = atR(b)
    if (t > maxR) maxR = t
    arrive[B, A](k, t, b, rights, lefts, endedL)(a => emit((k, (a, b))))
  }

  def leftEnd(): Unit = { endedL = true; clear(rights); advance(watermark) }

  def rightEnd(): Unit = { endedR = true; clear(lefts); advance(watermark) }
}

object WindowJoin {
  /** one arrival of the merged sides: a row, or a side's end */
  type Event[K, A, B] = Either[Option[(K, A)], Option[(K, B)]]

  /** the join as a pipeline stage over the two sides' MERGED events;
   * takes the parameters, never an instance — a Stage is a value */
  def stage[K, A, B](within: Long, lateness: Long)(atL: A => Long, atR: B => Long)
  : Stage[Event[K, A, B], (K, (A, B)), Unit] = {
    type In = Event[K, A, B]
    type Out = (K, (A, B))
    def tellAll(buf: mutable.ArrayBuffer[Out], i: Int): Stage[In, Out, Unit] =
      if (i >= buf.length) { buf.clear(); pure(()) }
      else Stage.tell[In, Out](buf(i)).flatMap(_ => tellAll(buf, i + 1))

    def go(j: WindowJoin[K, A, B], buf: mutable.ArrayBuffer[Out]): Stage[In, Out, Unit] =
      Stage.await[In, Out].flatMap {
        case Some(ev) =>
          val join: WindowJoin[K, A, B] = if (j == null) new WindowJoin[K, A, B](within, lateness, atL, atR) else j
          val out = if (buf == null) mutable.ArrayBuffer.empty[Out] else buf
          ev match {
            case Left(Some((k, a))) => join.left(k, a)(o => { out += o; () })
            case Right(Some((k, b))) => join.right(k, b)(o => { out += o; () })
            case Left(None) => join.leftEnd()
            case Right(None) => join.rightEnd()
          }
          if (join.exhausted) tellAll(out, 0)
          else tellAll(out, 0).flatMap(_ => go(join, out))
        case None => pure(())
      }

    go(null, null)
  }
}
