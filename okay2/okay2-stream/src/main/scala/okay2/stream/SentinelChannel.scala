package okay2.stream

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.collection.mutable.ArrayBuffer
import okay2.async.{Accepted, Handoff}

/**
 * A mark travelling in the buffer alongside the elements: `end` closes
 * the stream at that exact position; a void is a slot a sender won and
 * declined, which the receiver steps over. Private to the package with
 * a private constructor, so no caller can put one into a channel.
 */
private[stream] final class Mark private[stream] (val end: Boolean)

/**
 * The default channel — okay-stream's SentinelChannel.scala: a mutable
 * ring for the mechanism and a mark in the FIFO stream for the guarantee.
 *
 * WHY THIS SHAPE, measured in the Scala 3 core: `StmChannel` rebuilds an
 * immutable value per transaction and 40% of its samples were
 * `List.reverse`; the same contract assembled as a weak ring plus a
 * sentinel riding it cost 2.4% over the weak mechanism where the
 * invariant had cost 47%.
 *
 * WHY THE MARK IS IN THE RING: "the channel is open" and "my element is
 * in" are two facts a sender learns at two instants, and close can land
 * between them. Here termination is an ELEMENT, ordered by the ring's
 * own tail CAS against every send, and a sender decides what to publish
 * only after winning its position (`pushDeciding`): if close landed
 * first it publishes a void and answers false. Four drafts that kept the
 * end beside the ring met that window; this one has nowhere to open it.
 */
final class SentinelChannel[A](buf: Buffer[Any]) extends Channel[A] {

  /** the bounded channel: a fixed ring */
  def this(requested: Int) = this(new Ring[Any](requested))

  private final class Waiter(val resume: () => Unit) {
    val claimed = new AtomicBoolean(false)
    def claim(): Boolean = claimed.compareAndSet(false, true)
  }

  private val ring: Buffer[Any] = buf
  private val receivers = new ConcurrentLinkedQueue[Waiter]()
  /** senders wait PER PART, because room appears per part; sized by
   * `maxParts`, not `parts`, since an adaptive buffer grows */
  private val senders: Array[ConcurrentLinkedQueue[Waiter]] =
    Array.fill(if (buf.maxParts < 1) 1 else buf.maxParts)(new ConcurrentLinkedQueue[Waiter]())

  private def sendersAt(route: Int): ConcurrentLinkedQueue[Waiter] =
    senders(if (senders.length == 1) 0 else Math.floorMod(route, senders.length))

  private def wakeSender(): Unit = { val _ = wakeOne(sendersAt(ring.lastRoute)) }

  private def wakeAllSenders(): Unit = {
    var i = 0
    while (i < senders.length) { wakeAll(senders(i)); i += 1 }
  }

  /** close has been decided: no further element is accepted */
  private val closing = new AtomicBoolean(false)
  /** the end mark did not fit yet; the next freed slot takes it */
  private val endPending = new AtomicBoolean(false)
  /** the end has been REACHED by a receiver */
  private val ended = new AtomicBoolean(false)
  /** parts with their end mark in, and end marks a receiver has met: the
   * stream is over when the second reaches `parts` */
  private val partsSealed = new AtomicInteger(0)
  private val metEnds = new AtomicInteger(0)
  /** the last end mark, taken out by a bulk receive that also carried
   * elements and so could not answer with it yet */
  private val reached = new AtomicReference[Mark](null)
  private val failure = new AtomicReference[Throwable](null)
  /** how many MARKS the ring holds (rare: one end and, under a close
   * race, voids), so elements outstanding is `ring.size - marks` — an
   * element counter on the hot path cost 2x in the Scala 3 core */
  private val marks = new AtomicInteger(0)

  /** the one declined-slot value, held for the channel's life: no
   * allocation in the claim-to-publish window */
  private val void: Any = new Mark(false)

  val capacity: Int = ring.capacity

  /**
   * THE ONE CAST, as in the Scala 3 core: a slot holds an element or a
   * `Mark`, every `Mark` is matched before this runs, and no `Mark` can
   * come from outside (private class, private constructor), so what is
   * left is an A the channel itself was handed.
   */
  private def element(x: Any): A = x.asInstanceOf[A]

  private def enqueue(q: ConcurrentLinkedQueue[Waiter], w: Waiter): Unit = { val _ = q.offer(w) }

  /** the oldest live waiter, resumed; claimed ones are skipped */
  private def wakeOne(q: ConcurrentLinkedQueue[Waiter]): Boolean = {
    var out = false
    var go = true
    while (go) {
      val w = q.poll()
      if (w == null) go = false
      else if (w.claim()) { w.resume(); out = true; go = false }
    }
    out
  }

  /** wake a SNAPSHOT, never the live queue: a resumed receiver that finds
   * nothing re-parks into the same queue, and draining the live one
   * re-woke it for ever (100% CPU for minutes in the Scala 3 core) */
  private def wakeAll(q: ConcurrentLinkedQueue[Waiter]): Unit = {
    val batch = ArrayBuffer.empty[Waiter]
    var go = true
    while (go) {
      val w = q.poll()
      if (w == null) go = false else batch += w
    }
    batch.foreach(w => if (w.claim()) w.resume())
  }

  /** put the end mark in — one per ORDER — as soon as there is room */
  private def placeEnd(): Unit = {
    val buffer = ring
    if (endPending.get) {
      val placed = buffer.seal(new Mark(true))
      if (placed > 0) {
        marks.addAndGet(placed)
        if (partsSealed.addAndGet(placed) >= buffer.parts) endPending.set(false)
      }
    }
  }

  private def endAnswer: End = {
    val e = failure.get
    if (e != null) Left(e) else Right(None)
  }

  /** the failure is read HERE, at the end: `fail` records without closing */
  private def endReached(): End = {
    ended.set(true)
    wakeAll(receivers)
    endAnswer
  }

  /** `receiveAsync`'s first scan with an early return: a ready element
   * goes into the handoff directly and the caller never parks */
  override private[stream] def receiveInto(h: Handoff[A]): Boolean = {
    val buffer = ring
    var hit = false
    var go = true
    while (go) {
      go = false
      if (ended.get) h(endAnswer)
      else {
        var answered = false
        var stepped = true
        while (stepped && !answered) {
          stepped = false
          buffer.pop() match {
            case null => ()
            case m: Mark =>
              marks.decrementAndGet()
              wakeSender()
              placeEnd()
              if (m.end) {
                if (metEnds.incrementAndGet() >= buffer.parts) { h(endReached()); answered = true }
                else stepped = true
              } else stepped = true
            case other =>
              h.got(element(other))
              hit = true
              wakeSender()
              placeEnd()
              answered = true
          }
        }
        if (!answered && reached.get != null) { h(endReached()); answered = true }
        if (!answered) {
          val w = new Waiter(() => receiveAsync(h))
          enqueue(receivers, w)
          if ((buffer.hasReady || ended.get) && w.claim()) {
            val _ = receivers.remove(w)
            go = true
          }
        }
      }
    }
    hit
  }

  def sendAsync(a: A)(k: Accepted): Unit = attemptSend(a, granted0 = false, route0 = 0)(k)

  /** the route is taken HERE, on the producer's thread, AFTER the buffer
   * is read, and carried through every retry (growing-stale-route) */
  private def attemptSend(a: A, granted0: Boolean, route0: Int)(k: Accepted): Unit = {
    val buffer = ring
    val route = if (granted0) route0 else buffer.route()
    val granted = granted0
    var go = true
    while (go) {
      go = false
      if (closing.get) k(false)
      else if (granted || sendersAt(route).isEmpty) {
        val pushed =
          if (granted) buffer.pushDecidingAtOnBehalf(route, a, closing, void)
          else buffer.pushDecidingAt(route, a, closing, void)
        pushed match {
          case null =>
            // full: park, and re-check in case a pop freed a slot between
            val w = new Waiter(() => attemptSend(a, granted0 = true, route)(k))
            enqueue(sendersAt(route), w)
            if ((buffer.hasRoomAt(route) || closing.get) && w.claim()) {
              val _ = sendersAt(route).remove(w)
              go = true
            }
          case _: Mark =>
            // close landed between the open check and the claim: a void
            marks.incrementAndGet()
            k(false)
            val _ = wakeOne(receivers)
          case _ =>
            k(true)
            val _ = wakeOne(receivers)
        }
      } else {
        val w = new Waiter(() => attemptSend(a, granted0 = true, route)(k))
        enqueue(sendersAt(route), w)
        if ((buffer.hasRoomAt(route) || closing.get) && w.claim()) {
          val _ = sendersAt(route).remove(w)
          go = true
        }
      }
    }
  }

  def receiveAsync(k: End => Unit): Unit = {
    val buffer = ring
    var go = true
    while (go) {
      go = false
      if (ended.get) k(endAnswer)
      else {
        var answered = false
        var stepped = true
        while (stepped && !answered) {
          stepped = false
          buffer.pop() match {
            case null => ()
            case m: Mark =>
              marks.decrementAndGet()
              wakeSender()
              placeEnd()
              if (m.end) {
                if (metEnds.incrementAndGet() >= buffer.parts) { k(endReached()); answered = true }
                else stepped = true   // another order still has elements
              } else stepped = true   // a void: step over it
            case other =>
              k(Right(Some(element(other))))
              wakeSender()
              placeEnd()
              answered = true
          }
        }
        // a bulk receive took the end mark out and answered with elements;
        // it is delivered here — parking would park for good
        if (!answered && reached.get != null) { k(endReached()); answered = true }
        if (!answered) {
          val w = new Waiter(() => receiveAsync(k))
          enqueue(receivers, w)
          if ((buffer.hasReady || ended.get) && w.claim()) {
            val _ = receivers.remove(w)
            go = true
          }
        }
      }
    }
  }

  /** the bulk receive: one head CAS for the whole run. The end mark cannot
   * ride out with elements (an empty chunk IS the end), so it is parked
   * in `reached` and delivered on the next call */
  override private[stream] def receiveManyAsync(max: Int)(k: Either[Throwable, Chunk[A]] => Unit): Unit = {
    val buffer = ring
    if (ended.get) k(endAnswer.map(_ => Chunks.emptyChunk[A]))
    else {
      val room = if (max < buffer.capacity) max else buffer.capacity
      val out = ChunkBuf[A](room)
      var n = 0
      val took = buffer.popMany(room) {
        case m: Mark =>
          marks.decrementAndGet()
          if (m.end && metEnds.incrementAndGet() >= buffer.parts) reached.set(m)
        case other => out.update(n, element(other)); n += 1
      }
      if (n > 0) {
        var i = 0
        while (i < took) { wakeSender(); i += 1 }
        placeEnd()
        k(Right(out.take(n)))
      } else if (reached.get != null) k(endReached().map(_ => Chunks.emptyChunk[A]))
      else {
        if (took > 0) {
          var i = 0
          while (i < took) { wakeSender(); i += 1 }
          placeEnd()
        }
        receiveAsync(e => k(e.map(_.fold(Chunks.emptyChunk[A])(a => ChunkBuf.of(Seq(a))))))
      }
    }
  }

  def offer(a: A): Boolean = {
    val buffer = ring
    val r = buffer.route()
    if (closing.get || !sendersAt(r).isEmpty) false
    else buffer.pushDecidingAt(r, a, closing, void) match {
      case null => false
      case _: Mark =>
        marks.incrementAndGet()
        val _ = wakeOne(receivers)
        false
      case _ =>
        val _ = wakeOne(receivers)
        true
    }
  }

  def close(): Unit =
    if (closing.compareAndSet(false, true)) {
      // a parked sender was never accepted: refusing it is the truthful answer
      wakeAllSenders()
      endPending.set(true)
      placeEnd()
      wakeAll(receivers)
    }

  /** RECORD, do not close: the failure rides the END of the stream */
  def fail(e: Throwable): Unit = { val _ = failure.compareAndSet(null, e) }

  def failed: Option[Throwable] = Option(failure.get)
  def isClosed: Boolean = closing.get

  private[stream] def finished: Boolean = ended.get || (closing.get && ring.size <= marks.get)

  private[stream] def cancelSend(cb: Accepted): Unit = ()
  private[stream] def cancelReceive(k: End => Unit): Unit = ()
}
