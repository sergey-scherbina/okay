package okay

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.Queue

/**
 * A channel: a queue between fibers, the missing primitive of
 * CONCURRENT streams — everything the pull-based observation cannot
 * say (readiness, pacing) lives here. ONE implementation for every
 * platform (specs/cross-platform-async.md, channel-callback):
 * nobody waits in a thread. A receiver that finds the buffer empty
 * leaves a callback; a sender that finds it full leaves the element
 * and a callback; send, receive and close hand things straight to
 * the first waiter. `receive` and `send` are therefore Async
 * programs — the RUNTIME decides how to wait (runAsync: not at all;
 * Async.run: one park at the boundary, under CanBlock), and the
 * channel itself parks no thread and polls nothing. The state is
 * ONE immutable value in a TRef (the STM's cell, specs/stm.md),
 * every operation a pure transition installed by its single CAS
 * (channel-cas, then stm): no lock, not even a short one; callbacks
 * run after the CAS, outside any critical section.
 *
 * A channel is itself a Stream (in Async), but a LINEAR one: the
 * observation consumes — a repeated uncons reads the NEXT element,
 * not the same one. Bridge to LazyList (toLazyList memoizes) for a
 * re-observable view.
 */
final class Channel[A](capacity: Int = Int.MaxValue) {

  private type End = Either[Throwable, Option[A]]

  /** the whole channel as ONE immutable value: persistent queues and
   * a size counter (immutable.Queue's size is O(n)). Every operation
   * is a pure State => (State, action); the action runs only after
   * the CAS that installed the new state won — the Drive handshake's
   * shape, so no thread ever holds a lock, not even for an instant */
  private[okay] final case class State(
    buf: Queue[A], size: Int,
    receivers: Queue[End => Unit],
    senders: Queue[(A, Boolean => Unit)],
    open: Boolean,
    failure: Throwable | Null) extends TRef.Stamped[State]:
    def value: State = this
  // invariant: receivers waiting => buf empty and no sender waiting;
  // senders waiting => buf full (or capacity 0) and no receiver waiting

  /** the channel IS a one-cell STM structure (specs/stm.md): its
   * state is a TRef, its transitions go through TRef.modify — the
   * single-CAS path a one-op transaction takes — and the full
   * transaction language works on the same cell */
  private[okay] val cell = TRef.bare(State(Queue.empty, 0, Queue.empty, Queue.empty, true, null))

  private def transact[R](f: State => (State, () => R)): R = cell.modify(f)()

  private def endOf(s: State): End =
    if s.failure != null then Left(s.failure.nn) else Right(None)

  /** the callback form of send: k(true) once the channel TOOK the
   * element (handed to a receiver, or buffered — now, or later when
   * room opens), k(false) if it is closed: the element is dropped,
   * nothing thrown. A producer that outlives its stream is ordinary;
   * its send is a fact to read, not a fault to unwind. */
  def sendAsync(a: A)(k: Boolean => Unit): Unit = transact[Unit] { s =>
    if !s.open then (s, () => k(false))
    else if s.receivers.nonEmpty then
      val (r, rest) = s.receivers.dequeue
      (s.copy(receivers = rest), () => { r(Right(Some(a))); k(true) })
    else if s.size < capacity then
      (s.copy(buf = s.buf.enqueue(a), size = s.size + 1), () => k(true))
    else (s.copy(senders = s.senders.enqueue((a, k))), () => ())
  }

  /** the callback form of receive: now if an element (or the end) is
   * ready, later when one arrives. The end is Right(None), or Left of
   * the producer's failure — buffered elements drain first, the
   * failure is what the END of the stream is */
  def receiveAsync(k: End => Unit): Unit = transact[Unit] { s =>
    if s.buf.nonEmpty then
      val (a, rest) = s.buf.dequeue
      if s.senders.nonEmpty then
        // room opened: admit the first parked sender
        val ((b, sk), more) = s.senders.dequeue
        (s.copy(buf = rest.enqueue(b), senders = more), () => { sk(true); k(Right(Some(a))) })
      else (s.copy(buf = rest, size = s.size - 1), () => k(Right(Some(a))))
    else if s.senders.nonEmpty then
      // capacity 0: the rendezvous
      val ((b, sk), more) = s.senders.dequeue
      (s.copy(senders = more), () => { sk(true); k(Right(Some(b))) })
    else if !s.open then
      val e = endOf(s)
      (s, () => k(e))
    else (s.copy(receivers = s.receivers.enqueue(k)), () => ())
  }

  /** send as a program: suspends while the buffer is full, answers
   * whether the channel took the element (false: closed, dropped) */
  def send(a: A): Boolean ! Async =
    Async.await { k =>
      val cb: Boolean => Unit = b => k(Right(b))
      sendAsync(a)(cb)
      () => transact[Unit](s => (s.copy(senders = s.senders.filterNot(_._2 eq cb)), () => ()))
    }

  /** receive as a program: suspends while the channel is empty and
   * open; None once closed and drained, the producer's failure
   * (through the error channel) if one ended it */
  def receive: Option[A] ! Async =
    Async.await { k =>
      receiveAsync(k)
      () => transact[Unit](s => (s.copy(receivers = s.receivers.filterNot(_ eq k)), () => ()))
    }

  /** the non-suspending send: true if taken NOW (handed over or
   * buffered), false if the channel is closed or full — never
   * waits, never drops silently */
  def offer(a: A): Boolean = transact[Boolean] { s =>
    if !s.open then (s, () => false)
    else if s.receivers.nonEmpty then
      val (r, rest) = s.receivers.dequeue
      (s.copy(receivers = rest), () => { r(Right(Some(a))); true })
    else if s.size < capacity then
      (s.copy(buf = s.buf.enqueue(a), size = s.size + 1), () => true)
    else (s, () => false)
  }

  /** the parking forms, only where parking is GRANTED (JVM/Native;
   * a compile error on JS): the same programs, forced at this point */
  def sendBlocking(a: A)(using cb: CanBlock): Boolean =
    cb.block[Boolean](k => { sendAsync(a)(k); () => () })

  def receiveBlocking()(using cb: CanBlock): Option[A] =
    cb.block[End](k => { receiveAsync(k); () => () }).fold(e => throw e, identity)

  /** end the stream: the buffered elements still drain, parked
   * senders' elements were accepted before the end and join the
   * buffer, waiting receivers hear the end at once */
  def close(): Unit = transact[Unit] { s =>
    val admitted = s.senders
    val buf = admitted.foldLeft(s.buf)((q, e) => q.enqueue(e._1))
    val woken = if buf.isEmpty then s.receivers else Queue.empty
    val s2 = s.copy(buf = buf, size = s.size + admitted.size, senders = Queue.empty,
      receivers = if buf.isEmpty then Queue.empty else s.receivers, open = false)
    val e = endOf(s2)
    (s2, () => { admitted.foreach(_._2(true)); woken.foreach(_(e)) })
  }

  /**
   * Record that a producer broke. It does NOT close: in a merge the
   * other source is still feeding, and one side failing is no reason
   * to cut the side that is fine. `close` still ends the stream, and
   * the error is what the END then is — so a consumer receives
   * everything that was actually produced and only then hears that
   * something went wrong. Without this a producer that failed halfway
   * was indistinguishable from one that finished.
   */
  def fail(e: Throwable): Unit = transact[Unit] { s =>
    if s.failure == null then (s.copy(failure = e), () => ()) else (s, () => ())
  }

  /** what ended the stream, if anything did badly */
  def failed: Option[Throwable] = Option(cell.get.failure)

  def isClosed: Boolean = !cell.get.open
}

/** a channel is an async stream of what it receives (linear: see
 * above); the uncons is an Await, served by whoever sends */
given Stream[Channel, Async] with
  def uncons[A](c: Channel[A]): Option[(A, Channel[A])] ! Async =
    c.receive.map(_.map((_, c)))

object Channel {

  /** unfold a stream into the channel as an Async program; stops
   * early if the channel refuses (closed under the producer) */
  private def feed[A, U[_], H[+_]](c: Channel[A], u: U[A])
                                  (using St: Stream[U, H], HH: Handler[H]): Unit ! Async =
    def go(x: U[A]): Unit ! Async =
      async(St.uncons(x).runWith).flatMap {
        case Some((a, r)) => c.send(a).flatMap(ok => if ok then go(r) else pure(()))
        case None => pure(())
      }
    go(u)

  /**
   * Merge two streams by READINESS, not by turns: a fiber per source
   * feeds one channel, the loser of every race simply arrives later.
   * This is the concurrency zip and ++ cannot express — they are
   * strictly sequential. The channel closes when both sources end;
   * a source that fails is recorded (fail) and the other still feeds.
   */
  def merge[A, S[_], F[+_], T[_], G[+_]](s: S[A], t: T[A], capacity: Int = Int.MaxValue)
                                        (using Stream[S, F], Handler[F], Stream[T, G], Handler[G])
                                        (using sch: Scheduler): Channel[A] =
    val c = Channel[A](capacity)
    val alive = AtomicInteger(2)
    def watch(f: Fiber[Unit]): Unit = f.onComplete { r =>
      r.left.foreach(c.fail)
      if alive.decrementAndGet() == 0 then c.close()
    }
    watch(sch.fork(() => feed(c, s)))
    watch(sch.fork(() => feed(c, t)))
    c

  /**
   * Run the producer ahead of the consumer, at most capacity elements
   * ahead: a fiber unfolds the stream into a bounded channel — send
   * suspends the producer when the consumer lags by capacity.
   */
  def buffer[A, S[_], F[+_]](capacity: Int)(s: S[A])
                            (using Stream[S, F], Handler[F])(using sch: Scheduler): Channel[A] =
    val c = Channel[A](capacity)
    sch.fork(() => feed(c, s)).onComplete { r =>
      r.left.foreach(c.fail)
      c.close()
    }
    c
}
