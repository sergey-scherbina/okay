package okay

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

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
 * channel itself parks no thread and polls nothing. The state is a
 * few queues under a short lock; callbacks run outside it.
 *
 * A channel is itself a Stream (in Async), but a LINEAR one: the
 * observation consumes — a repeated uncons reads the NEXT element,
 * not the same one. Bridge to LazyList (toLazyList memoizes) for a
 * re-observable view.
 */
final class Channel[A](capacity: Int = Int.MaxValue) {

  private type End = Either[Throwable, Option[A]]
  private val lock = new AnyRef
  private val buf = mutable.Queue[A]()
  private val receivers = mutable.Queue[End => Unit]()
  private val senders = mutable.Queue[(A, Boolean => Unit)]()
  private var open = true
  private var failure: Throwable | Null = null
  // invariant: receivers waiting => buf empty and no sender waiting;
  // senders waiting => buf full (or capacity 0) and no receiver waiting

  private def end: End = if failure != null then Left(failure.nn) else Right(None)

  /** the callback form of send: k(true) once the channel TOOK the
   * element (handed to a receiver, or buffered — now, or later when
   * room opens), k(false) if it is closed: the element is dropped,
   * nothing thrown. A producer that outlives its stream is ordinary;
   * its send is a fact to read, not a fault to unwind. */
  def sendAsync(a: A)(k: Boolean => Unit): Unit =
    val go: () => Unit = lock.synchronized {
      if !open then () => k(false)
      else if receivers.nonEmpty then
        val r = receivers.dequeue()
        () => { r(Right(Some(a))); k(true) }
      else if buf.size < capacity then
        buf.enqueue(a)
        () => k(true)
      else
        senders.enqueue((a, k))
        () => ()
    }
    go()

  /** the callback form of receive: now if an element (or the end) is
   * ready, later when one arrives. The end is Right(None), or Left of
   * the producer's failure — buffered elements drain first, the
   * failure is what the END of the stream is */
  def receiveAsync(k: End => Unit): Unit =
    val go: () => Unit = lock.synchronized {
      if buf.nonEmpty then
        val a = buf.dequeue()
        if senders.nonEmpty then
          // room opened: admit the first parked sender
          val (b, sk) = senders.dequeue()
          buf.enqueue(b)
          () => { sk(true); k(Right(Some(a))) }
        else () => k(Right(Some(a)))
      else if senders.nonEmpty then
        // capacity 0: the rendezvous
        val (b, sk) = senders.dequeue()
        () => { sk(true); k(Right(Some(b))) }
      else if !open then
        val e = end
        () => k(e)
      else
        receivers.enqueue(k)
        () => ()
    }
    go()

  /** send as a program: suspends while the buffer is full, answers
   * whether the channel took the element (false: closed, dropped) */
  def send(a: A): Boolean ! Async =
    Async.await { k =>
      val cb: Boolean => Unit = b => k(Right(b))
      sendAsync(a)(cb)
      () => lock.synchronized { senders.removeFirst(_._2 eq cb): Unit }
    }

  /** receive as a program: suspends while the channel is empty and
   * open; None once closed and drained, the producer's failure
   * (through the error channel) if one ended it */
  def receive: Option[A] ! Async =
    Async.await { k =>
      receiveAsync(k)
      () => lock.synchronized { receivers.removeFirst(_ eq k): Unit }
    }

  /** the non-suspending send: true if taken NOW (handed over or
   * buffered), false if the channel is closed or full — never
   * waits, never drops silently */
  def offer(a: A): Boolean =
    val go: () => Boolean = lock.synchronized {
      if !open then () => false
      else if receivers.nonEmpty then
        val r = receivers.dequeue()
        () => { r(Right(Some(a))); true }
      else if buf.size < capacity then
        buf.enqueue(a)
        () => true
      else () => false
    }
    go()

  /** the parking forms, only where parking is GRANTED (JVM/Native;
   * a compile error on JS): the same programs, forced at this point */
  def sendBlocking(a: A)(using cb: CanBlock): Boolean =
    cb.block[Boolean](k => { sendAsync(a)(k); () => () })

  def receiveBlocking()(using cb: CanBlock): Option[A] =
    cb.block[End](k => { receiveAsync(k); () => () }).fold(e => throw e, identity)

  /** end the stream: the buffered elements still drain, parked
   * senders' elements were accepted before the end and join the
   * buffer, waiting receivers hear the end at once */
  def close(): Unit =
    val go: () => Unit = lock.synchronized {
      open = false
      val admitted = senders.dequeueAll(_ => true)
      admitted.foreach((b, _) => buf.enqueue(b))
      val woken = if buf.isEmpty then receivers.dequeueAll(_ => true) else Seq.empty
      val e = end
      () => { admitted.foreach((_, sk) => sk(true)); woken.foreach(_(e)) }
    }
    go()

  /**
   * Record that a producer broke. It does NOT close: in a merge the
   * other source is still feeding, and one side failing is no reason
   * to cut the side that is fine. `close` still ends the stream, and
   * the error is what the END then is — so a consumer receives
   * everything that was actually produced and only then hears that
   * something went wrong. Without this a producer that failed halfway
   * was indistinguishable from one that finished.
   */
  def fail(e: Throwable): Unit = lock.synchronized { if failure == null then failure = e }

  /** what ended the stream, if anything did badly */
  def failed: Option[Throwable] = lock.synchronized(Option(failure))

  def isClosed: Boolean = lock.synchronized(!open)
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
