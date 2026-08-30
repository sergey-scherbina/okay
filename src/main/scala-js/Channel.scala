package okay

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.collection.mutable

/**
 * The Await-based channel (specs/cross-platform-async.md): the same
 * surface as the JVM one with no parking anywhere — a receive that
 * finds the buffer empty leaves a callback, send hands the element
 * straight to the first waiter. On the single JS thread nothing
 * races. Capacity is advisory: a JS sender cannot park, so it never
 * waits — bound producers by demand (the pull side), not by buffer
 * pressure.
 */
final class Channel[A](capacity: Int = Int.MaxValue) {

  private val q = mutable.Queue[A]()
  private val waiters = mutable.Queue[Option[A] => Unit]()
  private var open = true

  /** enqueue, or hand straight to a waiting receiver */
  def send(a: A): Unit =
    if waiters.nonEmpty then waiters.dequeue()(Some(a))
    else q.enqueue(a)

  /** end the stream: the buffered elements still drain */
  def close(): Unit =
    open = false
    while waiters.nonEmpty && q.isEmpty do waiters.dequeue()(None)

  /** the callback form of receive: now if an element (or the end) is
   * ready, later when one arrives */
  def receiveAsync(k: Option[A] => Unit): Unit =
    if q.nonEmpty then k(Some(q.dequeue()))
    else if !open then k(None)
    else waiters.enqueue(k)
}

/** a channel is an async stream of what it receives (linear): the
 * uncons is an Await, served by the event loop */
given Stream[Channel, Async] with
  def uncons[A](c: Channel[A]): Option[(A, Channel[A])] ! Async =
    await(k => c.receiveAsync(o => k(o.map((_, c)))))

object Channel {

  /** merge two streams into one channel; the sources feed on their
   * own (event-loop) fibers, the channel closes when both end */
  def merge[A, S[_], F[+_], T[_], G[+_]](s: S[A], t: T[A], capacity: Int = Int.MaxValue)
                                        (using Stream[S, F], Handler[F], Stream[T, G], Handler[G])
                                        (using sch: Scheduler): Channel[A] =
    val c = Channel[A](capacity)
    val alive = AtomicInteger(2)
    inline def feed[U[_], H[+_]](u: U[A])(using St: Stream[U, H], HH: Handler[H]): Unit =
      try
        @tailrec def go(x: U[A]): Unit = St.uncons(x).runWith match
          case Some((a, r)) => c.send(a); go(r)
          case None => ()
        go(u)
      finally if alive.decrementAndGet() == 0 then c.close()
    sch.fork(() => async(feed(s)))
    sch.fork(() => async(feed(t)))
    c

  /** unfold the stream into the channel on its own fiber */
  def buffer[A, S[_], F[+_]](capacity: Int)(s: S[A])
                            (using Stream[S, F], Handler[F], Scheduler): Channel[A] =
    val c = Channel[A](capacity)
    summon[Scheduler].fork: () =>
      async:
        try s.toLazyList.foreach(c.send)
        finally c.close()
    c
}

/** merge two chunked streams (type args spelled out — inference
 * abstracts the wrong slot through the nested alias) */
def mergeChunks[A](s: Chunks[A], t: Chunks[A], capacity: Int = Int.MaxValue)
                  (using Scheduler): Channel[Chunk[A]] =
  Channel.merge[Chunk[A], Producer, Pure, Producer, Pure](s, t, capacity)
