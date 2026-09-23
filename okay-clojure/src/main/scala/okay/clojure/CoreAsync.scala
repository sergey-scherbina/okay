package okay.clojure

import okay.Channel
import clojure.lang.{AFn, IFn}
import scala.collection.mutable
import scala.reflect.ClassTag

/**
 * A core.async channel AS an okay `Channel` (specs/clojure.md, stage 3):
 * okay's streams, `Source`s, `merge`s and actors read and write a
 * `clojure.core.async` channel through it, and Clojure code on the other
 * end sees an ordinary core.async channel — its `go` blocks, its
 * transducers, its `alts!`.
 *
 * okay's `Channel` promises more than a queue (Channel.scala, `close`):
 * close is two-phase, the end comes after the buffer, acceptance is final
 * — and `TestChannelLaws`' battery checks this view against it, the same
 * laws every okay channel answers for. What makes it keep them over
 * core.async's callbacks:
 *
 *  - ONE `take!` in flight. okay's receivers queue here; the answer goes
 *    to the first that has not given up, or to a STASH the next receive
 *    takes first. core.async cannot withdraw a registered `take!`, so a
 *    cancelled receive must neither lose the element that arrives for it
 *    nor let a later element overtake it.
 *  - ONE `put!` in flight, sends queued here in order: a cancelled send
 *    still in the queue is withdrawn; the one already handed to
 *    core.async cannot be — it will be delivered, which acceptance
 *    permits (the sender simply is not told).
 *  - close is two-phase: new sends are refused at once, the queued ones
 *    finish, and only then `close!`; `finished` is true only once
 *    core.async has answered `nil` — closed AND empty.
 *
 * core.async is called OUTSIDE the lock: its callbacks may run on the
 * calling thread, and a callback re-entering the lock mid-decision is
 * the kind of window the Channel contract exists to close.
 */
final class CoreAsyncChannel[A](val chan: AnyRef)(using ct: ClassTag[A]) extends Channel[A] {

  private val lock = new Object
  private val waiters = mutable.Queue.empty[End => Unit]
  private val stash = mutable.Queue.empty[A]
  private var taking = false
  private var drained = false
  private val sends = mutable.Queue.empty[(A, Boolean => Unit)]
  private var putting = false
  private var closing = false
  private var closedCore = false
  @volatile private var failure: Option[Throwable] = None

  /** an erased element handed to core.async, which takes `Object`: at
   * run time it already IS one (a primitive arrives boxed), so the
   * ascription checks nothing and cannot fail */
  private def boxed(a: A): AnyRef = a.asInstanceOf[AnyRef]

  private def fn1(f: AnyRef => Unit): IFn = new AFn:
    override def invoke(x: AnyRef): AnyRef = { f(x); null }

  /** an element from core.async, as the type the okay side declared */
  private def element(x: AnyRef): Either[Throwable, A] = x match
    case ct(a) => Right(a)
    case other => Left(IllegalArgumentException(
      s"okay.clojure.CoreAsyncChannel: expected ${ct.runtimeClass.getName}, got ${other.getClass.getName}"))

  private def theEnd: End = failure.fold(Right(None))(Left(_))

  // ------------------------------------------------------------ sending

  def sendAsync(a: A)(k: Boolean => Unit): Unit =
    val refused = lock.synchronized {
      if closing then true else { sends.enqueue((a, k)); false }
    }
    if refused then k(false) else startPut()

  private def startPut(): Unit =
    val next = lock.synchronized {
      if putting || sends.isEmpty then None
      else { putting = true; Some(sends.dequeue()) }
    }
    next.foreach { (a, k) =>
      CoreAsync.put.invoke(chan, boxed(a), fn1 { accepted =>
        lock.synchronized { putting = false }
        k(accepted == java.lang.Boolean.TRUE)
        startPut()
        finishClose()
      }): Unit
    }

  def offer(a: A): Boolean =
    val free = lock.synchronized { !closing && !putting && sends.isEmpty }
    free && CoreAsync.offer.invoke(chan, boxed(a)) == java.lang.Boolean.TRUE

  private[okay] def cancelSend(cb: Boolean => Unit): Unit =
    lock.synchronized { sends.dequeueAll(_._2 eq cb) }: Unit

  // ----------------------------------------------------------- receiving

  def receiveAsync(k: End => Unit): Unit =
    val now = lock.synchronized {
      if stash.nonEmpty then Some(Right(Some(stash.dequeue())))
      else if drained then Some(theEnd)
      else { waiters.enqueue(k); None }
    }
    now match
      case Some(e) => k(e)
      case None => startTake()

  private def startTake(): Unit =
    val go = lock.synchronized {
      if taking || waiters.isEmpty || drained then false else { taking = true; true }
    }
    if go then CoreAsync.take.invoke(chan, fn1(arrived)): Unit

  private def arrived(v: AnyRef): Unit =
    val deliveries: List[(End => Unit, End)] = lock.synchronized {
      taking = false
      if v == null then
        drained = true
        val all = waiters.dequeueAll(_ => true).toList
        all.map(w => (w, theEnd))
      else element(v) match
        case Left(err) =>
          if waiters.nonEmpty then List((waiters.dequeue(), Left(err))) else Nil
        case Right(a) =>
          if waiters.nonEmpty then List((waiters.dequeue(), Right(Some(a))))
          else { stash.enqueue(a); Nil }
    }
    deliveries.foreach((w, e) => w(e))
    startTake()

  private[okay] def cancelReceive(k: End => Unit): Unit =
    lock.synchronized { waiters.dequeueAll(_ eq k) }: Unit

  // --------------------------------------------------------------- close

  def close(): Unit =
    lock.synchronized { closing = true }
    finishClose()

  /** the second phase: once no send of ours is queued or in flight */
  private def finishClose(): Unit =
    val now = lock.synchronized {
      if closing && !closedCore && !putting && sends.isEmpty then { closedCore = true; true } else false
    }
    if now then CoreAsync.close.invoke(chan): Unit

  def fail(e: Throwable): Unit = failure = Some(e)
  def failed: Option[Throwable] = failure
  def isClosed: Boolean = lock.synchronized(closing)

  /**
   * Closed AND empty, answered without a receiver waiting: once core.async
   * is closed (our own sends done), a `poll!` that answers nil means no
   * element will ever come — and one that answers an element is stashed
   * for the next receive, never lost.
   */
  private[okay] def finished: Boolean =
    val probe = lock.synchronized {
      if drained && stash.isEmpty then Some(true)
      else if stash.nonEmpty || !closedCore || taking then Some(false)
      else None
    }
    probe.getOrElse {
      val v = CoreAsync.poll.invoke(chan)
      lock.synchronized {
        if v == null then { drained = true; stash.isEmpty }
        else
          element(v).foreach(stash.enqueue)
          false
      }
    }
}

/** core.async, reached as its own vars */
object CoreAsync {
  private def core(name: String): IFn =
    Clj.fn("clojure.core.async", name).fold(e => throw IllegalStateException(e), identity)

  private[clojure] lazy val put: IFn = core("put!")
  private[clojure] lazy val take: IFn = core("take!")
  private[clojure] lazy val offer: IFn = core("offer!")
  private[clojure] lazy val poll: IFn = core("poll!")
  private[clojure] lazy val close: IFn = core("close!")
  private lazy val chanFn: IFn = core("chan")

  /** a new core.async channel with a fixed buffer, as an okay Channel */
  def channel[A: ClassTag](capacity: Int): CoreAsyncChannel[A] =
    CoreAsyncChannel[A](chanFn.invoke(Long.box(capacity.toLong)))

  /** an existing core.async channel (made in Clojure, with its own
   * buffer and transducer) as an okay Channel */
  def of[A: ClassTag](chan: AnyRef): CoreAsyncChannel[A] = CoreAsyncChannel[A](chan)
}
