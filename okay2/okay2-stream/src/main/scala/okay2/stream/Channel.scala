package okay2.stream

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import okay2._
import okay2.async._

/**
 * ONE immutable value behind ONE compare-and-set: the cell every
 * transition of a `StmChannel` goes through. `modify` computes the
 * next state and an action from the current one, installs the state
 * by CAS, retries on a lost race, and runs the action only after the
 * CAS that installed it won — so no thread ever holds a lock, and a
 * callback runs outside any critical section. The Scala 3 core's
 * `TRef.modify` at one operation; the transaction language over
 * several cells (okay-stm) is a later stage.
 */
final class Cell[S](init: S) {
  private val ref = new AtomicReference[S](init)

  def get: S = ref.get

  @tailrec def modify[R](f: S => (S, () => R)): R = {
    val cur = ref.get
    val (next, action) = f(cur)
    if (ref.compareAndSet(cur, next)) action() else modify(f)
  }
}

/**
 * A channel: a queue between fibers, the missing primitive of
 * CONCURRENT streams — everything the pull-based observation cannot
 * say (readiness, pacing) lives here. Nobody waits in a thread: a
 * receiver that finds the buffer empty leaves a callback; a sender
 * that finds it full leaves the element and a callback; send, receive
 * and close hand things straight to the first waiter. `receive` and
 * `send` are therefore Async programs — the RUNTIME decides how to
 * wait — and the channel itself parks no thread and polls nothing.
 *
 * THE CONTRACT of `close`, written here because two Scala 3
 * implementations rediscovered it by failing a gate: close is
 * two-phase (refuse new sends, wait for sends already accepted, only
 * then publish the end); the end comes AFTER the buffer, never
 * instead of it; and ACCEPTANCE IS FINAL — if `send` answered true,
 * that element WILL be delivered. `StmChannel` gets all three for free
 * from its single atomic transition.
 *
 * An implementation provides the callback primitives and the two
 * cancellers; everything a caller usually touches is derived here.
 */
trait Channel[A] {

  type End = Either[Throwable, Option[A]]

  /** k(true) once the channel TOOK the element, k(false) if it is closed */
  def sendAsync(a: A)(k: Accepted): Unit

  /** now if an element (or the end) is ready, later when one arrives */
  def receiveAsync(k: End => Unit): Unit

  /** the non-suspending send: true if taken NOW, never waits */
  def offer(a: A): Boolean

  /** end the stream: buffered elements still drain */
  def close(): Unit

  /** record that a producer broke, WITHOUT closing */
  def fail(e: Throwable): Unit

  def failed: Option[Throwable]
  def isClosed: Boolean

  /** closed AND finished — the single fact a consumer terminates on */
  private[stream] def finished: Boolean

  private[stream] def cancelSend(cb: Accepted): Unit
  private[stream] def cancelReceive(k: End => Unit): Unit

  /** send as a program: suspends while the buffer is full, answers
   * whether the channel took the element (false: closed, dropped) */
  def send(a: A): Boolean ! Async =
    Async.await[Boolean] { k =>
      val cb: Accepted = b => k(Right(b))
      sendAsync(a)(cb)
      () => cancelSend(cb)
    }

  /** receive as a program: suspends while the channel is empty and
   * open; None once closed and drained, the producer's failure
   * (through the error channel) if one ended it */
  def receive: Option[A] ! Async =
    Async.await[Option[A]] { k =>
      receiveAsync(k)
      () => cancelReceive(k)
    }

  /** the parking send, where parking is GRANTED: OFFER FIRST, since a
   * channel with room has nothing to wait for; the handshake only on
   * the element the ring refused */
  def sendBlocking(a: A)(implicit cb: CanBlock): Boolean =
    if (offer(a)) true
    else cb.blockAccepted(k => { sendAsync(a)(k); () => () })

  /** the receive whose try is its own first scan: true if an element
   * was ready NOW and written into `h`; otherwise `h` is registered */
  private[stream] def receiveInto(h: Handoff[A]): Boolean = {
    receiveAsync(h)
    false
  }

  def receiveBlocking()(implicit cb: CanBlock): Option[A] = {
    val h = cb.handoff[A]()
    if (!receiveInto(h)) cb.await(h)
    h.answer
  }

  /** put up to `n` elements in one go; never waits */
  private[stream] def sendManyNow(n: Int)(src: Int => A): Int = {
    var i = 0
    var go = true
    while (go && i < n) { if (offer(src(i))) i += 1 else go = false }
    i
  }

  /** take up to `max` elements that are ALREADY buffered, in one go;
   * the default is the honest one-at-a-time answer */
  private[stream] def receiveManyAsync(max: Int)(k: Either[Throwable, Chunk[A]] => Unit): Unit = {
    val _ = max
    receiveAsync(e => k(e.map(_.fold(Chunks.emptyChunk[A])(a => ChunkBuf.of(Seq(a))))))
  }

  /** up to `max` buffered elements as a program; an empty answer is the end */
  private[stream] def receiveMany(max: Int): Chunk[A] ! Async =
    Async.await[Chunk[A]] { k => receiveManyAsync(max)(k); () => () }
}

/**
 * The channel whose whole state is ONE immutable value in a `Cell`:
 * persistent queues and a size counter, every operation a pure
 * transition installed by one CAS, its action run after the CAS won.
 * The reference implementation: acceptance and ordering are the same
 * instant, so the close contract holds by construction. `StmChannel`
 * by its Scala 3 name; the STM that name refers to is a later stage,
 * and here the cell is a plain CAS.
 */
final class StmChannel[A](capacity: Int = Int.MaxValue, emptyBuf: () => Fifo[A] = () => Fifo.array[A]) extends Channel[A] {

  import StmChannel.State

  private[stream] val cell = new Cell[State[A]](State[A](emptyBuf(), 0, Queue.empty, Queue.empty, true, null))

  private def transact[R](f: State[A] => (State[A], () => R)): R = cell.modify(f)

  private def endOf(s: State[A]): End = if (s.failure != null) Left(s.failure) else Right(None)

  def sendAsync(a: A)(k: Accepted): Unit = transact[Unit] { s =>
    if (!s.open) (s, () => k(false))
    else if (s.receivers.nonEmpty) {
      val (r, rest) = s.receivers.dequeue
      (s.copy(receivers = rest), () => { r(Right(Some(a))); k(true) })
    }
    else if (s.size < capacity) (s.copy(buf = s.buf.enqueue(a), size = s.size + 1), () => k(true))
    else (s.copy(senders = s.senders.enqueue((a, k))), () => ())
  }

  def receiveAsync(k: End => Unit): Unit = transact[Unit](receiveOne(_)(k))

  private[stream] def cancelSend(cb: Accepted): Unit =
    transact[Unit](s => (s.copy(senders = s.senders.filterNot(_._2 eq cb)), () => ()))

  private[stream] def cancelReceive(k: End => Unit): Unit =
    transact[Unit](s => (s.copy(receivers = s.receivers.filterNot(_ eq k)), () => ()))

  /** take up to `max` elements that are ALREADY buffered, in ONE
   * transaction: what is buffered is already late, and taking ten under
   * one CAS instead of ten hands the consumer the same elements in the
   * same order. The transition is cheap and the work is not in it: the
   * chunk is filled in the action, after the CAS won */
  private[stream] override def receiveManyAsync(max: Int)(k: Either[Throwable, Chunk[A]] => Unit): Unit =
    transact[Unit] { s =>
      if (s.buf.isEmpty) receiveOne(s)(e => k(e.map(_.fold(Chunks.emptyChunk[A])(a => ChunkBuf.of(Seq(a))))))
      else {
        val take = math.min(max, s.size)
        var senders = s.senders
        var admitted = List.empty[A]
        var woken = List.empty[Accepted]
        var m = 0
        while (m < take && senders.nonEmpty) {
          val ((sa, sk), more) = senders.dequeue
          admitted = sa :: admitted
          woken = sk :: woken
          senders = more
          m += 1
        }
        val rest = s.buf.drop(take, s.size)
        val b2 = admitted.foldRight(rest)((a, q) => q.enqueue(a))
        val taken = s.buf
        val size0 = s.size
        val s2 = s.copy(buf = b2, size = size0 - take + m, senders = senders)
        (s2, () => {
          val out = ChunkBuf[A](take)
          taken.fill(out, take, size0)
          woken.foreach(_(true))
          k(Right(out.take(take)))
        })
      }
    }

  /** the single receive's transition, shared with the batched one */
  private def receiveOne(s: State[A])(k: End => Unit): (State[A], () => Unit) =
    if (s.buf.nonEmpty) {
      val (a, rest) = s.buf.dequeue
      if (s.senders.nonEmpty) {
        val ((b, sk), more) = s.senders.dequeue
        (s.copy(buf = rest.enqueue(b), senders = more), () => { sk(true); k(Right(Some(a))) })
      } else (s.copy(buf = rest, size = s.size - 1), () => k(Right(Some(a))))
    }
    else if (s.senders.nonEmpty) {
      val ((b, sk), more) = s.senders.dequeue
      (s.copy(senders = more), () => { sk(true); k(Right(Some(b))) })
    }
    else if (!s.open) { val e = endOf(s); (s, () => k(e)) }
    else (s.copy(receivers = s.receivers.enqueue(k)), () => ())

  def offer(a: A): Boolean = transact[Boolean] { s =>
    if (!s.open) (s, () => false)
    else if (s.receivers.nonEmpty) {
      val (r, rest) = s.receivers.dequeue
      (s.copy(receivers = rest), () => { r(Right(Some(a))); true })
    }
    else if (s.size < capacity) (s.copy(buf = s.buf.enqueue(a), size = s.size + 1), () => true)
    else (s, () => false)
  }

  /** end the stream: the buffered elements still drain, parked
   * senders' elements were accepted before the end and join the
   * buffer, waiting receivers hear the end at once */
  def close(): Unit = transact[Unit] { s =>
    val admitted = s.senders
    val buf = admitted.foldLeft(s.buf)((q, e) => q.enqueue(e._1))
    val woken = if (buf.isEmpty) s.receivers else Queue.empty[End => Unit]
    val s2 = s.copy(buf = buf, size = s.size + admitted.size, senders = Queue.empty,
      receivers = if (buf.isEmpty) Queue.empty else s.receivers, open = false)
    val e = endOf(s2)
    (s2, () => { admitted.foreach(_._2(true)); woken.foreach(_(e)) })
  }

  /** record that a producer broke: it does NOT close — in a merge the
   * other source is still feeding; the error is what the END then is */
  def fail(e: Throwable): Unit = transact[Unit] { s =>
    if (s.failure == null) (s.copy(failure = e), () => ()) else (s, () => ())
  }

  def failed: Option[Throwable] = Option(cell.get.failure)

  def isClosed: Boolean = !cell.get.open

  private[stream] def finished: Boolean = {
    val s = cell.get
    !s.open && s.buf.isEmpty && s.senders.isEmpty
  }
}

object StmChannel {
  /** the whole channel as ONE immutable value. Invariant: receivers
   * waiting => buf empty and no sender waiting; senders waiting => buf
   * full (or capacity 0) and no receiver waiting. At the top level, not
   * nested in the class, so the type test in `Cell` needs no outer
   * reference. */
  private[stream] final case class State[A](buf: Fifo[A], size: Int,
                                            receivers: Queue[Either[Throwable, Option[A]] => Unit],
                                            senders: Queue[(A, Accepted)],
                                            open: Boolean,
                                            failure: Throwable)
}

/**
 * A channel read in BATCHES, one element at a time: the carrier is
 * the channel plus whatever the last transaction took and how far the
 * consumer has got through it. The elements and their order are the
 * channel's; only the CAS is paid once per batch. Nothing waits for a
 * batch to fill.
 */
final case class Drain[A](c: Channel[A], held: Chunk[A], at: Int)

object Drain {
  /** how many elements one transaction may take */
  private[stream] val Batch = 64

  def apply[A](c: Channel[A]): Drain[A] = Drain(c, Chunks.emptyChunk[A], 0)

  implicit val stream: Stream[Drain, Async] = new Stream[Drain, Async] {
    def uncons[A](d: Drain[A]): Option[(A, Drain[A])] ! Async =
      if (d.at < d.held.length) pure(Some((d.held(d.at), d.copy(at = d.at + 1))))
      else d.c.receiveMany(Drain.Batch).map { got =>
        if (got.isEmpty) None else Some((got(0), Drain(d.c, got, 1)))
      }
  }
}

object Channel {

  /** a channel is an async stream of what it receives (linear: a
   * repeated uncons reads the NEXT element) */
  implicit val stream: Stream[Channel, Async] = new Stream[Channel, Async] {
    def uncons[A](c: Channel[A]): Option[(A, Channel[A])] ! Async = c.receive.map(_.map(a => (a, c)))
  }

  /** the largest ring worth allocating up front: 2^20 slots */
  private final val MaxRing = 1 << 20

  /** how many parts the default may grow into; parts open lazily */
  private final val Parts = 8

  /**
   * The default channel, chosen by the capacity asked for, as the Scala 3
   * core's (okay2 stage 29). Every choice keeps the SAME contract — every
   * law in `TestChannelLaws`, both tiers — so this is a performance
   * decision nobody can observe except in the timing:
   *  - a bounded capacity: `SentinelChannel` over `Growing` — a plain ring
   *    while one producer pushes, partitioned once a second appears (the
   *    Scala 3 core measured 1.10x worse at one producer, 4-23x better at
   *    two to sixteen). Its price is EXACT FIFO ACROSS PRODUCERS, and one
   *    displacement per producer across the swap;
   *    `Queues.strong[A].fifo(n)` is the total order by name;
   *  - past `MaxRing`: `SentinelChannel` over `Segments`, unbounded;
   *  - below two: `StmChannel`, a rendezvous the ring's stamps cannot
   *    express.
   */
  def apply[A](capacity: Int = Int.MaxValue): Channel[A] =
    if (capacity >= 2 && capacity <= MaxRing) new SentinelChannel[A](Queues.Mechanism.growing(capacity, Parts)())
    else if (capacity > MaxRing) new SentinelChannel[A](new Segments[Any]())
    else new StmChannel[A](capacity)

  /**
   * THE CHANNEL A SEAM BUILDS WHEN IT KNOWS ITS PRODUCERS (the Scala 3
   * core's channel-known-producers): `merge` has exactly two, `buffer`
   * exactly one, so neither needs `growing`'s guess or its swap — two
   * fixed parts for the merge (each producer's order exact by
   * construction), a plain ring for the buffer. `capacity` is per part.
   */
  private[stream] def forProducers[A](n: Int, capacity: Int): Channel[A] =
    if (capacity < 2) Channel[A](capacity)
    else if (n <= 1) { if (capacity <= MaxRing) new SentinelChannel[A](capacity) else Channel[A](capacity) }
    else if (capacity <= MaxRing) Queues.strong[A].relaxed.parts(n).each(capacity).build
    else Queues.strong[A].relaxed.parts(n).unbounded.build

  implicit final class ChannelOps[A](private val c: Channel[A]) extends AnyVal {
    /** the channel as a source that reads it in BATCHES */
    def drained: Source[A] = Writer.of[Drain, Async, A](Drain(c))

    /** the channel as a source of CHUNKS: each batch told as one chunk */
    def drainedChunks: Source[Chunk[A]] = {
      type R = Writer[Chunk[A]] + Async
      def go: Source[Chunk[A]] =
        Async.await[Chunk[A]](k => { c.receiveManyAsync(Drain.Batch)(k); () => () }).at[R].flatMap { got =>
          if (got.isEmpty) pure(())
          else Writer.tell(got).at[R].flatMap(_ => go)
        }
      pure[R, Unit](()).flatMap(_ => go)
    }
  }

  /**
   * Unfold a stream into the channel as an Async program; stops early
   * if the channel refuses. OFFER FIRST in a plain loop while the
   * channel takes, and park with ONE `send` only on the element it
   * refused. The loop lives INSIDE a program step, so a producer that
   * throws throws in its own fiber, not out of `Channel.buffer`.
   */
  private def feed[A, U[_], H <: Row](c: Channel[A], u: U[A])(implicit St: Stream[U, H], HH: Handler[H]): Unit ! Async = {
    val it = St.iterator(u)
    def go: Unit ! Async =
      pure[Async, Unit](()).flatMap { _ =>
        if (!it.hasNext) pure(())
        else {
          var a = it.next()
          var taken = c.offer(a)
          while (taken && it.hasNext) { a = it.next(); taken = c.offer(a) }
          if (taken) pure(())
          else c.send(a).flatMap(ok => if (ok) go else pure(()))
        }
      }
    go
  }

  /** the buffered producer's feed: accumulate into a LOCAL buffer and
   * send whole chunks — an element costs an array store, a chunk one send */
  private def feedBatched[A, U[_], H <: Row](c: Channel[Chunk[A]], u: U[A], size: Int)(implicit St: Stream[U, H], HH: Handler[H]): Unit ! Async = {
    val it = St.iterator(u)
    def go(buf: ChunkBuf[A], n: Int): Unit ! Async =
      if (!it.hasNext) { if (n == 0) pure(()) else c.send(buf.take(n)).map(_ => ()) }
      else {
        var i = n
        while (i < size && it.hasNext) { buf(i) = it.next(); i += 1 }
        if (i < size) c.send(buf.take(i)).map(_ => ())
        else c.send(buf.chunk).flatMap(ok => if (ok) go(ChunkBuf[A](size), 0) else pure(()))
      }
    go(ChunkBuf[A](size), 0)
  }

  /** the chunking feed whose buffer a FLUSHER may take concurrently: a
   * `Cell`, not a local, so the timer never touches the pull */
  private def feedChunked[A, U[_], H <: Row](c: Channel[Chunk[A]], u: U[A], size: Int, buf: Cell[Vector[A]])(implicit St: Stream[U, H], HH: Handler[H]): Unit ! Async = {
    def take(full: Boolean): Option[Chunk[A]] = takeChunk(buf, size, full)
    def go(x: U[A]): Unit ! Async =
      Async(Effects.runFree(St.uncons(x))).flatMap {
        case Some((a, r)) =>
          buf.modify(b => (b :+ a, () => ()))
          take(full = false) match {
            case Some(ch) => c.send(ch).flatMap(ok => if (ok) go(r) else pure(()))
            case None => go(r)
          }
        case None => take(full = true) match {
          case Some(ch) => c.send(ch).map(_ => ())
          case None => pure(())
        }
      }
    go(u)
  }

  /** take a chunk if one is due — `full` meaning "whatever is there" */
  private def takeChunk[A](buf: Cell[Vector[A]], size: Int, full: Boolean): Option[Chunk[A]] =
    buf.modify { b =>
      if (b.isEmpty || (!full && b.length < size)) (b, () => None)
      else (Vector.empty, () => Some(ChunkBuf.of(b)))
    }

  /**
   * Merge two streams as CHUNKS: one channel transaction per `size`
   * elements. `within` bounds how long a partial chunk may wait: the
   * flusher is a fiber per source that sleeps and then TAKES what has
   * accumulated; it never cancels or races the pull, and it is
   * cancelled when its source's feed completes.
   */
  private def chunkedMerge[A](capacity: Int, size: Int, within: Option[Long])
                             (feedS: (Channel[Chunk[A]], Cell[Vector[A]]) => Unit ! Async,
                              feedT: (Channel[Chunk[A]], Cell[Vector[A]]) => Unit ! Async)
                             (implicit sch: Scheduler, timer: Timer): Channel[Chunk[A]] = {
    val c = Channel[Chunk[A]](capacity)
    val alive = new AtomicInteger(2)
    def flusher(buf: Cell[Vector[A]], done: AtomicInteger): Fiber[Unit] = within match {
      case None => null
      case Some(ms) =>
        def tick(): Unit ! Async =
          Async.sleep(ms).flatMap { _ =>
            if (done.get > 0) takeChunk(buf, size, full = true) match {
              case Some(ch) => c.send(ch).flatMap(_ => tick())
              case None => tick()
            }
            else pure(())
          }
        sch.fork(() => tick())
    }
    def watch(f: Fiber[Unit], mine: AtomicInteger, fl: => Fiber[Unit]): Unit =
      f.onComplete { r =>
        mine.set(0)
        r.left.foreach(e => c.fail(e))
        val t = fl
        if (t != null) t.cancel()
        if (alive.decrementAndGet() == 0) c.close()
      }
    val bs = new Cell[Vector[A]](Vector.empty)
    val bt = new Cell[Vector[A]](Vector.empty)
    val ds = new AtomicInteger(1)
    val dt = new AtomicInteger(1)
    lazy val fs: Fiber[Unit] = flusher(bs, ds)
    lazy val ft: Fiber[Unit] = flusher(bt, dt)
    watch(sch.fork(() => feedS(c, bs)), ds, fs); val _ = fs
    watch(sch.fork(() => feedT(c, bt)), dt, ft); val _ = ft
    c
  }

  /** the chunking merge for ordinary sources */
  def mergeChunked[A, S[_], F <: Row, T[_], G <: Row](s: S[A], t: T[A], capacity: Int, size: Int, within: Option[Long])
                                                    (implicit SS: Stream[S, F], HF: Handler[F], ST: Stream[T, G], HG: Handler[G],
                                                     sch: Scheduler, timer: Timer): Channel[Chunk[A]] =
    chunkedMerge(capacity, size, within)((c, buf) => feedChunked(c, s, size, buf), (c, buf) => feedChunked(c, t, size, buf))

  /** merge two streams by READINESS, not by turns: a fiber per source
   * feeds one channel; it closes when both sources end; a source that
   * fails is recorded and the other still feeds */
  def merge[A, S[_], F <: Row, T[_], G <: Row](s: S[A], t: T[A], capacity: Int = Int.MaxValue)
                                             (implicit SS: Stream[S, F], HF: Handler[F], ST: Stream[T, G], HG: Handler[G],
                                              sch: Scheduler): Channel[A] = {
    val c = forProducers[A](2, capacity)
    val alive = new AtomicInteger(2)
    def watch(f: Fiber[Unit]): Unit = f.onComplete { r =>
      r.left.foreach(c.fail)
      if (alive.decrementAndGet() == 0) c.close()
    }
    watch(sch.fork(() => feed(c, s)))
    watch(sch.fork(() => feed(c, t)))
    c
  }

  /** `merge` of two sources: the writer instance in Async, the feed
   * parking under CanBlock */
  def mergeSources[A](s: Source[A], t: Source[A], capacity: Int)(implicit sch: Scheduler, cb: CanBlock): Channel[A] = {
    type L[W] = Unit ! (Writer[W] + Async)
    merge[A, L, Async, L, Async](s, t, capacity)(Stream.writerStreamIn[Unit, Async], Async.handler(cb), Stream.writerStreamIn[Unit, Async], Async.handler(cb), sch)
  }

  def mergeSourcesChunked[A](s: Source[A], t: Source[A], capacity: Int, size: Int, within: Option[Long])
                            (implicit sch: Scheduler, cb: CanBlock, timer: Timer): Channel[Chunk[A]] = {
    type L[W] = Unit ! (Writer[W] + Async)
    mergeChunked[A, L, Async, L, Async](s, t, capacity, size, within)(Stream.writerStreamIn[Unit, Async], Async.handler(cb), Stream.writerStreamIn[Unit, Async], Async.handler(cb), sch, timer)
  }

  /** the same as `buffer`, in CHUNKS: `capacity` counts chunks */
  def bufferChunked[A, S[_], F <: Row](capacity: Int, size: Int = Source.ChunkSize)(s: S[A])
                                     (implicit SS: Stream[S, F], HF: Handler[F], sch: Scheduler): Channel[Chunk[A]] = {
    val c = Channel[Chunk[A]](capacity)
    sch.fork(() => feedBatched(c, s, size)).onComplete { r =>
      r.left.foreach(c.fail)
      c.close()
    }
    c
  }

  /** run the producer ahead of the consumer, at most capacity elements
   * ahead: a fiber unfolds the stream into a bounded channel */
  def buffer[A, S[_], F <: Row](capacity: Int)(s: S[A])(implicit SS: Stream[S, F], HF: Handler[F], sch: Scheduler): Channel[A] = {
    val c = forProducers[A](1, capacity)
    sch.fork(() => feed(c, s)).onComplete { r =>
      r.left.foreach(c.fail)
      c.close()
    }
    c
  }
}

/**
 * The persistent FIFO a `StmChannel` keeps its buffer in. It replaces
 * `immutable.Queue` for one reason: `Queue` only lets you take ONE
 * element at a time, and a batched receive would build thousands of
 * intermediate queues inside one transaction. So the batched pair is
 * `drop` (what remains, decided inside the transaction) and `fill`
 * (what is taken, written in the action after the CAS won).
 */
trait Fifo[A] {
  def isEmpty: Boolean
  def nonEmpty: Boolean
  def enqueue(a: A): Fifo[A]
  def dequeue: (A, Fifo[A])
  def drop(n: Int, total: Int): Fifo[A]
  def fill(out: ChunkBuf[A], n: Int, total: Int): Unit
}

/** two lists: the front oldest-first, the back newest-first;
 * `List.reverse` never runs on the batched path */
final class ListFifo[A](val front: List[A], val back: List[A]) extends Fifo[A] {
  def isEmpty: Boolean = front.isEmpty && back.isEmpty
  def nonEmpty: Boolean = !isEmpty
  def enqueue(a: A): Fifo[A] = new ListFifo(front, a :: back)

  def dequeue: (A, Fifo[A]) =
    if (front.nonEmpty) (front.head, new ListFifo(front.tail, back))
    else { val f = back.reverse; (f.head, new ListFifo(f.tail, Nil)) }

  def drop(n: Int, total: Int): Fifo[A] =
    if (n >= total) new ListFifo(Nil, Nil)
    else if (n <= 0) this
    else {
      var k = n
      var f = front
      while (k > 0 && f.nonEmpty) { f = f.tail; k -= 1 }
      if (k == 0) new ListFifo(f, back)
      else {
        var lb = 0
        var b = back
        while (b.nonEmpty) { lb += 1; b = b.tail }
        var keep = List.empty[A]
        var i = 0
        var c = back
        while (i < lb - k) { keep = c.head :: keep; i += 1; c = c.tail }
        new ListFifo(keep, Nil)
      }
    }

  def fill(out: ChunkBuf[A], n: Int, total: Int): Unit = {
    var i = 0
    var f = front
    while (i < n && f.nonEmpty) { out(i) = f.head; i += 1; f = f.tail }
    if (i < n) {
      var b = back
      if (n < total) {
        var lb = 0
        var c = back
        while (c.nonEmpty) { lb += 1; c = c.tail }
        var skip = lb - (n - i)
        while (skip > 0) { b = b.tail; skip -= 1 }
      }
      var j = n - 1
      while (j >= i && b.nonEmpty) { out(j) = b.head; j -= 1; b = b.tail }
    }
  }
}

/** the front as an immutable array plus an index: turning the back
 * round allocates ONE array, `dequeue` is index arithmetic, and `drop`
 * within the front only moves the index */
final class ArrayFifo[A](val front: Chunk[A], val start: Int, val back: List[A]) extends Fifo[A] {
  def frontSize: Int = front.length - start
  def isEmpty: Boolean = frontSize <= 0 && back.isEmpty
  def nonEmpty: Boolean = !isEmpty
  def enqueue(a: A): Fifo[A] = new ArrayFifo(front, start, a :: back)

  private def turned(keep: Int): Chunk[A] = {
    val buf = ChunkBuf[A](keep)
    var j = keep - 1
    var c = back
    while (j >= 0) { buf(j) = c.head; j -= 1; c = c.tail }
    buf.chunk
  }

  private def backSize: Int = {
    var n = 0
    var b = back
    while (b.nonEmpty) { n += 1; b = b.tail }
    n
  }

  def dequeue: (A, Fifo[A]) =
    if (frontSize > 0) (front(start), new ArrayFifo(front, start + 1, back))
    else { val f = turned(backSize); (f(0), new ArrayFifo(f, 1, Nil)) }

  def drop(n: Int, total: Int): Fifo[A] =
    if (n >= total) new ArrayFifo(Chunks.emptyChunk[A], 0, Nil)
    else if (n <= 0) this
    else if (n <= frontSize) new ArrayFifo(front, start + n, back)
    else new ArrayFifo(turned(backSize - (n - frontSize)), 0, Nil)

  def fill(out: ChunkBuf[A], n: Int, total: Int): Unit = {
    var i = 0
    val fs = frontSize
    val m = if (n < fs) n else fs
    while (i < m) { out(i) = front(start + i); i += 1 }
    if (i < n) {
      var b = back
      if (n < total) {
        var skip = backSize - (n - i)
        while (skip > 0) { b = b.tail; skip -= 1 }
      }
      var j = n - 1
      while (j >= i && b.nonEmpty) { out(j) = b.head; j -= 1; b = b.tail }
    }
  }
}

object Fifo {
  def list[A]: Fifo[A] = new ListFifo(Nil, Nil)
  def array[A]: Fifo[A] = new ArrayFifo(Chunks.emptyChunk[A], 0, Nil)
}
