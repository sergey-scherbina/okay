package okay

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.Queue
import okay.!.*

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
  def receiveAsync(k: End => Unit): Unit = transact[Unit](receiveOne(_)(k))

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

  /**
   * Take up to `max` elements that are ALREADY buffered, in ONE
   * transaction — the receive side's answer to what chunking does on
   * the send side, and without its price.
   *
   * Profiling put 71% of the per-element merge inside the channel
   * transaction (CAS 33%, the immutable queues 19%, `resume`'s
   * rotation 19%), and four lanes established that the transaction
   * cannot be made cheaper — only rarer. Chunking makes it rarer by
   * batching SENDS, which delays an element that could have gone now,
   * which is why it is opt-in. Batching RECEIVES delays nothing: what
   * is already in the buffer is already late, and taking ten of them
   * under one CAS instead of ten hands the consumer exactly the same
   * elements in exactly the same order. So this needs no flag.
   *
   * An empty buffer falls back to the single receive, parking as
   * before: `max` is a ceiling on what may be taken, never a quota to
   * wait for. Parked senders are admitted into the room this frees,
   * as the single receive does, and their callbacks fire after the
   * CAS like every other action here.
   */
  private[okay] def receiveManyAsync(max: Int)(k: Either[Throwable, Chunk[A]] => Unit): Unit =
    transact[Unit] { s =>
      if s.buf.isEmpty then
        // nothing buffered: exactly the single receive, its one
        // element handed over as a chunk of one
        receiveOne(s)(e => k(e.map(_.fold(Chunks.emptyChunk[A])(a => ChunkBuf.of(Seq(a))))))
      else
        val take = math.min(max, s.size)
        val out = ChunkBuf[A](take)
        var b = s.buf
        var senders = s.senders
        var n = s.size
        var woken = Vector.empty[Boolean => Unit]
        var i = 0
        while i < take && b.nonEmpty do
          val (a, rest) = b.dequeue
          out.update(i, a)
          i += 1
          if senders.nonEmpty then
            // room opened: admit the first parked sender, as the
            // single receive does — the size does not move
            val ((sa, sk), more) = senders.dequeue
            b = rest.enqueue(sa); senders = more; woken = woken :+ sk
          else { b = rest; n -= 1 }
        val s2 = s.copy(buf = b, size = n, senders = senders)
        (s2, () => { woken.foreach(_(true)); k(Right(out.take(i))) })
    }

  /** the single receive's transition, shared with the batched one */
  private def receiveOne(s: State)(k: End => Unit): (State, () => Unit) =
    if s.buf.nonEmpty then
      val (a, rest) = s.buf.dequeue
      if s.senders.nonEmpty then
        val ((b, sk), more) = s.senders.dequeue
        (s.copy(buf = rest.enqueue(b), senders = more), () => { sk(true); k(Right(Some(a))) })
      else (s.copy(buf = rest, size = s.size - 1), () => k(Right(Some(a))))
    else if s.senders.nonEmpty then
      val ((b, sk), more) = s.senders.dequeue
      (s.copy(senders = more), () => { sk(true); k(Right(Some(b))) })
    else if !s.open then
      val e = endOf(s)
      (s, () => k(e))
    else (s.copy(receivers = s.receivers.enqueue(k)), () => ())

  /** up to `max` buffered elements as a program; an empty answer is
   * the end of the stream */
  private[okay] def receiveMany(max: Int): Chunk[A] ! Async =
    Async.await { k =>
      receiveManyAsync(max)(k)
      () => ()
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
/**
 * A channel read in BATCHES, one element at a time.
 *
 * The carrier is the channel plus whatever the last transaction took
 * and how far the consumer has got through it, so `uncons` answers
 * from memory until the batch runs out and only then touches the
 * channel. The elements, and their order, are exactly the channel's;
 * the only difference is how often the CAS is paid — which profiling
 * made the whole cost of the per-element path (channel-drain).
 *
 * Nothing waits for a batch to fill: `receiveMany` takes what is
 * ALREADY buffered and falls back to a single parking receive when
 * nothing is. So this is not chunking's trade, and needs no flag.
 */
final case class Drain[A](c: Channel[A], held: Chunk[A], at: Int)

object Drain:
  /** how many elements one transaction may take. Large enough to
   * amortise the CAS, small enough that a batch is cheap to build */
  private[okay] inline val Batch = 64

  def apply[A](c: Channel[A]): Drain[A] = Drain(c, Chunks.emptyChunk[A], 0)

given Stream[Drain, Async] with
  def uncons[A](d: Drain[A]): Option[(A, Drain[A])] ! Async =
    if d.at < d.held.length then pure(Some((d.held(d.at), d.copy(at = d.at + 1))))
    else d.c.receiveMany(Drain.Batch).map: got =>
      if got.isEmpty then None else Some((got(0), Drain(d.c, got, 1)))

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
   * The chunking feed: accumulate up to `size` elements and send them
   * as one chunk, so the channel runs one transaction per chunk
   * rather than per element (source-merge-chunked: 71% of the
   * elementwise merge's CPU is that transaction).
   *
   * The buffer is a `TRef` rather than a local, because a FLUSHER may
   * take it concurrently — and that is the whole design constraint
   * here. The obvious way to bound a partial chunk's wait is to race
   * the pull against a timer, and it is wrong: `Async.timeout`
   * cancels the loser, and cancelling an in-flight `uncons` on a live
   * source can lose the element it was about to yield. So the timer
   * never touches the pull. It runs beside it and takes whatever has
   * accumulated, which is safe whatever the pull is doing.
   */
  private def feedChunked[A, U[_], H[+_]](c: Channel[Chunk[A]], u: U[A], size: Int,
                                          buf: TRef[ChunkBuffer[A]])
                                         (using St: Stream[U, H], HH: Handler[H]): Unit ! Async =
    def take(full: Boolean): Option[Chunk[A]] = takeChunk(buf, size, full)
    def go(x: U[A]): Unit ! Async =
      async(St.uncons(x).runWith).flatMap:
        case Some((a, r)) =>
          buf.modify(b => (ChunkBuffer(b.pending :+ a), ()))
          take(full = false) match
            case Some(ch) => c.send(ch).flatMap(ok => if ok then go(r) else pure(()))
            case None => go(r)
        // the source ended: whatever is left is a chunk, however short
        case None => take(full = true) match
          case Some(ch) => c.send(ch).map(_ => ())
          case None => pure(())
    go(u)

  /**
   * The chunking feed for a source that marks its OWN boundaries. It
   * walks the program instead of pulling through `Stream.uncons`,
   * because `Flush` has to be interpreted where it occurs — at the
   * exact point in the told sequence the producer put it.
   *
   * `relay` cannot serve: its handler answers an operation with a
   * VALUE, while `Flush.Now` must become a channel send, an Async
   * program that suspends. So this is the walk `Writer.widen` and
   * `Generate.uncons` are written in — resume, split the row,
   * rebuild — with the accumulation in between.
   *
   * It is a SECOND walk rather than the only one, and that was
   * measured rather than assumed: routing the ordinary chunked merge
   * through here too (widening every source into the flushing row,
   * paying one more tree rebuild per source and one more row split
   * per element) cost 11% on the common path — 244.3us +/-15.2
   * against 219.6 +/-1.5 in the same window. `Flush` is opt-in and
   * rare; plain chunking is not, so the duplication is the accumulation
   * helpers shared and the walk written twice (flush-op, 2026-09-03).
   */
  private def feedFlushing[A](c: Channel[Chunk[A]], p: Flushing[A], size: Int,
                              buf: TRef[ChunkBuffer[A]]): Unit ! Async =
    def sendIf(o: Option[Chunk[A]])(rest: => Unit ! Async): Unit ! Async = o match
      case Some(ch) => c.send(ch).flatMap(ok => if ok then rest else okay.pure(()))
      case None => rest
    def step[X](e: Flush[X] | (Writer % A + Async)[X], k: X => Flushing[A]): Unit ! Async =
      <|>[Flush, Writer % A + Async](e) match
        // the producer's own boundary: emit what is held, however short
        case Left(Flush.Now) => sendIf(takeChunk(buf, size, full = true))(go(k(())))
        case Right(rest) => <|>[Async, Writer % A](rest) match
          case Left(a) => Effect(a).flatMap(x => go(k(x)))
          case Right(Writer.Say(w)) =>
            buf.modify(b => (ChunkBuffer(b.pending :+ w), ()))
            sendIf(takeChunk(buf, size, full = false))(go(k(())))
    def go(p: Flushing[A]): Unit ! Async = (p.resume: @unchecked) match
      case Pure(_) => sendIf(takeChunk(buf, size, full = true))(okay.pure(()))
      case Effect(e) => step(e, _ => okay.pure(()))
      case Bind(Effect(e), k) => step(e, k)
    go(p)

  /** what both chunking feeds do to the buffer: take a chunk if one
   * is due — `full` meaning "whatever is there, the input is over or
   * the producer said so" */
  private def takeChunk[A](buf: TRef[ChunkBuffer[A]], size: Int, full: Boolean)
  : Option[Chunk[A]] = buf.modify: b =>
    if b.pending.isEmpty || (!full && b.pending.length < size)
    then (b, None)
    else (ChunkBuffer(Vector.empty), Some(ChunkBuf.ofSpecialized(b.pending)))

  /** the same merge, for sources that mark their own boundaries */
  def mergeFlushing[A](s: Flushing[A], t: Flushing[A], capacity: Int, size: Int,
                       within: Option[Long])
                      (using sch: Scheduler, timer: Timer): Channel[Chunk[A]] =
    chunkedMerge(capacity, size, within)(
      (c, buf) => feedFlushing(c, s, size, buf), (c, buf) => feedFlushing(c, t, size, buf))

  /** the buffer a chunking feed accumulates into, in a cell of its
   * own so a flusher can take it without racing the pull */
  private[okay] final case class ChunkBuffer[A](pending: Vector[A])
    extends TRef.Stamped[ChunkBuffer[A]]:
    def value: ChunkBuffer[A] = this

  /**
   * Merge two streams as CHUNKS: the same readiness merge, one
   * channel transaction per `size` elements. `within` bounds how long
   * a partial chunk may wait — without it a chunk is emitted only
   * when full or when its source ends, which on a slow or unending
   * source means an element can wait indefinitely.
   *
   * The flusher is a fiber per source that sleeps and then TAKES what
   * has accumulated; it never cancels or races the pull (see
   * `feedChunked`). It stops when its source's feed completes.
   */
  private def chunkedMerge[A](capacity: Int, size: Int, within: Option[Long])
                             (feedS: (Channel[Chunk[A]], TRef[ChunkBuffer[A]]) => Unit ! Async,
                              feedT: (Channel[Chunk[A]], TRef[ChunkBuffer[A]]) => Unit ! Async)
                             (using sch: Scheduler, timer: Timer): Channel[Chunk[A]] =
    val c = Channel[Chunk[A]](capacity)
    val alive = AtomicInteger(2)
    def flusher(buf: TRef[ChunkBuffer[A]], done: AtomicInteger): Unit = within.foreach: ms =>
      def tick(): Unit ! Async =
        Async.sleep(ms).flatMap: _ =>
          if done.get > 0 then
            takeChunk(buf, size, full = true) match
              case Some(ch) => c.send(ch).flatMap(_ => tick())
              case None => tick()
          else pure(())
      val _ = sch.fork(() => tick())
    def watch(f: Fiber[Unit], mine: AtomicInteger): Unit = f.onComplete { r =>
      mine.set(0)
      r.left.foreach(e => c.fail(e))
      if alive.decrementAndGet() == 0 then c.close()
    }
    val (bs, bt) = (TRef.bare(ChunkBuffer[A](Vector.empty)), TRef.bare(ChunkBuffer[A](Vector.empty)))
    val (ds, dt) = (AtomicInteger(1), AtomicInteger(1))
    watch(sch.fork(() => feedS(c, bs)), ds); flusher(bs, ds)
    watch(sch.fork(() => feedT(c, bt)), dt); flusher(bt, dt)
    c

  /** the chunking merge for ordinary sources: the common path, fed
   * through `feedChunked` rather than the flushing walk because that
   * routing measured 11% dearer (see `feedFlushing`) */
  def mergeChunked[A, S[_], F[+_], T[_], G[+_]](s: S[A], t: T[A], capacity: Int, size: Int,
                                                within: Option[Long])
                                               (using Stream[S, F], Handler[F],
                                                Stream[T, G], Handler[G])
                                               (using Scheduler, Timer): Channel[Chunk[A]] =
    chunkedMerge(capacity, size, within)(
      (c, buf) => feedChunked(c, s, size, buf), (c, buf) => feedChunked(c, t, size, buf))

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
