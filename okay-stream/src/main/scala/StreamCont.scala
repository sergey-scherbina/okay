package okay

import okay.AsyncCont.{awaitEither, fork, join}
import okay.cont.Free
import okay.std.Writer
import okay.freer.Row.plus
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.immutable.ArraySeq

/**
 * THE STREAM ON THE MACHINE, A TWIN (stream-twin): beside the classic `Source[W] = Unit ! Writer % W + Async`, a
 * stream of the machine's `A ! R` — to learn what streaming costs and looks like there before anything moves.
 *
 * A PULL, NOT A PUSH. The classic's stream is a program that TELLS (Writer), and a transformation is a handler
 * that tells again downstream. On the machine a handler's clause has no capability for the rest of the row, so a
 * handler cannot re-perform downstream; the machine's own shape is the pull: a stream is an Async program that
 * answers its first element and the rest, or that it ended (`Step`). Its transformations are functions over that
 * answer — no handler at all — and its effects are Async's alone. A stream is re-runnable when what it was built
 * from is (a `List`, a `range`; an `Iterator` is read once).
 *
 * A PULL ANSWERS A CHUNK (stream-chunks): a step is a run of elements and the rest — so the program's nodes (a
 * Step, a Src, a delay, a bind) are paid per chunk, not per element. The chunk is STRICT (stream-strict), an
 * `ArraySeq` as the classic's `Chunks`: `map` computes its chunk at once, and `take` never pulls the producer past
 * the chunk it needs; a producer whose elements are effects reads one per pull (`fromIterator`).
 */
object StreamCont:
  type R = Async +: Pure

  /** how many elements a range or a list answers per pull */
  final val ChunkSize = 256

  /** where a stream stands when pulled: ended, or a run of elements (a lazy view, never empty) and the rest */
  enum Step[+W]:
    case Done
    case Chunk(ws: ArraySeq[W], rest: Src[W])

  /** a stream: the program that pulls it */
  final class Src[+W](val pull: Free[R, Step[W]]):
    /** `inline`, as the classic `Chunks.map`: the chunk mapper is built ONCE where `V` may still be concrete
     * (`ChunkBuf.mapper`, a loop over the chunk, unboxed when a `ClassTag` is there) and handed to the recursion
     * as a value — so every chunk is mapped by it, not only the first */
    inline def map[V](inline f: W => V): Src[V] = mapWith(ChunkBuf.mapper[W, V](f))

    /** the recursion behind the inline `map`: public, as an inline method may reach no less accessible member */
    def mapWith[V](g: ArraySeq[W] => ArraySeq[V]): Src[V] = Src(pull.map {
      case Step.Done => Step.Done
      case Step.Chunk(ws, r) => Step.Chunk(g(ws), r.mapWith(g))
    })

    inline def filter(inline p: W => Boolean): Src[W] = filterWith[W](ChunkBuf.filterer[W](p))

    /** the recursion behind the inline `filter`; an empty chunk is not a step (a consumer would pull it for nothing) */
    def filterWith[V >: W](g: ArraySeq[V] => ArraySeq[V]): Src[V] = Src(pull.flatMap {
      case Step.Done => Free.pure(Step.Done)
      case Step.Chunk(ws, r) =>
        val kept = g(ws)
        if kept.nonEmpty then Free.pure(Step.Chunk(kept, r.filterWith(g))) else r.filterWith(g).pull
    })

    /** the first `n` elements; the producer is not pulled past the chunk that holds the last of them */
    def take(n: Int): Src[W] =
      if n <= 0 then StreamCont.empty
      else Src(pull.map {
        case Step.Done => Step.Done
        case Step.Chunk(ws, r) =>
          if ws.length >= n then Step.Chunk(ws.take(n), StreamCont.empty) else Step.Chunk(ws, r.take(n - ws.length))
      })

    /** this, then `that` */
    def ++[V >: W](that: => Src[V]): Src[V] = Src(pull.flatMap {
      case Step.Done => that.pull
      case Step.Chunk(ws, r) => Free.pure(Step.Chunk(ws, r ++ that))
    })

    /** each element's stream, in order */
    def flatMap[V](f: W => Src[V]): Src[V] = Src(pull.flatMap {
      case Step.Done => Free.pure(Step.Done)
      case Step.Chunk(ws, r) => (ws.foldRight(StreamCont.defer(r.flatMap(f)))((w, acc) => f(w) ++ acc)).pull
    })

    /** an Async step per element, in order */
    def evalMap[V](f: W => V ! R): Src[V] = Src(pull.flatMap {
      case Step.Done => Free.pure(Step.Done)
      case Step.Chunk(ws, r) =>
        def each(it: Iterator[W], acc: Vector[V]): Free[R, Vector[V]] =
          if !it.hasNext then Free.pure(acc) else f(it.next()).flatMap(v => each(it, acc :+ v))
        each(ws.iterator, Vector.empty).map(vs => Step.Chunk(ArraySeq.untagged.from(vs), r.evalMap(f)))
    })

    /** `inline`: the fold of a chunk is a loop at the call site, handed to the recursion as a value */
    inline def foldLeft[B](z: B)(inline f: (B, W) => B): B ! R =
      foldWith(z)((acc, c) =>
        var a = acc
        var i = 0
        val n = c.length
        while i < n do
          a = f(a, c(i))
          i += 1
        a)

    /** the recursion behind the inline `foldLeft` */
    def foldWith[B](z: B)(g: (B, ArraySeq[W]) => B): B ! R = pull.flatMap {
      case Step.Done => Free.pure(z)
      case Step.Chunk(ws, r) => r.foldWith(g(z, ws))(g)
    }

    def toVector: Vector[W] ! R = foldLeft(Vector.newBuilder[W])((b, w) => b += w).map(_.result())

    def runForeach(f: W => Unit ! R): Unit ! R = pull.flatMap {
      case Step.Done => Free.pure(())
      case Step.Chunk(ws, r) =>
        def each(it: Iterator[W]): Unit ! R = if !it.hasNext then r.runForeach(f) else f(it.next()).flatMap(_ => each(it))
        each(ws.iterator)
    }

    /** `eff` once the stream has ended (not if its consumer stopped before) */
    def onDone(eff: Unit ! R): Src[W] = Src(pull.flatMap {
      case Step.Done => eff.map(_ => Step.Done)
      case Step.Chunk(ws, r) => Free.pure(Step.Chunk(ws, r.onDone(eff)))
    })

    /**
     * both streams at once, each chunk as it comes: each side's next pull on its own fiber, the first answer
     * taken and its side pulled again, the other side's fiber kept. A CANCEL SCOPE holds the pulls still pending
     * (stream-merge-scope): it is exited when both sides have ended, and a consumer that stops first (`take`)
     * ends its program with the scope open — the drive releases it, and the pending pulls are cancelled
     * (`AsyncCont.runAsync`'s scopes; under the blocking handler, as on the classic's, the marks are empty)
     */
    def merge[V >: W](that: Src[V])(using Scheduler): Src[V] = Src(Free.delay { () =>
      val live = Live()
      val scope = Async.CancelScope(() => live.cancelAll())
      AsyncCont.enter(scope).flatMap(_ =>
        fork[Step[V]](this.pull).flatMap(fa =>
          fork[Step[V]](that.pull).flatMap { fb =>
            live.add(fa); live.add(fb)
            StreamCont.merging[V](fa, fb, live, scope).pull
          }))
    })

    /** pumped into `c`, an element a send (waiting while `c` is full); `c` closed at the end, failed by a failure */
    def toChannel[V >: W](c: Channel[V]): Unit ! R =
      AsyncCont.attempt(runForeach(w => StreamCont.send(c, w).at[R].map(_ => ()))).flatMap {
        case Right(()) => AsyncCont.async(c.close()).at
        case Left(e) => AsyncCont.async(c.fail(e)).at
      }

    /**
     * read through a channel of `capacity`: a producer fiber pumps this stream into it while the consumer reads
     * chunks out (`receiveManyAsync`). A consumer that stops first leaves a cancel scope open, and the drive
     * cancels the pump and closes the channel, as `merge`'s pending pulls are
     */
    def buffer(capacity: Int)(using Scheduler): Src[W] = Src(Free.delay { () =>
      val c = Channel[W](capacity)
      val live = Live()
      val scope = Async.CancelScope(() => { live.cancelAll(); c.close() })
      AsyncCont.enter(scope).flatMap(_ =>
        fork(toChannel(c)).flatMap { pump =>
          live.add(pump)
          StreamCont.fromChannel(c).onDone(AsyncCont.exit(scope).at).pull
        })
    })

  /** the pulls a merge still has running: one per run of the merged stream */
  private final class Live:
    private var fibers: List[Fiber[?]] = Nil
    def add(f: Fiber[?]): Unit = synchronized { fibers = f :: fibers }
    def remove(f: Fiber[?]): Unit = synchronized { fibers = fibers.filterNot(_ eq f) }
    def cancelAll(): Unit = synchronized { fibers }.foreach(_.cancel())

  private def merging[W](fa: Fiber[Step[W]], fb: Fiber[Step[W]], live: Live, scope: Async.CancelScope)
                        (using Scheduler): Src[W] =
    Src(first(fa, fb).flatMap { (step, won, other) =>
      live.remove(won)
      step match
        // one side has ended: the other goes on alone, joined, and the scope is exited when it ends too
        case Step.Done => join(other).flatMap { s =>
          live.remove(other)
          s match
            case Step.Done => AsyncCont.exit(scope).map(_ => Step.Done)
            case Step.Chunk(ws, r) => Free.pure(Step.Chunk(ws, r.onDone(AsyncCont.exit(scope).at)))
        }
        case Step.Chunk(ws, r) =>
          Free.pure(Step.Chunk(ws, Src(fork(r.pull).flatMap { again =>
            live.add(again)
            merging(again, other, live, scope).pull
          })))
    })

  /** which of two fibers answers first: its answer, the winner, and the other fiber */
  private def first[W](fa: Fiber[Step[W]], fb: Fiber[Step[W]]): (Step[W], Fiber[Step[W]], Fiber[Step[W]]) ! R =
    awaitEither[(Step[W], Fiber[Step[W]], Fiber[Step[W]])] { k =>
      val won = AtomicBoolean(false)
      fa.onComplete(r => if !won.getAndSet(true) then k(r.map(s => (s, fa, fb))))
      fb.onComplete(r => if !won.getAndSet(true) then k(r.map(s => (s, fb, fa))))
      () => ()
    }.at

  def empty[W]: Src[W] = Src(Free.pure(Step.Done))

  /** pulled again only when its rest is pulled: a tail call between mutually recursive streams costs no frame */
  def defer[W](s: => Src[W]): Src[W] = Src(Free.delay(() => s.pull))

  def emit[W](w: W): Src[W] = Src(Free.pure(Step.Chunk(ArraySeq.untagged(w), empty)))

  def apply[W](ws: W*): Src[W] = fromList(ws.toList)

  def fromList[W](ws: List[W]): Src[W] =
    if ws.isEmpty then empty
    else
      val (now, later) = ws.splitAt(ChunkSize)
      Src(Free.pure(Step.Chunk(ArraySeq.untagged.from(now), defer(fromList(later)))))

  /** read once, one element a pull: the iterator is made when the stream is first pulled, and nothing is read
   * ahead of what is pulled (its elements may be effects of their own) */
  def fromIterator[W](it: => Iterator[W]): Src[W] = Src(Free.delay(() => reading(it).pull))
  private def reading[W](it: Iterator[W]): Src[W] =
    Src(Free.delay(() =>
      if it.hasNext then Free.pure(Step.Chunk(ArraySeq.untagged(it.next()), reading(it))) else Free.pure(Step.Done)))

  def range(from: Long, until: Long): Src[Long] =
    if from >= until then empty
    else
      val n = math.min(ChunkSize.toLong, until - from).toInt
      // a long[] filled by a loop, as `Chunks.range`: `ArraySeq.range` is generic over `Integral` and boxed every
      // element it built (stream-range: 25 B an element more than the classic's, measured by -prof gc)
      val arr = new Array[Long](n)
      var i = 0
      while i < n do
        arr(i) = from + i
        i += 1
      Src(Free.pure(Step.Chunk(ArraySeq.unsafeWrapArray(arr), defer(range(from + n, until)))))

  /** one Async step as a stream of its answer */
  def eval[W](op: W ! R): Src[W] = Src(op.map(w => Step.Chunk(ArraySeq.untagged(w), empty)))

  // ---- channels (stream-channels): the classic Channel is backend-neutral at its callbacks

  /** send as an operation of the machine: waits while the channel is full; false if it is closed */
  def send[A](c: Channel[A], a: A): Op[Async, Boolean] = awaitEither[Boolean] { k =>
    val cb: Accepted = b => k(Right(b))
    c.sendAsync(a)(cb)
    () => c.cancelSend(cb)
  }

  /** receive as an operation of the machine: None once the channel is closed and drained */
  def receive[A](c: Channel[A]): Op[Async, Option[A]] = awaitEither[Option[A]] { k =>
    val cb: Either[Throwable, Option[A]] => Unit = k
    c.receiveAsync(cb)
    () => c.cancelReceive(cb)
  }

  /** a channel read as a stream: a chunk of what is buffered per pull (up to `ChunkSize`), ended when the channel
   * is closed and drained, failed by its failure */
  def fromChannel[A](c: Channel[A]): Src[A] =
    Src(awaitEither[ArraySeq[A]] { k => c.receiveManyAsync(ChunkSize)(k); () => () }.at[R].map { ch =>
      if ch.isEmpty then Step.Done else Step.Chunk(ch, fromChannel(c))
    })

  // ---- bridges to the classic Source, per element

  /** the classic stream pulled from the machine: `Writer.uncons` per element, each one Await of the machine */
  def fromSource[W](s: Source[W]): Src[W] =
    Src(AsyncCont.fromClassic(Writer.uncons[W, Unit, Async](s)).at[R].map {
      case Left(()) => Step.Done
      case Right((w, rest)) => Step.Chunk(ArraySeq.untagged(w), fromSource(rest))
    })

  /** the machine's stream as a classic one: each pull a classic program (`toClassic`), each element told */
  def toSource[W](s: Src[W]): Source[W] =
    AsyncCont.toClassic(s.pull).plus[okay.freer.%[Writer, W]].flatMap {
      case Step.Done => okay.freer.pure(())
      case Step.Chunk(ws, r) =>
        def tells(it: Iterator[W]): Source[W] =
          if !it.hasNext then toSource(r) else Writer.tell(it.next()).plus[Async].flatMap(_ => tells(it))
        tells(ws.iterator)
    }
