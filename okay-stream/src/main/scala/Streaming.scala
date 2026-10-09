package okay

import okay.std.Writer
import okay.freer.Row.plus
import okay.std.given
import scala.concurrent.Future

/**
 * STREAMS, ONE FRONT, TWO BACKENDS (stream-twin; the operator: "если имплементация стримов очень зависит от
 * бекенда — две имплементации и прозрачный фронтенд"). The classic's stream is a PUSH — a Writer program that
 * tells its elements, walked by its consumer; the machine's is a PULL — an Async program that answers the next
 * element and the rest (`StreamCont`). The words a stream is written in are members of `Streaming[S]`, as the
 * words of a program are members of `Effects[M]`, and the backend is the import's choice:
 * `import okay.streams.machine.{*, given}` or `import okay.streams.classic.{*, given}`; code written over
 * `Streaming[S]` runs on either. `P[A]` is the backend's own Async program — what a fold answers and what
 * `evalMap` takes — and `runAsync` runs it.
 */
trait Streaming[S[_]]:
  /** the backend's Async program */
  type P[A]

  def empty[W]: S[W]
  def emit[W](w: W): S[W]
  def fromList[W](ws: List[W]): S[W]
  def range(from: Long, until: Long): S[Long]
  /** one Async step as a stream of its answer */
  def eval[W](p: P[W]): S[W]
  /** a (possibly blocking) computation as a program of the backend */
  def async[A](a: => A): P[A]
  def runAsync[A](p: P[A]): Future[A]

  // the primitives the words below are written in, once for every backend (stream-joins)
  def pureP[A](a: A): P[A]
  extension [A](p: P[A])
    def flatMapP[B](f: A => P[B]): P[B]
    def mapP[B](f: A => B): P[B] = p.flatMapP(a => pureP(f(a)))
  /** the next chunk (never empty) and the rest, or the end */
  def unconsChunk[W](s: S[W]): P[Option[(IndexedSeq[W], S[W])]]
  /** a stream unfolded from `z` by an effectful step: each step tells a batch (maybe empty) and the next state */
  def unfoldP[Z, W](z: Z)(f: Z => P[Option[(Seq[W], Z)]]): S[W]

  extension [W](s: S[W])
    /** pairs, in lockstep; as long as the shorter */
    def zip[V](t: S[V]): S[(W, V)] = StreamingOps.zip(this)(s, t)
    def zipWith[V, U](t: S[V])(f: (W, V) => U): S[U] = s.zip(t).map(f.tupled)
    /** event-time windows (`Windows`, its panes), one engine per run of the stream */
    def windowed[K, Acc, O](w: => Windows[K, W, Acc, O]): S[Pane[K, O]] = StreamingOps.windowed(this)(s, () => w)

  extension [K, A](l: S[(K, A)])
    /** the sort-merge joins (`SortMerge`): both sides non-decreasing in key, checked */
    def joinSorted[B](r: S[(K, B)])(using Ordering[K]): S[(K, (A, B))] =
      StreamingOps.join(this)(l, r, () => SortMerge.inner[K, A, B])
    def leftJoinSorted[B](r: S[(K, B)])(using Ordering[K]): S[(K, (A, Option[B]))] =
      StreamingOps.join(this)(l, r, () => SortMerge.left[K, A, B])
    def fullJoinSorted[B](r: S[(K, B)])(using Ordering[K]): S[(K, (Option[A], Option[B]))] =
      StreamingOps.join(this)(l, r, () => SortMerge.full[K, A, B])

  extension [W](s: S[W])
    def map[V](f: W => V): S[V]
    def filter(p: W => Boolean): S[W]
    /** the first `n`; the producer is not resumed past them */
    def take(n: Int): S[W]
    def ++(t: => S[W]): S[W]
    def flatMap[V](f: W => S[V]): S[V]
    def evalMap[V](f: W => P[V]): S[V]
    def foldLeft[B](z: B)(f: (B, W) => B): P[B]
    def toVector: P[Vector[W]]
    /** both at once, each element as it comes; each side keeps its own order */
    def merge(t: S[W])(using Scheduler): S[W]
    /** read through a channel of `capacity`, filled by a producer fiber: the producer runs ahead by up to it */
    def buffer(capacity: Int)(using Scheduler): S[W]

object streams:

  /** the machine's: a pull over the machine's Async (`StreamCont`), behind a type of its own so that its instance
   * is found where the type is, as the classic's (stream-merge-scope) */
  object machine:
    opaque type Flow[W] = StreamCont.Src[W]
    given instance: Streaming[Flow] with
      type P[A] = A ! StreamCont.R
      def empty[W]: Flow[W] = StreamCont.empty
      def emit[W](w: W): Flow[W] = StreamCont.emit(w)
      def fromList[W](ws: List[W]): Flow[W] = StreamCont.fromList(ws)
      def range(from: Long, until: Long): Flow[Long] = StreamCont.range(from, until)
      def eval[W](p: P[W]): Flow[W] = StreamCont.eval(p)
      def async[A](a: => A): P[A] = AsyncCont.async(a).at
      def runAsync[A](p: P[A]): Future[A] = AsyncCont.runAsync(p)
      def pureP[A](a: A): P[A] = okay.cont.Free.pure(a)
      extension [A](p: P[A]) def flatMapP[B](f: A => P[B]): P[B] = p.flatMap(f)
      def unconsChunk[W](s: Flow[W]): P[Option[(IndexedSeq[W], Flow[W])]] = s.pull.map {
        case StreamCont.Step.Done => None
        case StreamCont.Step.Chunk(ws, r) => Some((ws, r))
      }
      def unfoldP[Z, W](z: Z)(f: Z => P[Option[(Seq[W], Z)]]): Flow[W] = StreamCont.unfold(z)(f)
      extension [W](s: Flow[W])
        def map[V](f: W => V): Flow[V] = s.map(f)
        def filter(p: W => Boolean): Flow[W] = s.filter(p)
        def take(n: Int): Flow[W] = s.take(n)
        def ++(t: => Flow[W]): Flow[W] = s ++ t
        def flatMap[V](f: W => Flow[V]): Flow[V] = s.flatMap(f)
        def evalMap[V](f: W => P[V]): Flow[V] = s.evalMap(f)
        def foldLeft[B](z: B)(f: (B, W) => B): P[B] = s.foldLeft(z)(f)
        def toVector: P[Vector[W]] = s.toVector
        def merge(t: Flow[W])(using Scheduler): Flow[W] = s.merge(t)
        def buffer(capacity: Int)(using Scheduler): Flow[W] = s.buffer(capacity)
    def fromSource[W](s: Source[W]): Flow[W] = StreamCont.fromSource(s)
    def fromSrc[W](s: StreamCont.Src[W]): Flow[W] = s
    extension [W](f: Flow[W])
      def toSource: Source[W] = StreamCont.toSource(f)
      def toSrc: StreamCont.Src[W] = f

  /** the classic's: a Writer program, `Source[W]`, behind a type of its own so that its words are the stream's
   * and not the program's (`Source` is a program: its own `map` maps the program's answer) */
  object classic:
    opaque type Flow[W] = Source[W]
    def fromSource[W](s: Source[W]): Flow[W] = s
    extension [W](f: Flow[W]) def toSource: Source[W] = f

    private def tell[W](w: W): Source[W] = Writer.tell(w).plus[Async]
    private def done[W]: Source[W] = okay.freer.pure(())
    private def unconsed[W](s: Source[W]): okay.freer.![Either[Unit, (W, Source[W])], Async] =
      Writer.uncons[W, Unit, Async](s)
    private def walk[W, V](s: Source[W])(step: (W, Source[W]) => Source[V]): Source[V] =
      unconsed(s).plus[okay.freer.%[Writer, V]].flatMap {
        case Left(()) => done[V]
        case Right((w, rest)) => step(w, rest)
      }

    given instance: Streaming[Flow] with
      type P[A] = okay.freer.![A, Async]
      def empty[W]: Flow[W] = done[W]
      def emit[W](w: W): Flow[W] = tell(w)
      def fromList[W](ws: List[W]): Flow[W] = Source.of(ws)
      def range(from: Long, until: Long): Flow[Long] = Source.range(from, until)
      def eval[W](p: P[W]): Flow[W] = p.plus[okay.freer.%[Writer, W]].flatMap(w => tell(w))
      def async[A](a: => A): P[A] = okay.async(a)
      def runAsync[A](p: P[A]): Future[A] = Async.runAsync(p)
      def pureP[A](a: A): P[A] = okay.freer.pure(a)
      extension [A](p: P[A]) def flatMapP[B](f: A => P[B]): P[B] = p.flatMap(f)
      def unconsChunk[W](s: Flow[W]): P[Option[(IndexedSeq[W], Flow[W])]] = unconsed(s).map {
        case Left(()) => None
        case Right((w, rest)) => Some((Vector(w), rest))
      }
      def unfoldP[Z, W](z: Z)(f: Z => P[Option[(Seq[W], Z)]]): Flow[W] =
        def tells(ws: List[W], next: => Source[W]): Source[W] = ws match
          case Nil => next
          case w :: more => tell(w).flatMap(_ => tells(more, next))
        def go(z: Z): Source[W] = okay.freer.Free.delay { () =>
          f(z).plus[okay.freer.%[Writer, W]].flatMap {
            case None => done[W]
            case Some((ws, z2)) => tells(ws.toList, go(z2))
          }
        }
        go(z)
      extension [W](s: Flow[W])
        def map[V](f: W => V): Flow[V] = Writer.map[W, V, Unit, Async](s)(f)
        def filter(p: W => Boolean): Flow[W] = walk(s)((w, rest) =>
          if p(w) then tell(w).flatMap(_ => (rest: Flow[W]).filter(p)) else (rest: Flow[W]).filter(p))
        def take(n: Int): Flow[W] =
          if n <= 0 then done[W] else walk(s)((w, rest) => tell(w).flatMap(_ => (rest: Flow[W]).take(n - 1)))
        def ++(t: => Flow[W]): Flow[W] = (s: Source[W]).flatMap(_ => t)
        def flatMap[V](f: W => Flow[V]): Flow[V] = walk(s)((w, rest) => (f(w): Source[V]).flatMap(_ => (rest: Flow[W]).flatMap(f)))
        def evalMap[V](f: W => P[V]): Flow[V] = walk(s)((w, rest) =>
          f(w).plus[okay.freer.%[Writer, V]].flatMap(v => tell(v).flatMap(_ => (rest: Flow[W]).evalMap(f))))
        def foldLeft[B](z: B)(f: (B, W) => B): P[B] = unconsed(s).flatMap {
          case Left(()) => okay.freer.pure(z)
          case Right((w, rest)) => (rest: Flow[W]).foldLeft(f(z, w))(f)
        }
        def toVector: P[Vector[W]] = (s: Source[W]).runCollect
        def merge(t: Flow[W])(using Scheduler): Flow[W] = (s: Source[W]).mergeReady(t)
        def buffer(capacity: Int)(using sch: Scheduler): Flow[W] = okay.freer.Free.delay { () =>
          val c = Channel[W](capacity)
          sch.fork(() => (s: Source[W]).runForeach(w => c.send(w).map(_ => ()))).onComplete { r =>
            r.left.foreach(c.fail)
            c.close()
          }
          c.drained
        }

/** the words of `Streaming` written once, over its primitives: a chunk walked by a cursor (O(1) an element), the
 * engines (`SortMerge`, `Windows`) made fresh for each run of the stream */
object StreamingOps:
  import scala.collection.mutable.ArrayBuffer

  final class Cursor[S[_], W](val chunk: IndexedSeq[W], val i: Int, val rest: S[W])
  private def start[S[_], W](s: S[W]): Cursor[S, W] = Cursor(IndexedSeq.empty, 0, s)

  /** the next element and the cursor after it, or the end */
  def next[S[_], W](St: Streaming[S])(c: Cursor[S, W]): St.P[Option[(W, Cursor[S, W])]] =
    import St.*
    if c.i < c.chunk.length then pureP(Some((c.chunk(c.i), Cursor(c.chunk, c.i + 1, c.rest))))
    else unconsChunk(c.rest).flatMapP {
      case None => pureP(None)
      case Some((ch, r)) => next(St)(Cursor(ch, 0, r))
    }

  def zip[S[_], W, V](St: Streaming[S])(s: S[W], t: S[V]): S[(W, V)] =
    import St.*
    unfoldP[(Cursor[S, W], Cursor[S, V]), (W, V)]((start(s), start(t))) { (a, b) =>
      next(St)(a).flatMapP {
        case None => pureP(None)
        case Some((w, a2)) => next(St)(b).mapP {
          case None => None
          case Some((v, b2)) => Some((List((w, v)), (a2, b2)))
        }
      }
    }

  def join[S[_], K, A, B, O](St: Streaming[S])(l: S[(K, A)], r: S[(K, B)], make: () => SortMerge[K, A, B, O]): S[O] =
    import St.*
    final case class J(l: Cursor[S, (K, A)], r: Cursor[S, (K, B)], m: SortMerge[K, A, B, O] | Null, done: Boolean)
    unfoldP[J, O](J(start(l), start(r), null, false)) { j =>
      if j.done then pureP(None)
      else
        val m = if j.m == null then make() else j.m.nn
        val out = ArrayBuffer.empty[O]
        m.step(out += _) match
          case SortMerge.Need.Done => pureP(Some((out.toList, j.copy(m = m, done = true))))
          case SortMerge.Need.Left => next(St)(j.l).mapP {
            case None => m.leftEnd(); Some((out.toList, j.copy(m = m)))
            case Some(((k, a), l2)) => m.left(k, a); Some((out.toList, j.copy(l = l2, m = m)))
          }
          case SortMerge.Need.Right => next(St)(j.r).mapP {
            case None => m.rightEnd(); Some((out.toList, j.copy(m = m)))
            case Some(((k, b), r2)) => m.right(k, b); Some((out.toList, j.copy(r = r2, m = m)))
          }
    }

  def windowed[S[_], A, K, Acc, O](St: Streaming[S])(s: S[A], make: () => Windows[K, A, Acc, O]): S[Pane[K, O]] =
    import St.*
    final case class W(c: Cursor[S, A], w: Windows[K, A, Acc, O] | Null, done: Boolean)
    unfoldP[W, Pane[K, O]](W(start(s), null, false)) { st =>
      if st.done then pureP(None)
      else
        val w = if st.w == null then make() else st.w.nn
        val out = ArrayBuffer.empty[Pane[K, O]]
        next(St)(st.c).mapP {
          case None => w.close()(out += _); Some((out.toList, st.copy(w = w, done = true)))
          case Some((a, c2)) => w.add(a)(out += _); Some((out.toList, W(c2, w, false)))
        }
    }
