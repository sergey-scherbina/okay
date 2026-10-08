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

object streams:

  /** the machine's: a pull over the machine's Async (`StreamCont`) */
  object machine:
    type Flow[W] = StreamCont.Src[W]
    given instance: Streaming[Flow] with
      type P[A] = A ! StreamCont.R
      def empty[W]: Flow[W] = StreamCont.empty
      def emit[W](w: W): Flow[W] = StreamCont.emit(w)
      def fromList[W](ws: List[W]): Flow[W] = StreamCont.fromList(ws)
      def range(from: Long, until: Long): Flow[Long] = StreamCont.range(from, until)
      def eval[W](p: P[W]): Flow[W] = StreamCont.eval(p)
      def async[A](a: => A): P[A] = AsyncCont.async(a).at
      def runAsync[A](p: P[A]): Future[A] = AsyncCont.runAsync(p)
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
    def fromSource[W](s: Source[W]): Flow[W] = StreamCont.fromSource(s)
    extension [W](f: Flow[W]) def toSource: Source[W] = StreamCont.toSource(f)

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
