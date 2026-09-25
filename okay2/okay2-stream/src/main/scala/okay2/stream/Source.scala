package okay2.stream

import okay2._
import okay2.Free.{Return, Inject, Bind}
import okay2.async._
import scala.annotation.tailrec

/**
 * An asynchronous SOURCE: a program that tells its elements as it
 * goes, performing Async between them — `Source[W] = Unit !
 * (Writer[W] + Async)`. By Writer's instance an ordinary stream in
 * Async, so every stream combinator applies; `merge` is the
 * concurrency only this carrier can express.
 */
object Source {

  /** any PURE stream as a source: told one by one into a row that also
   * admits Async, so a constant feed and a live one compose */
  def of[S[_], A](s: S[A])(implicit St: Stream[S, Pure]): Source[A] = Writer.of[S, Pure, A](s).at[Writer[A] + Async]

  /** these elements, told in order */
  def apply[A](as: A*): Source[A] = of(as.toList)

  /** the general generator: peel one element off `s` at a time, no
   * collection anywhere; lazy, nothing told until consumed */
  def unfold[S, A](s: S)(f: S => Option[(A, S)]): Source[A] = {
    def go(s: S): Source[A] = f(s) match {
      case Some((a, s2)) => Writer.tell(a).at[Writer[A] + Async].flatMap(_ => go(s2))
      case None => pure(())
    }
    pure[Writer[A] + Async, Unit](()).flatMap(_ => go(s))
  }

  /** the half-open range, told one element at a time */
  def range(from: Long, until: Long): Source[Long] = {
    def go(i: Long): Source[Long] =
      if (i >= until) pure(())
      else Writer.tell(i).at[Writer[Long] + Async].flatMap(_ => go(i + 1))
    pure[Writer[Long] + Async, Unit](()).flatMap(_ => go(from))
  }

  /** a source of CHUNKS as one Vector of their elements: the chunks
   * consed as they arrive and copied ONCE where the program ends */
  def concat[X](s: Source[Chunk[X]]): Vector[X] ! Async =
    Writer.loopWith[Chunk[X], List[Chunk[X]], Unit, Vector[X], Async](s)(Nil)((l, c) => c :: l)((l, _) => flattenReversed(l))

  private def flattenReversed[X](l: List[Chunk[X]]): Vector[X] = {
    var n = 0
    var cs = l
    while (cs.nonEmpty) { n += cs.head.length; cs = cs.tail }
    val b = Vector.newBuilder[X]
    b.sizeHint(n)
    var rs = l.reverse
    while (rs.nonEmpty) { b ++= rs.head; rs = rs.tail }
    b.result()
  }

  /** what `merge(chunked = true)` batches by: not a parameter, since
   * exposing it would quietly break `capacity`, which counts ELEMENTS */
  private[stream] val ChunkSize = 16

  implicit final class SourceOps[A](private val s: Source[A]) extends AnyVal {

    /** the whole stream, as a Vector — a program, not a value forced
     * by parking; ONE walk (`Writer.loopWith`'s), tail-recursive across
     * tells, re-entered through flatMap only for a forwarded Async */
    def runCollect: Vector[A] ! Async =
      Writer.loopWith[A, List[A], Unit, Vector[A], Async](s)(Nil)((l, a) => a :: l)((l, _) => l.reverse.toVector)

    /** a fold that stops: an Async operation before the stop is
     * performed, one after it never is */
    def runFoldUntil[S, R](fo: FoldUntil[A, S, R]): R ! Async = Writer.foldUntilAt[A, S, Unit, R, Async](s)(fo)

    /** run `f` for each element, in order, as ONE walk */
    def runForeach(f: A => Unit ! Async): Unit ! Async = {
      val Mine = okay2.Split.at[Writer[A]]
      // a call from inside flatMap (or a by-name `++`) cannot be a jump; `again`
      // takes it, so the walk itself stays a checked loop (specs/stack-safety.md)
      def again(x: Source[A]): Unit ! Async = loop(x)
      @tailrec def loop(x: Source[A]): Unit ! Async = Free.resume(x) match {
        case Return(_) => pure(())
        case Inject(e) => loop(Bind(Inject[Writer[A] + Async, Unit](e), (x: Unit) => Return[Writer[A] + Async, Unit](x)))
        case Bind(Inject(Mine(Writer.Say(a))), k) => f(a).flatMap(_ => again(k(())))
        case Bind(Inject(g), k) => Free.Inject[Async, Any](g).flatMap(v => again(k(v)))
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }
      loop(s)
    }

    /**
     * Merge two sources by READINESS, back into a source: a fiber per
     * source feeds one channel; the channel is told out again as a
     * program. EACH SOURCE KEEPS ITS OWN ORDER; which side arrives next
     * is a race. Lazy at the seam: the fibers start at the FIRST PULL.
     * BOUNDED by default (an endless source merged unbounded is
     * unbounded memory). The element types need not agree: the result
     * tells their common supertype — Scala 2's spelling of the Scala 3
     * union `A | B` (`either` keeps the side as data instead).
     *
     * `chunked` trades readiness for throughput — one channel
     * transaction per 16 elements, an ordinary Source either way — and
     * is OFF by default because a partial chunk on a slow or unending
     * source waits for elements that may never come; `flushAfter`
     * bounds that wait, by a flusher that takes what has accumulated
     * and never touches the pull.
     */
    def merge[B >: A](t: Source[B], capacity: Int = 64, chunked: Boolean = false, flushAfter: Option[Long] = None)
                     (implicit sch: Scheduler, cb: CanBlock, timer: Timer): Source[B] = {
      val sw: Source[B] = Writer.widen[A, B, Unit, Async](s)
      if (!chunked)
        pure[Writer[B] + Async, Unit](()).flatMap(_ => Writer.of[Drain, Async, B](Drain(Channel.mergeSources(sw, t, capacity))))
      else {
        val slots = math.max(1, capacity / Source.ChunkSize)
        pure[Writer[B] + Async, Unit](()).flatMap(_ =>
          Writer.expand[Chunk[B], B, Unit, Async](
            Writer.of[Drain, Async, Chunk[B]](Drain(Channel.mergeSourcesChunked(sw, t, slots, Source.ChunkSize, flushAfter))))(c => c))
      }
    }

    /** `merge`, keeping which side each element came from: this
     * source's elements arrive as `Left`, `t`'s as `Right` */
    def either[B](t: Source[B], capacity: Int = 64, chunked: Boolean = false, flushAfter: Option[Long] = None)
                 (implicit sch: Scheduler, cb: CanBlock, timer: Timer): Source[Either[A, B]] =
      new SourceOps(Writer.mapAt[A, Either[A, B], Unit, Async](s)(a => Left(a)))
        .merge[Either[A, B]](Writer.mapAt[B, Either[A, B], Unit, Async](t)(b => Right(b)), capacity, chunked, flushAfter)

    /** chunking as a property of the STREAM: a pure transducer, one
     * Chunk per `size` elements plus a short final one */
    def chunked(size: Int = Source.ChunkSize): Source[Chunk[A]] =
      Pipe.intoIn[A, Chunk[A], Async, Unit, Unit](s)(Stage.chunked[A](size).at[Take[A] + (Writer[Chunk[A]] + Async)])

    /** the same source at a WIDER element type: `Say[A]` IS a `Say[B]`,
     * so nothing is rebuilt — the row is invariant in W, hence the name */
    def widen[B >: A]: Source[B] = Writer.widen[A, B, Unit, Async](s)

    /** the source as a LazyList, each pull forced under CanBlock */
    def toLazyList(implicit cb: CanBlock): LazyList[A] = Stream.FeedInOps(s).toLazyList
  }

  implicit final class ChunkSourceOps[A](private val s: Source[Chunk[A]]) extends AnyVal {
    /** chunks back into elements: `Writer.expand` walks the program
     * once and re-tells the elements into a plain Free chain */
    def unchunked: Source[A] = Writer.expand[Chunk[A], A, Unit, Async](s)(c => c)
  }

  implicit final class ChunksMergeOps[A](private val s: Chunks[A]) extends AnyVal {
    /** the same merge for CHUNKED streams: one queue operation per
     * chunk; answers the channel itself */
    def merge(t: Chunks[A], capacity: Int = 64)(implicit sch: Scheduler): Channel[Chunk[A]] = {
      type L[W] = Unit ! Writer[W]
      Channel.merge[Chunk[A], L, Pure, L, Pure](s, t, capacity)(Stream.feedStream[Unit], Handler.pure, Stream.feedStream[Unit], Handler.pure, sch)
    }

    /** `merge`, tagging which side each element came from */
    def either[B](t: Chunks[B], capacity: Int = 64)(implicit sch: Scheduler): Channel[Chunk[Either[A, B]]] =
      new ChunksMergeOps(Chunks.map(s)(a => Left(a): Either[A, B])).merge(Chunks.map(t)(b => Right(b): Either[A, B]), capacity)
  }
}
