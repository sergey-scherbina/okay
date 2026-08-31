package okay

/**
 * An asynchronous SOURCE: a program that tells its elements as it
 * goes, performing Async between them. The shape every streaming seam
 * in this library already had — a transport's lines, a model's
 * tokens, a merged feed — spelled once, and by Writer's instance an
 * ordinary `Stream` in Async, so every stream combinator applies.
 *
 * `Writer.of` is the general constructor (any stream, its own effects
 * kept); the two here are the async ones, and `merge` below is the
 * concurrency that only this carrier can express.
 */
type Source[W] = Unit ! (Writer % W + Async)

object Source {
  /**
   * Any PURE stream as a source: a List, a LazyList, a Producer, a
   * Chunks' element view — told one by one into a row that also
   * admits Async, so a constant feed and a live one compose (see
   * merge). `Writer.of` is the general form, which keeps whatever
   * effects the stream itself has.
   */
  def of[S[_], A](s: S[A])(using Stream[S, Pure]): Source[A] =
    !.widen[Unit, Writer % A, Async](Writer.of(s))

  /** these elements, told in order */
  def apply[A](as: A*): Source[A] = of(as.toList)
}

extension [A](s: Source[A])
  /**
   * Merge two sources by READINESS, back into a source — the
   * concurrent join, in the shape the pipeline combinators consume
   * (`through`, `pipe`, a Stage). A fiber per source feeds one
   * channel; the channel is told out again as a program, so what
   * comes back is an ordinary source and nothing had to leave the
   * effect world.
   *
   * The element types need NOT agree: the result tells their union,
   * which is what a join of two differently shaped feeds actually is
   * — the consumer splits it by an ordinary type test. (Writer is
   * invariant in what it tells, so each side is re-told at the union
   * by Writer.map; that walk is the whole cost.)
   *
   * Lazy at the seam: the fibers start at the FIRST PULL, not when
   * this is called — a source nobody consumes drains nothing.
   */
  infix def merge[B](t: Source[B], capacity: Int = Int.MaxValue)
                    (using Scheduler, CanBlock): Source[A | B] =
    type S[W] = Unit ! (Writer % W + Async)
    pure[Writer % (A | B) + Async, Unit](()).flatMap: _ =>
      Writer.of(Channel.merge[A | B, S, Async, S, Async](
        Writer.map(s)(identity[A | B]), Writer.map(t)(identity[A | B]), capacity))

extension [A](s: Chunks[A])
  /**
   * The same merge for CHUNKED streams: the existing Channel.merge,
   * one queue operation per chunk.
   *
   * It answers the channel itself rather than a source, and that is
   * measured rather than stylistic — this is the benchmarked path
   * (merge 2x500: 14.7us against ZIO's 47.3), and a source would add
   * a told program node per chunk to a walk whose whole point is that
   * a chunk costs one queue operation. Consume it with `receive`, or
   * as the Async stream it already is.
   */
  infix def merge(t: Chunks[A])(using Scheduler): Channel[Chunk[A]] =
    merge(t, Int.MaxValue)

  /** the same, with the channel bounded: a fast producer parks when
   * the consumer is that many chunks behind (the arity is spelled out
   * rather than defaulted — only one overload of a name may carry
   * default arguments, and the source merge has them) */
  def merge(t: Chunks[A], capacity: Int)(using Scheduler): Channel[Chunk[A]] =
    Channel.merge[Chunk[A], Producer, Pure, Producer, Pure](s, t, capacity)
