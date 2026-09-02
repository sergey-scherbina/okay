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
   * — the consumer splits it by an ordinary type test. (Free is
   * invariant in its row, so each side is re-told at the union by
   * `Writer.widen` — a walk over the program's nodes, but with
   * `Writer[+W, +A]` covariant no transform runs and no operation is
   * rebuilt, only the Free nodes around it. Fusing that walk into
   * the original construction — one unfold per side instead of
   * build-then-widen — was TRIED and MEASURED WORSE, not better:
   * specs/writer-covariance.md Results.)
   *
   * Lazy at the seam: the fibers start at the FIRST PULL, not when
   * this is called — a source nobody consumes drains nothing.
   *
   * BOUNDED by default, and that default is the interesting decision.
   * The channel underneath will take everything offered, so an
   * unbounded merge of an ENDLESS source is unbounded memory: measured,
   * a source merged unbounded and consumed ten elements deep produced
   * 1 269 819 of them in 300ms and kept going, where the same merge at
   * 64 produced 74. What the bound costs was measured too, and it is
   * nothing this benchmark can see (2x500 elements: 210.1us +/-9.7
   * unbounded against 232.7 +/-20.4 at 64 — bars that overlap), so the
   * safe default is the free one. `Int.MaxValue` still buys the
   * unbounded channel where the producer is known to be finite and
   * small; `Channel.merge` underneath keeps ITS default unbounded,
   * because there the capacity is the caller's explicit business.
   */
  infix def merge[B](t: Source[B], capacity: Int = 64)
                    (using Scheduler, CanBlock): Source[A | B] =
    type S[W] = Unit ! (Writer % W + Async)
    pure[Writer % (A | B) + Async, Unit](()).flatMap: _ =>
      Writer.of(Channel.merge[A | B, S, Async, S, Async](
        Writer.widen[A, A | B, Unit, Async](s), Writer.widen[B, A | B, Unit, Async](t), capacity))

extension [A](s: Chunks[A])
  /**
   * The same merge for CHUNKED streams: the existing Channel.merge,
   * one queue operation per chunk.
   *
   * It answers the channel itself rather than a source, and that is
   * measured rather than stylistic — this is the benchmarked path
   * (merge 2x500: 10.7us against ZIO's 45.4), and a source would add
   * a told program node per chunk to a walk whose whole point is that
   * a chunk costs one queue operation. Consume it with `receive`, or
   * as the Async stream it already is.
   *
   * Bounded by default for the reason the source merge is (an endless
   * source merged unbounded is unbounded memory), and here the price
   * was measured directly on that benchmark: 10.700us +/-0.292
   * unbounded against 10.819 +/-0.136 at 64 chunks. No difference to
   * see, so the default is the safe one.
   */
  infix def merge(t: Chunks[A])(using Scheduler): Channel[Chunk[A]] =
    merge(t, 64)

  /** the same, with the channel bounded explicitly — `Int.MaxValue`
   * for the unbounded one (the arity is spelled out rather than
   * defaulted: only one overload of a name may carry default
   * arguments, and the source merge has them) */
  def merge(t: Chunks[A], capacity: Int)(using Scheduler): Channel[Chunk[A]] =
    Channel.merge[Chunk[A], Producer, Pure, Producer, Pure](s, t, capacity)
