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

  /**
   * The same merge, one channel transaction per `size` elements
   * instead of one per element — 4-5x, measured, and the reason it is
   * a SEPARATE combinator rather than how `merge` works.
   *
   * Profiling the elementwise merge (source-merge-chunked, 2026-09-03)
   * put 71% of its samples inside the per-element channel
   * transaction: 33% in the CAS itself, 19% in the immutable Queue
   * the state rebuilds around it, 19% in the rotation `resume` does
   * per pull. Two earlier lanes established that neither the queue's
   * data structure (channel-queue-reversal) nor the retry rate
   * (channel-cas-contention) can be improved in place. What is left
   * is the COUNT: chunk the sources, merge the chunk streams through
   * the very same machinery, unchunk on the way out, and the whole
   * 71% is divided by `size`. Measured against `merge` on the same
   * elements, quiet box: 2x2000 in 223.5us +/-2.9 against 1169.9
   * +/-9.3 (5.2x) at size 16, 247.2 +/-1.8 against 1163.4 +/-12.7
   * (4.7x) at 64; 2x500 in 70.2 +/-1.0 against 292.8 +/-3.5 (4.2x).
   * The default follows the library's other chunk defaults rather
   * than this benchmark's optimum — 16 measured ~10% better than 64
   * here, so tune it where throughput is worth tuning for.
   *
   * WHAT IT COSTS, and why `merge` keeps its own shape: a merge is
   * defined by READINESS, and this one batches. An element that could
   * have been handed over now waits for up to `size - 1` more (or for
   * its source to end, which flushes). The win does not come from
   * absorbing backpressure — under a consumer that keeps up there is
   * none to absorb — it comes precisely from batching sends that
   * would otherwise have succeeded immediately, so it cannot be made
   * invisible and is not a default. Reach for it where THROUGHPUT is
   * the point; keep `merge` where an element's arrival time is.
   *
   * (For a stream that is chunked to begin with, `Chunks.merge` is
   * cheaper still — 10.7us on 2x500 — because it never builds a
   * program node per element at all. This combinator is for sources
   * that are elementwise by nature but consumed in bulk.)
   */
  def mergeChunked[B](t: Source[B], size: Int = 64, capacity: Int = 64)
                     (using Scheduler, CanBlock): Source[A | B] =
    // both sides are lifted to the union BEFORE chunking: Free is
    // invariant in its row, so Source[Chunk[B]] is not a
    // Source[Chunk[A | B]] however covariant Chunk itself is — the
    // same invariance free-row-variance measured and kept, and the
    // widen pass it costs is the one that pays for itself
    def chunker: Unit ! (Take % (A | B) + (Writer % Chunk[A | B] + Async)) =
      !.widen[Unit, Take % (A | B) + Writer % Chunk[A | B], Async](Stage.chunked[A | B](size))
    val ac: Source[Chunk[A | B]] = through(Writer.widen[A, A | B, Unit, Async](s))(chunker)
    val bc: Source[Chunk[A | B]] = through(Writer.widen[B, A | B, Unit, Async](t))(chunker)
    val merged: Source[Chunk[A | B]] = ac.merge(bc, capacity)
    through(merged)(
      !.widen[Unit, Take % Chunk[A | B] + Writer % (A | B), Async](Stage.unchunk[A | B]))

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
