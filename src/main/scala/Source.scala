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

  /** what `merge(chunked = true)` batches by. Not a parameter: the
   * size barely moves the number (16 against 64 measured ~10% apart
   * across a 4x span) and exposing it would quietly break
   * `capacity`, which counts elements rather than chunks. 16 is the
   * better of the two measured (source-merge-chunked). */
  private[okay] val ChunkSize = 16
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
  /**
   * `chunked` trades READINESS for throughput, and is the one knob
   * here that changes what the merge PROMISES rather than only what
   * it costs.
   *
   * Left `false`, this is the readiness merge described above: every
   * element crosses the channel on its own, and an element that is
   * ready is handed over. That costs what it costs — profiled
   * (source-merge-chunked, 2026-09-03), 71% of the elementwise
   * merge's samples sit in the per-element channel TRANSACTION: 33%
   * the CAS itself, 19% the immutable Queue rebuilt around it, 19%
   * `resume`'s per-pull rotation. Four earlier lanes tried to make
   * that transaction cheaper and failed (its queue's data structure,
   * its retry rate, the kernel's tree shape, the row's variance);
   * what is left is to run FEWER of them.
   *
   * `true` does that: both sides are chunked, the chunk streams merge
   * through the very same machinery, and the chunks are flattened on
   * the way out — one transaction per `ChunkSize` elements, an
   * ordinary `Source` either way. Measured on 2x2000, quiet box:
   *
   *   false                    1163.4us +/-21.2
   *   true,  capacity 64        443.6   +/-23.8   2.6x
   *   true,  capacity 1024      226.5   +/-1.2    5.1x
   *
   * The two knobs are orthogonal and both honest: `capacity` counts
   * ELEMENTS whichever way `chunked` is set, so turning chunking on
   * alone buys 2.6x while holding the same 64 elements, and the rest
   * of the win is bought explicitly with memory. (226.5 is the
   * ceiling: a hand-built chunk-merge pipeline measures 223.2.)
   *
   * WHY IT IS OFF BY DEFAULT, and it is not politeness. On its own,
   * `chunked` emits when a chunk is FULL or when its input ENDS. On a
   * slow or unending source — which is what this merge is for (a
   * model's tokens, a chat's turns, a live feed) — an element
   * therefore waits for `ChunkSize - 1` others that may be a long
   * time coming, or may never come. There is a test that shows
   * exactly that stall rather than describing it.
   *
   * `flushAfter` is the answer to it: a partial chunk waits at most
   * that many milliseconds before being sent anyway, so chunking
   * becomes safe on a live source. It costs nothing when it does not
   * fire (230.0us +/-3.3 with a 30-second window against 230.1
   * +/-0.9 without, on 2x2000 — the same number), because the
   * flusher is a fiber that sleeps beside the feed rather than
   * anything the per-element path pays for.
   *
   * It also, deliberately, never touches the PULL. The obvious way
   * to bound the wait is to race the source's `uncons` against a
   * timer, and it is wrong: `Async.timeout` cancels the loser, and
   * cancelling an in-flight `uncons` on a live source can lose the
   * element it was about to yield. The flusher instead takes what
   * has already accumulated, out of a cell the feed writes into,
   * which is safe whatever the pull is doing.
   *
   * The SIZE is not a parameter, on purpose. It barely matters — 16
   * against 64 measured ~10% apart across a 4x span — while exposing
   * it would quietly break `capacity`, which counts ELEMENTS: a
   * channel of `capacity` chunks would hold `capacity * size` of
   * them, so a caller asking for 64 would silently get 4096. The
   * size is fixed here and the channel is given `capacity /
   * ChunkSize` slots instead, which keeps `capacity` meaning what it
   * says either way — and leaves raising it as the honest, visible
   * way to buy the rest of the throughput.
   *
   * (A stream that is chunked to begin with wants `Chunks.merge`
   * instead — 10.7us on 2x500, since it never builds a program node
   * per element at all.)
   */
  infix def merge[B](t: Source[B], capacity: Int = 64, chunked: Boolean = false,
                     flushAfter: Option[Long] = None)
                    (using Scheduler, CanBlock, Timer): Source[A | B] =
    type S[W] = Unit ! (Writer % W + Async)
    val sw = Writer.widen[A, A | B, Unit, Async](s)
    val tw = Writer.widen[B, A | B, Unit, Async](t)
    if !chunked then
      pure[Writer % (A | B) + Async, Unit](()).flatMap: _ =>
        Writer.of(Channel.merge[A | B, S, Async, S, Async](sw, tw, capacity))
    else
      // capacity counts ELEMENTS, so the channel gets that many
      // divided by what each of its slots now holds
      val slots = math.max(1, capacity / Source.ChunkSize)
      pure[Writer % (A | B) + Async, Unit](()).flatMap: _ =>
        through(
          Writer.of(Channel.mergeChunked[A | B, S, Async, S, Async](
            sw, tw, slots, Source.ChunkSize, flushAfter)))(
          !.widen[Unit, Take % Chunk[A | B] + Writer % (A | B), Async](
            Stage.unchunk[A | B]))

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
