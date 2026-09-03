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
/**
 * The chunk boundary as an OPERATION, not a value.
 *
 * A chunking consumer emits when its chunk is full, when its input
 * ends, or (with `flushAfter`) when a timer expires — three rules
 * that all guess. A producer usually KNOWS: this token ended the
 * model's turn, that byte ended the frame. `Flush.now` says so
 * directly, and the boundary lands exactly where it belongs rather
 * than wherever the size or the clock happened to fall.
 *
 * It is an operation rather than a distinguished element because a
 * boundary is not data: making it one would widen every element type
 * to `A | Boundary` and force every consumer to match on something
 * that is not part of its stream.
 */
enum Flush[+A]:
  case Now extends Flush[Unit]

object Flush:
  given TypeableK[Flush] = typeableK(classOf[Flush[?]])

  /** emit whatever the chunker holds, full or not */
  def now[F[+_]]: Unit ! (Flush + F) = effect(Flush.Now)

/** a source that can also mark its own chunk boundaries. An ordinary
 * `Source` widens into it (it simply never uses the operation), so
 * the chunking path has one implementation rather than two */
type Flushing[W] = Unit ! (Flush + (Writer % W + Async))

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

  /**
   * The half-open range, told one element at a time, with no
   * collection underneath at all.
   *
   * `of(LazyList.range(...))` has to walk a lazy list, and profiling
   * the chunked merge found LazyList's own frames
   * (`state$lzycompute`, `unfold`) as large as the interpreter's —
   * a cell allocated and forced per element, for a sequence a
   * counter can produce. A `List` is no better: also a cell per
   * element, also traversed (measured, chunked-profile: 223.3 against
   * 212.9, no difference worth the name). This generates instead,
   * which is what `ZStream.range` does and what `Chunks.range`
   * already did on the chunked side.
   *
   * Lazy in the same way `Writer.of` is: nothing is told until the
   * result is consumed.
   */
  /**
   * The general generator: peel one element off `s` at a time, no
   * collection anywhere — `range` below is this specialised to
   * `Long`, kept separately only because a `Long => Option[(Long,
   * Long)]` step allocates a tuple this one's hand-written loop does
   * not (measured, idiomatic-api-compare: close enough not to matter
   * for most `S`, but `range`'s existence says the specialised form
   * is worth having when the step itself is this trivial).
   *
   * Verified against `ZStream.unfold` (zio-streams 2.1.14 sources,
   * `ZStream.scala:5076-5091`): the same shape, `Chunk.single(a)` per
   * step there — genuinely one element per production, not a
   * degenerate case of chunking. This is that shape, at the row this
   * library already had for it.
   */
  def unfold[S, A](s: S)(f: S => Option[(A, S)]): Source[A] =
    def go(s: S): Source[A] = f(s) match
      case Some((a, s2)) =>
        okay.effect[Writer % A + Async, Unit](Writer(a)).flatMap(_ => go(s2))
      case None => okay.pure(())
    okay.pure[Writer % A + Async, Unit](()).flatMap(_ => go(s))

  def range(from: Long, until: Long): Source[Long] =
    def go(i: Long): Source[Long] =
      if i >= until then okay.pure(())
      else okay.effect[Writer % Long + Async, Unit](Writer(i)).flatMap(_ => go(i + 1))
    okay.pure[Writer % Long + Async, Unit](()).flatMap(_ => go(from))

  /** what `merge(chunked = true)` batches by. Not a parameter: the
   * size barely moves the number (16 against 64 measured ~10% apart
   * across a 4x span) and exposing it would quietly break
   * `capacity`, which counts elements rather than chunks. 16 is the
   * better of the two measured (source-merge-chunked). */
  private[okay] val ChunkSize = 16

}

extension [A](s: Source[A])
  /**
   * The whole stream, as a `Vector` — the shape `ZStream#runCollect`
   * and fs2's `compile.toVector` both have, under the name this
   * library already uses for a terminal effect (`Writer.run`,
   * `Async.run`, `!.run`): a program, not a value forced by parking.
   *
   * `toLazyList` also reads the whole stream, but forces each pull
   * through `CanBlock` to hand back a plain `LazyList` — the right
   * shape for a synchronous caller (a benchmark, a REPL). `runCollect`
   * stays IN the program: walking `Writer.uncons`'s `Async` answer
   * never leaves the effect row, so it composes with `flatMap` like
   * any other step and is what an async caller — the shape every
   * competitor's terminal actually returns — wants.
   */
  def runCollect: Vector[A] ! Async =
    def go(rest: Source[A], acc: Vector[A]): Vector[A] ! Async =
      Writer.uncons[A, Unit, Async](rest).flatMap:
        case Right((a, more)) => go(more, acc :+ a)
        case Left(_) => okay.pure(acc)
    go(s, Vector.empty)

  /**
   * Run `f` for each element, in order — `ZStream#runForeach`,
   * fs2's `compile.foreach`, at this library's own `run` prefix. `f`
   * is itself a program, so a caller doing real work per element (an
   * I/O call, a send) writes it as one and this sequences it; a
   * caller with a plain side effect lifts it with `Async.Run` at the
   * call site, same as anywhere else in this library.
   */
  def runForeach(f: A => Unit ! Async): Unit ! Async =
    def go(rest: Source[A]): Unit ! Async =
      Writer.uncons[A, Unit, Async](rest).flatMap:
        case Right((a, more)) => f(a).flatMap(_ => go(more))
        case Left(_) => okay.pure(())
    go(s)

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
        // read in batches: the elements and their order are the
        // channel's, only the CAS is paid once per batch instead of
        // once per element (channel-drain)
        Writer.of(Drain(Channel.merge[A | B, S, Async, S, Async](sw, tw, capacity)))
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

extension [A](s: Source[A])
  /**
   * Chunking as a property of the STREAM rather than a parameter of
   * whatever consumes it — the orthogonal form, and the one to reach
   * for first.
   *
   * Everything that crosses a channel pays per crossing (71% of the
   * elementwise merge's CPU is one channel transaction, measured in
   * source-merge-chunked), so everything that crosses a channel wants
   * the option of crossing it in batches: `merge`, `buffer`, and
   * whatever comes next. Giving each of them its own `chunked` flag
   * would be the same concept spelled once per consumer; giving the
   * SOURCE a `chunked` combinator gives it to all of them at once and
   * costs nothing to compose — `a.chunked() merge b.chunked()`
   * measured 223.2us on 2x2000 against the fused `merge(chunked =
   * true)`'s 230.1, so composing is if anything the cheaper road.
   *
   * This form needs no concurrency: it is a pure transducer, one
   * `Chunk` per `size` elements plus a short final one.
   *
   * KNOWN LIMIT on `size`. A stage that accumulates without emitting
   * recurses once per element in `through`'s pull loop, so a chunk
   * larger than roughly two thousand elements can overflow the stack
   * — as can a SMALLER chunk that never fills because the stream is
   * shorter than it. Reproduced on b8c65c7 with `through` and
   * `Stage.chunked` alone, so it predates this combinator rather than
   * being introduced by it; filed as chunk-stack-safety. Sizes in the
   * tens or hundreds, which is where the throughput is anyway, are
   * unaffected. A TIMED flush
   * does need concurrency (a timer has to fire while the source is
   * silent), which is why `within` lives on `merge` rather than here —
   * see `merge`'s own `flushAfter`, which fuses the two so the timed
   * case still needs only ONE channel.
   */
  def chunked(size: Int = Source.ChunkSize): Source[Chunk[A]] =
    through(s)(!.widen[Unit, Take % A + Writer % Chunk[A], Async](Stage.chunked[A](size)))

extension [A](s: Source[Chunk[A]])
  /** chunks back into elements — the inverse of `chunked`, so a
   * pipeline can batch where it crosses a channel and go back to
   * per-element semantics on the other side */
  def unchunked: Source[A] =
    through(s)(!.widen[Unit, Take % Chunk[A] + Writer % A, Async](Stage.unchunk[A]))

extension [A](s: Flushing[A])
  /**
   * Merge two sources that mark their own chunk boundaries. Same
   * merge, same chunking, except that `Flush.now` in either source
   * emits what that side holds at exactly that point — so a boundary
   * lands where the producer says it is, rather than where the chunk
   * size or `flushAfter` happened to fall.
   *
   * Always chunked: an unchunked merge has nothing to flush, and the
   * operation would be silently meaningless. `flushAfter` still
   * applies as the backstop for a producer that goes quiet WITHOUT
   * marking a boundary.
   */
  infix def mergeFlushing[B](t: Flushing[B], capacity: Int = 64,
                             flushAfter: Option[Long] = None)
                            (using Scheduler, Timer): Source[A | B] =
    val slots = math.max(1, capacity / Source.ChunkSize)
    val sw = !.widen[Unit, Flush + (Writer % A + Async), Writer % (A | B)](s)
    val tw = !.widen[Unit, Flush + (Writer % B + Async), Writer % (A | B)](t)
    pure[Writer % (A | B) + Async, Unit](()).flatMap: _ =>
      through(
        Writer.of(Channel.mergeFlushing[A | B](sw, tw, slots, Source.ChunkSize, flushAfter)))(
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
