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
enum Flush[+A] derives Effect:
  case Now extends Flush[Unit]

object Flush:

  /** emit whatever the chunker holds, full or not */
  def now[F[+_]]: Unit ! Flush + F = effect(Flush.Now)

/** a source that can also mark its own chunk boundaries. An ordinary
 * `Source` widens into it (it simply never uses the operation), so
 * the chunking path has one implementation rather than two */
type Flushing[W] = Unit ! (Flush + (Writer % W + Async))

type Source[W] = Unit ! Writer % W + Async

/**
 * `generate`/`nats`/`fibs` (Generate.scala) produce a live source for
 * free (put-de-diagonal, 2026-09-19): put is tell, widened onto the
 * row that also admits Async, with the continuation resumed by `()`
 * rather than the told value — the answer `Put` asks for now, and the
 * one no `Source` could ever give before, since its own answer is
 * always `Unit` and never the element.
 */
given Put[Source] with
  final override inline def put[W](w: W): Unit /> Source[W] =
    Cont.shift(k => !.widen[Unit, Writer % W, Async](Writer.tell(w)).flatMap(_ => k(())))

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

  /**
   * A producer in a row, as a source — the Writer road for a seam
   * typed on `Produce`.
   *
   * `W` is the element type and `B` the producer's ANSWER, named
   * separately because the identity signature cannot: `Blob.get` is
   * `Either[String, Unit] ! Produce + Async`, which reads as a
   * producer of Eithers and produces chunks. The answer is kept —
   * for `get` it is the outcome, and an absent key lives there —
   * and each element is told through `produced`, the one cast the
   * producer algebra rests on (Generate.scala).
   *
   * One walk, no Option and no tuple per element: the shape of the
   * `Stream[[A] =>> A ! Produce + G, G]` instance, told instead of
   * unconsed. `Writer.of(p)` arrives at the same type through uncons
   * and pays both.
   */
  def fromProducer[W, B, G[+_] : TypeableK](p: B ! Produce + G): B ! Writer % W + G =
    import !.*
    type R = Writer % W + G
    // the walk as a frame of the machine (handle-frames-loops): each production told
    def frame(x: B ! Produce + G): okay.Shift.U[R, B] =
      okay.HandleFrames.statefulOver[Produce, Unit, B, B, R, Produce + G](okay.producing[G], (_, b) => okay.pure(b))(
        (_, w, resume) => okay.effect[R, Unit](Writer(produced[W](w))).flatMap(_ => resume((), w)))((), x)
    def go(p: B ! Produce + G): B ! R = (p.resumeRun: @unchecked) match
      case Free.Return(b) => okay.pure(b)
      case Inject(e) => split[G, Produce](e)
        (g => Inject(g): B ! R)
        (w => okay.effect[R, Unit](Writer(produced[W](w))).map(_ => produced[B](w)))
      case Bind(Inject(e), k) => split[G, Produce](e)
        (g => Inject(g).flatMap(x => go(k(x))): B ! R)
        (w => okay.effect[R, Unit](Writer(produced[W](w))).flatMap(_ => go(k(w))))
      case y => okay.HandleFrames.pending[B, R](frame(y))
    okay.HandleFrames.run[B, R](go(p), frame(p))

  /** a producer whose elements ARE its answer type — `Producer[A]` in
   * a row — as a `Source`: the answer, phantom by construction, is
   * dropped for the Unit a source answers */
  def ofProducer[A, G[+_] : TypeableK](p: A ! Produce + G): Unit ! Writer % A + G =
    fromProducer[A, A, G](p).map(_ => ())

  /**
   * A source as a producer, for a seam typed on `Produce`. Each told
   * value becomes a produce operation and G is performed as before.
   *
   * `end` is what the producer's final `Pure` carries. A producer's
   * answer is phantom — its stream instance reads the Pure as None
   * and never looks inside — so `end` is seen only by a walk that
   * reads the final value directly, and a byte stream passes
   * `Chunks.emptyChunk`. It is a parameter rather than a `null`
   * because a null in a Pure is a cast in disguise, and the caller
   * knows its own element type.
   */
  /**
   * A source of CHUNKS as one Vector of their elements — the writer
   * twin of `Producer.concat` (producer-to-writer-carrier): the drain
   * okay-sql/jdbc/pg/r2dbc/rag and their tests each spelled by hand
   * over `Produce + Async`. The answer is `Unit`, dropped.
   *
   * `Writer.loopWith` with the flattening as its finisher, so the
   * chunks are consed as they arrive and copied ONCE into the result
   * where the program ends — not `Writer.collect(s).map(_._1.flatten)`,
   * which built a `Vector` of chunks by `:+` and then mapped over a
   * program still forwarding Async, a rotation per forwarded
   * operation (writer-collect-loops).
   */
  def concat[X](s: Source[Chunk[X]]): Vector[X] ! Async =
    Writer.loopWith[Chunk[X], List[Chunk[X]], Unit, Vector[X], Async](s)(Nil)((l, c) => c :: l)((l, _) => flattenReversed(l))

  /** the chunks consed newest-first, as one Vector in arrival order */
  private def flattenReversed[X](l: List[Chunk[X]]): Vector[X] =
    var n = 0
    var cs = l
    while cs.nonEmpty do
      n += cs.head.length
      cs = cs.tail
    val b = Vector.newBuilder[X]
    b.sizeHint(n)
    var rs = l.reverse
    while rs.nonEmpty do
      b ++= rs.head
      rs = rs.tail
    b.result()

  def toProducer[A, G[+_]](s: Unit ! Writer % A + G)(end: A): A ! Produce + G =
    import !.*
    type R = Produce + G
    // the walk as a frame of the machine (handle-frames-loops): each tell produced, the end answered
    def frame(x: Unit ! Writer % A + G): okay.Shift.U[R, A] =
      okay.HandleFrames.statefulOver[Writer % A, Unit, Unit, A, R, Writer % A + G](summon[TypeableK[Writer % A]], (_, _) => okay.pure(end))(
        (_, op, resume) => okay.effect[R, A](Writer.told[A](op)).flatMap(_ => resume((), ())))((), x)
    def go(s: Unit ! Writer % A + G): A ! R = (s.resumeRun: @unchecked) match
      case Free.Return(_) => okay.pure(end)
      // Say is Writer's ONLY constructor, so a value that reaches the
      // second arm IS one — `Writer.widen`'s own argument, and its
      // @unchecked: the erased W cannot be verified, only its shape
      // Writer tested first (distinct-on-handlers): with the rest
      // inferred as the Writer itself, a rest-first split forwarded
      // every Say untouched
      case Inject(e) => split[Writer % A, G](e)
        // the producer still ENDS in `end`: a bare terminal Inject
        // would answer its own element instead
        (w => (w: @unchecked) match
          case Writer.Say(v) => okay.effect[R, A](v).flatMap(_ => okay.pure(end)))
        // a terminal operation's answer is the program's own, Unit here
        (g => (Inject(g): Unit ! R).map(_ => end))
      case Bind(Inject(e), k) => split[Writer % A, G](e)
        (w => (w: @unchecked) match
          case Writer.Say(v) => okay.effect[R, A](v).flatMap(_ => go(k(()))))
        // the operation's answer type is the tree's own existential and
        // cannot be named: no ascription, the expected type of the
        // branch types the re-injection — `Writer.widen`'s own shape
        (g => Inject(g).flatMap(x => go(k(x))))
      case y => okay.HandleFrames.pending[A, R](frame(y))
    okay.HandleFrames.run[A, R](go(s), frame(s))

  /**
   * Merge by READINESS on ONE thread of control (specs/ready-merge.md):
   * the merged program keeps a ring of the sources themselves and steps
   * them — an element that is ready is told out and its source goes to
   * the back, a source that is not ready (an `Async.Await`) parks and
   * its callback wakes the merge. No fiber, no `Scheduler`: it runs
   * wherever `Async` is handled, JS included.
   *
   * Each source keeps its own order. When no source ever waits, the
   * turns are a strict round-robin, so the result is deterministic.
   *
   * The price is the one a single thread has: a source that COMPUTES
   * before its next element holds the others meanwhile. Give such a
   * source its own fiber by buffering it — `Channel.buffer(n)(s).drained`
   * — and the merge reads it like any other not-yet-ready source.
   * `Source.merge` is the case that buffers every side.
   */
  def mergeReady[A](sources: Source[A]*)(using Wait, Pause): Source[A] = ReadyMerge(sources)

  /** merges released by a cancel or an early stop — a test's view of it
   * (merge-scopes-everywhere); a normal end releases nothing */
  private[okay] val mergeReleases = java.util.concurrent.atomic.AtomicLong()

  /**
   * `s` inside a cancel scope whose release is `release`
   * (merge-scopes-everywhere): run by a cancel, or when the program ends
   * with `s` unfinished (a consumer that stopped early), on every
   * scheduler — the drive's scopes, or the blocking handler's frame. The
   * scope closes when `s` ends, by a flatMap after it: one rotation per
   * node of `s`, so wrap a CHUNK source, not an element source.
   */
  private[okay] def releasing[A](release: () => Unit)(s: Source[A]): Source[A] =
    okay.pure[Writer % A + Async, Unit](()).flatMap: _ =>
      val scope = Async.CancelScope(release)
      okay.effect[Writer % A + Async, Unit](Async.Run(Async.Enter(scope)))
        .flatMap(_ => s)
        .flatMap(_ => okay.effect[Writer % A + Async, Unit](Async.Run(Async.Exit(scope))))


  /**
   * Pair `s` with `t` element for element, in LOCKSTEP, back
   * into a source: the pair of the two sides' next elements, until
   * EITHER side ends (specs/source-zip.md). Where `merge` answers
   * whichever side is ready, `zip` answers both — `Chunks.zip`'s law
   * on the live carrier, which had `merge`, `concat` and `either` and
   * no zip at all.
   *
   * A COMPANION function, not an extension, and deliberately: an
   * extension here is a top-level `zip` in package `okay`, and the core
   * already owns that name (Stream.scala's lazy `s.zip(that)`). Two
   * top-level definitions of one name in a split package are not
   * overloads — a dependent that sees both keeps ONE and silently loses
   * the other (the compiler says "Toplevel definition zip is defined in
   * ... Keeping only ..."). `Chunks.zip(p, q)` is the same spelling.
   *
   * The same shape as `merge`: each side buffered onto a fiber of its
   * own (`Channel.buffer`, `capacity` elements deep), the pairing on
   * the consumer's thread of control — one receive per side per pair,
   * so the two sides' pulls overlap through their buffers while the
   * consumer walks them in step. Lazy at the seam: the fibers start at
   * the first pull, not when this is called.
   *
   * ENDS. The side that ends first ends the zip, and the other side's
   * channel is CLOSED right there: its feeder fiber, parked on the
   * full buffer, wakes and ends (one still inside its source's own
   * pull ends at the next element it offers — a channel's close does
   * not reach into a source's Await, for `merge` either), and whatever
   * it had buffered is dropped — a zip has no use for an unpaired
   * element. A consumer that stops EARLY (`take`, `runFoldUntil`)
   * closes both, through the same cancel scope `merge` uses
   * (`Merge.closing`, `mergeReleases` counting it); a zip that ran to
   * its end has closed both sides already and releases nothing.
   *
   * A side that FAILS fails the zip at the pair its failure reached —
   * its channel carries the failure behind what it had buffered
   * (`Channel.fail` keeps the elements) — so every pair produced
   * before the failure is delivered first, as `merge` promises.
   *
   * Not on `mergeReady`'s ring: readiness is the wrong question for a
   * zip, which needs BOTH sides and waits for the slower one whatever
   * the other has ready.
   */
  def zip[A, B](s: Source[A], t: Source[B], capacity: Int = 64)
               (using Scheduler, CanBlock, Wait, Pause): Source[(A, B)] =
    type R = Writer % (A, B) + Async
    // one receive as a program on THIS row, the way `Channel.drained`
    // spells its await — `receive` answers `! Async` alone
    def receive[X](c: Channel[X]): Option[X] ! R =
      okay.effect[R, Option[X]](Async.Await[Option[X]] { k => c.receiveAsync(k); () => c.cancelReceive(k) })
    // the sides' fibers start HERE, at the first pull (a Source is a
    // value, and running it twice zips twice)
    okay.pure[R, Unit](()).flatMap: _ =>
      val cl = Channel.buffer[A, Source, Async](capacity)(s)
      val cr = Channel.buffer[B, Source, Async](capacity)(t)
      // entered in front, EXITED where the zip ends (one Exit a run, in
      // the end branches — not a Bind after the loop, a rotation per
      // pair). The exit is also what keeps the scope REACHABLE while the
      // program runs: `go` names it, so every continuation of the loop
      // holds it. Entered and never named again, it was held by nothing
      // on a plain `runWith` (no drive, no fiber handler), and a
      // collection mid-run released it through its collector door —
      // the backstop for an ABANDONED program — closing both sides: the
      // zip ended early and silently (source-zip-lost-pairs). An early
      // stop never reaches the exit; the drive, a fiber's handler or,
      // abandoned, the collector releases it then
      val scope = Async.CancelScope(Merge.closing(cl, cr))
      def end(other: Channel[?]): Unit ! R =
        other.close()
        okay.effect[R, Unit](Async.Run(Async.Exit(scope)))
      def go: Unit ! R =
        receive(cl).flatMap:
          case None => end(cr)
          case Some(a) =>
            receive(cr).flatMap:
              case None => end(cl)
              case Some(b) => okay.effect[R, Unit](Writer((a, b))).flatMap(_ => go)
      okay.effect[R, Unit](Async.Run(Async.Enter(scope))).flatMap(_ => go)

  /** `zip`, the pair folded by `f` as it is told — one `Writer.map`
   * walk over the zipped program, not a second join */
  def zipWith[A, B, C](s: Source[A], t: Source[B], capacity: Int = 64)(f: (A, B) => C)
                      (using Scheduler, CanBlock, Wait, Pause): Source[C] =
    Writer.map[(A, B), C, Unit, Async](zip(s, t, capacity))(f.tupled)

  /**
   * The sort-merge join by key of two live sources NON-DECREASING in
   * key (specs/stream-join.md), in `zip`'s shape: a fiber per side,
   * `capacity` elements buffered a side, the merge on the consumer's
   * thread, the right run of equal keys held and nothing beyond it.
   * Inner: every pair sharing a key; it ends when EITHER side ends and
   * closes the other. A key out of order fails the join after the pairs
   * before it. `SortMerge` is the machine.
   */
  def joinSorted[K: Ordering, A, B](l: Source[(K, A)], r: Source[(K, B)], capacity: Int = 64)
                                   (using Scheduler, CanBlock, Wait, Pause): Source[(K, (A, B))] =
    SortMerge.source(l, r, capacity)(() => SortMerge.inner)

  /** `joinSorted`, every left row kept (`None` where the right has no
   * such key); ends when the left side ends */
  def leftJoinSorted[K: Ordering, A, B](l: Source[(K, A)], r: Source[(K, B)], capacity: Int = 64)
                                       (using Scheduler, CanBlock, Wait, Pause): Source[(K, (A, Option[B]))] =
    SortMerge.source(l, r, capacity)(() => SortMerge.left)

  /** `joinSorted`, every row of either side kept (`None` on the side
   * that lacks the key); ends when both sides have */
  def fullJoinSorted[K: Ordering, A, B](l: Source[(K, A)], r: Source[(K, B)], capacity: Int = 64)
                                       (using Scheduler, CanBlock, Wait, Pause): Source[(K, (Option[A], Option[B]))] =
    SortMerge.source(l, r, capacity)(() => SortMerge.full)

  /**
   * The event-time WINDOWED join by key of two unbounded, UNORDERED
   * sources (specs/stream-join.md, stage 2; `WindowJoin` is the
   * machine): a row matches on arrival every row of the other side
   * with its key within `within` of its event time (`atL`/`atR`), is
   * held until the joint watermark — the smaller side's greatest event
   * time minus `lateness` — passes its reach, and a row behind that
   * watermark is dropped and counted, never joined. The two sides are
   * `either`-merged, each side's end marked, and the merge's release
   * law is the join's: an early stop releases both sides.
   */
  def joinWithin[K, A, B](l: Source[(K, A)], r: Source[(K, B)], within: Long, lateness: Long, capacity: Int = 64)
                         (atL: A => Long, atR: B => Long)
                         (using Scheduler, CanBlock, Timer, Merge, Wait, Pause): Source[(K, (A, B))] =
    type Ev = WindowJoin.Event[K, A, B]
    def ended[X](s: Source[X]): Source[Option[X]] =
      Writer.map[X, Option[X], Unit, Async](s)(x => Some(x))
        .flatMap(_ => okay.effect[Writer % Option[X] + Async, Unit](Writer(None)))
    through(ended(l).either(ended(r), capacity))(
      !.widen[Unit, Take % Ev + Writer % (K, (A, B)), Async](WindowJoin.stage[K, A, B](within, lateness)(atL, atR)))

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
    // ONE walk, not two (close-the-gaps, 2026-09-06). The first cut
    // did `uncons` per element and rebuilt the rest as a new program
    // -- `flatMap` per element -- which the Async handler then
    // interpreted a second time: every element cost a Free node here
    // and a step there. The walk is `Writer.loopWith`'s: tail-
    // recursive across tells, re-entered through `flatMap` only when
    // an Async operation has to be forwarded, the accumulator a cons
    // list reversed once where the program ends. It was a copy of
    // that loop split on `TypeableK[Async]` while `writerK` at an
    // abstract A was an unchecked E092 test; since
    // writer-typeablek-by-class (2026-09-19) it is a call
    // (writer-collect-loops).
    Writer.loopWith[A, List[A], Unit, Vector[A], Async](s)(Nil)((l, a) => a :: l)((l, _) => l.reverse.toVector)

  /**
   * A fold that stops (specs/fold-until.md): `Writer.foldUntil` on
   * this source. An Async operation before the stop is performed,
   * one after it never is, and the producer is not resumed past the
   * element that satisfied the fold.
   */
  def runFoldUntil[S, R](using FoldUntil[A, S, R]): R ! Async =
    Writer.foldUntil[A, S, Unit, R, Async](s)

  /**
   * Run `f` for each element, in order — `ZStream#runForeach`,
   * fs2's `compile.foreach`, at this library's own `run` prefix. `f`
   * is itself a program, so a caller doing real work per element (an
   * I/O call, a send) writes it as one and this sequences it; a
   * caller with a plain side effect lifts it with `Async.Run` at the
   * call site, same as anywhere else in this library.
   */
  def runForeach(f: A => Unit ! Async): Unit ! Async =
    // ONE walk (runforeach-one-walk, 2026-09-06), the same move as
    // `runCollect` above: `uncons` built a program per element -- a
    // Pure, an Either, a Bind and a call -- which the Async handler
    // then interpreted beside f(a)'s own. Now a tell embeds f(a) with
    // ONE Bind and the walk continues inside it; a forwarded Async
    // operation re-enters the loop the way `Writer.foldWith` does.
    // Same order, same effects; stack safety is the trampoline's, as
    // before, since every element re-enters through flatMap.
    import !.*
    def loop(x: Source[A]): Unit ! Async = (x.resume: @unchecked) match
      case Free.Return(_) => okay.pure(())
      case Inject(e) => split[Async, Writer % A](e)
        (g => Inject(g).map(_ => ()): Unit ! Async)
        { case Writer.Say(a) => f(a) }
      case Bind(Inject(e), k) => split[Async, Writer % A](e)
        (g => Inject(g).flatMap(v => loop(k(v))))
        { w0 => (w0: @unchecked) match
            case Writer.Say(a) => f(a).flatMap(_ => loop(k(()))) }
    loop(s)

  /** `Source.mergeReady` of two sources: by readiness, on this
   * program's own thread of control, no fiber per side */
  infix def mergeReady[B](t: Source[B])(using Wait, Pause): Source[A | B] =
    ReadyMerge[A | B](Seq(Writer.widen[A, A | B, Unit, Async](s), Writer.widen[B, A | B, Unit, Async](t)))

  /**
   * Merge two sources by READINESS, back into a source — the
   * concurrent join, in the shape the pipeline combinators consume
   * (`through`, `pipe`, a Stage). Each source is BUFFERED onto a
   * fiber of its own, and the two buffers are joined by
   * `Source.mergeReady` — a ring of the sides' continuations on the
   * consumer's thread (specs/source-merge-via-ready.md). Until
   * 2026-09-26 both fibers pushed into ONE shared two-part channel;
   * with the same fibers and the same buffering, the ring join
   * measured 0.86x / 0.93x of it (ready-merge-numbers), so there is one
   * merge mechanism now, and a caller who wants a side NOT on a fiber
   * of its own calls `mergeReady` directly.
   *
   * A side that FAILS drops out, the other still runs to its end, and
   * then the merge fails with the failure — so everything actually
   * produced is delivered first.
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
   * EACH SOURCE KEEPS ITS OWN ORDER. Which of the two arrives next
   * is a race and that is the whole point, but a source's elements
   * come out in the order that source told them, `chunked` either
   * way. This went unwritten until the code stopped doing it and a
   * source came back 1..16, 49, 50, 17..48 (merge-chunked-order,
   * 2026-09-09). The defect was in the channel's buffer rather than
   * here, but the promise belongs where a caller reads it.
   * EXACT since channel-known-producers (2026-09-20): a merge knows it
   * has two producers, so its channel opens two parts from the start
   * (`Channel.forProducers`) and never adopts a ring — no swap, and
   * each side pushes to one part for its whole life. `TestMergeOrder`
   * states the law; the default channel's "except once, across its
   * swap" (TestChannelLaws) is not this seam's claim any more.
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
                    (using Scheduler, CanBlock, Timer, Merge, Wait, Pause): Source[A | B] =
    val sw = Writer.widen[A, A | B, Unit, Async](s)
    val tw = Writer.widen[B, A | B, Unit, Async](t)
    val m = summon[Merge]
    // the mechanism is the given `Merge` (Ready by default, Shared by
    // choice); the sides' fibers start HERE, at the first pull, as they
    // always did — a Source is a value, and running it twice merges twice
    if !chunked then
      pure[Writer % (A | B) + Async, Unit](()).flatMap(_ => m.elements[A | B](sw, tw, capacity))
    else
      // capacity counts ELEMENTS, so the channel gets that many
      // divided by what each of its slots now holds
      val slots = math.max(1, capacity / Source.ChunkSize)
      // NOT `through(...)(Stage.unchunk)` (same reason as
      // `Source.unchunked`, merge-chunk-size-curve-inverted): that paired
      // two coroutines and made every element of every chunk cross the
      // handshake. `Writer.expand` walks the chunked program once and
      // re-tells the elements into a plain Free chain
      pure[Writer % (A | B) + Async, Unit](()).flatMap: _ =>
        Writer.expand[Chunk[A | B], A | B, Unit, Async](
          m.chunks[A | B](sw, tw, slots, Source.ChunkSize, flushAfter))(c => c)

  /**
   * `merge`, but keeping which side each element came from instead of
   * losing it in the union: this source's elements arrive as `Left`,
   * `t`'s as `Right`. Built on `merge` itself — tag each side first,
   * then the ordinary merge — so it inherits `merge`'s concurrency,
   * its per-side ordering guarantee, and its `chunked`/`flushAfter`
   * knobs unchanged; the tagging costs one `Writer.map` walk per side
   * rather than a second concurrent join.
   */
  infix def either[B](t: Source[B], capacity: Int = 64, chunked: Boolean = false,
                      flushAfter: Option[Long] = None)
                     (using Scheduler, CanBlock, Timer, Merge, Wait, Pause): Source[Either[A, B]] =
    Writer.map[A, Either[A, B], Unit, Async](s)(a => Left(a))
      .merge(Writer.map[B, Either[A, B], Unit, Async](t)(b => Right(b)), capacity, chunked, flushAfter)

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
    // NOT `through(s)(Stage.unchunk)` any more: that paired two
    // coroutines and made every element of every chunk cross the
    // handshake, at a cost that GREW with the chunk it came from
    // (merge-chunk-size-curve-inverted — the numbers are in
    // `Writer.expand`'s scaladoc). `expand` walks the program once
    // and re-tells the elements into a plain Free chain.
    Writer.expand[Chunk[A], A, Unit, Async](s)(c => c)

/** map the elements of a `Flushing` stream, leaving its `Flush.now`
 * marks exactly where the producer put them — `Writer.map` alone
 * cannot do this: its `G` needs a `TypeableK` instance and none exists
 * for the compound `Flush + Async`, so the split is spelled out here,
 * the same two-step `feedFlushing` (Channel.scala) already does */
private def mapFlushing[A, B](a: Flushing[A])(f: A => B): Flushing[B] =
  import !.*
  (a.resume: @unchecked) match
    case Free.Return(x) => Free.Return(x)
    case Inject(e) => split[Flush, Writer % A + Async](e)
      (fl => Inject(fl): Flushing[B])
      (wa => split[Async, Writer % A](wa)
        (g => Inject(g): Flushing[B])
        { w0 => (w0: @unchecked) match
            case Writer.Say(w) => Inject(Writer(f(w))) })
    case Bind(Inject(e), k) => split[Flush, Writer % A + Async](e)
      (fl => Inject(fl).flatMap(x => mapFlushing(k(x))(f)))
      (wa => split[Async, Writer % A](wa)
        (g => Inject(g).flatMap(x => mapFlushing(k(x))(f)))
        { w0 => (w0: @unchecked) match
            case Writer.Say(w) => Inject(Writer(f(w))).flatMap(_ => mapFlushing(k(()))(f)) })

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
                            (using Scheduler, Timer, Merge, Wait, Pause): Source[A | B] =
    val slots = math.max(1, capacity / Source.ChunkSize)
    val sw = !.widen[Unit, Flush + (Writer % A + Async), Writer % (A | B)](s)
    val tw = !.widen[Unit, Flush + (Writer % B + Async), Writer % (A | B)](t)
    val m = summon[Merge]
    // the given mechanism's flushing join, unchunked by `Writer.expand`
    pure[Writer % (A | B) + Async, Unit](()).flatMap: _ =>
      Writer.expand[Chunk[A | B], A | B, Unit, Async](
        m.flushing[A | B](sw, tw, slots, Source.ChunkSize, flushAfter))(c => c)

  /**
   * `mergeFlushing`, but tagging which side each element came from —
   * the same `Left`/`Right` convention as `Source.either`, and the
   * same trick: tag each side's elements first (`mapFlushing` leaves
   * its `Flush.now` marks untouched), then the ordinary flushing merge.
   */
  infix def eitherFlushing[B](t: Flushing[B], capacity: Int = 64,
                              flushAfter: Option[Long] = None)
                             (using Scheduler, Timer, Merge, Wait, Pause): Source[Either[A, B]] =
    mapFlushing[A, Either[A, B]](s)(a => Left(a))
      .mergeFlushing(mapFlushing[B, Either[A, B]](t)(b => Right(b)), capacity, flushAfter)

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
    Channel.merge[Chunk[A], [W] =>> Unit ! Writer % W, Pure, [W] =>> Unit ! Writer % W, Pure](s, t, capacity)

  /**
   * `merge`, tagging which side each element came from — the same
   * `Left`/`Right` convention as `Source.either`, built the same way:
   * a `Chunks.map` walk tags each side, then the existing chunked
   * merge (unlike `Source.either` this needs no `Writer.map`, since a
   * chunked stream's element mapper already runs inside its chunks).
   */
  infix def either[B](t: Chunks[B])(using Scheduler): Channel[Chunk[Either[A, B]]] =
    either(t, 64)

  def either[B](t: Chunks[B], capacity: Int)(using Scheduler): Channel[Chunk[Either[A, B]]] =
    val ls: Chunks[Either[A, B]] = Chunks.map(s)(a => Left(a))
    val rt: Chunks[Either[A, B]] = Chunks.map(t)(b => Right(b))
    ls.merge(rt, capacity)
