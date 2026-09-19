package okay

import scala.collection.immutable.ArraySeq

/**
 * A chunked stream is an ordinary producer of whole batches — nothing
 * new in the stream layer, elements are polymorphic. What changes is
 * the arithmetic: the freer tree steps once per CHUNK, and an element
 * inside costs an array index — the amortization the chunked runtimes
 * (ZStream, fs2, kyo) are built on. Generators below fill each chunk
 * in a tight while-loop, so no tree node is ever paid per element;
 * merge of chunked streams is the existing Channel.merge applied to
 * Chunks values — one queue operation per chunk, for free.
 */
type Chunks[A] = Producer[Chunk[A]]

object Chunks {

  /**
   * Unfold with a tight per-chunk loop: size elements of f over the
   * unfolding seed per emitted chunk. Construction is lazy — the
   * first chunk is computed at the first pull, one chunk at a time.
   */
  inline def generate[A, B](a: A)(inline f: A => B)(inline g: A => A)
                           (size: Int = 64): Chunks[B] =
    generateWith(a)(ChunkBuf.filler[A, B](f)(g)(size))

  /** the recursion behind the inline `generate` — public for the same
   * binary-compatibility reason as `mapWith` */
  def generateWith[A, B](a: A)(fill: A => (Chunk[B], A)): Chunks[B] =
    def go(s: A): Chunks[B] = pure[Produce, Unit](()).flatMap: _ =>
      val (c, cur) = fill(s)
      produce(c).flatMap(_ => go(cur))

    go(a)

  /** the numbers from until (exclusive), a short tail chunk if needed */
  def range(from: Long, until: Long, size: Int = 64): Chunks[Long] =
    def go(s: Long): Chunks[Long] = pure[Produce, Unit](()).flatMap: _ =>
      if s >= until then pure(ArraySeq.empty)
      else
        val n = math.min(size.toLong, until - s).toInt
        // Long is concrete here, so this is a long[] with no boxing —
        // and it matters more than it looks: a boxed SOURCE makes
        // every downstream stage's specialization moot, because the
        // first thing each does is read a boxed element back out.
        val arr = new Array[Long](n)
        var i = 0
        while i < n do
          arr(i) = s + i
          i += 1
        produce(ArraySeq.unsafeWrapArray(arr)).flatMap(_ => go(s + n))

    go(from)

  /**
   * Chunk up a (mutable, linear) iterator: size elements per chunk,
   * pulled lazily — the iterator advances only as chunks are pulled,
   * so an infinite iterator is fine. The stream is as linear as its
   * source: re-observation re-reads the SAME iterator.
   */
  /**
   * A string as chunks of characters, WITHOUT boxing: `ArraySeq` has
   * a primitive-backed subclass, so the chars live in an Array[Char]
   * and never become objects. The generic `fromIterator` cannot do
   * this — it fills an Array[AnyRef] — and the difference is what a
   * measured lane blamed for chunked lexing being slower than the
   * element-wise path. Whether it is enough is a question for the
   * benchmark, not for this comment.
   */
  def ofChars(text: String, size: Int = 64): Chunks[Char] =
    def go(from: Int): Chunks[Char] = defer:
      if from >= text.length then end
      else
        val n = math.min(size, text.length - from)
        val arr = new Array[Char](n)
        text.getChars(from, from + n, arr, 0)
        produce(scala.collection.immutable.ArraySeq.unsafeWrapArray(arr))
          .flatMap(_ => go(from + n))

    go(0)

  inline def fromIterator[A](it: Iterator[A], size: Int = 64): Chunks[A] =
    fromIteratorWith(it)(ChunkBuf.factory[A](size))(size)

  /** the recursion behind the inline `fromIterator` — public for the
   * same binary-compatibility reason as `mapWith` */
  def fromIteratorWith[A](it: Iterator[A])(fresh: () => ChunkBuf[A])
                         (size: Int): Chunks[A] =
    def go(): Chunks[A] = pure[Produce, Unit](()).flatMap: _ =>
      if !it.hasNext then end
      else
        val buf = fresh()
        var i = 0
        while i < size && it.hasNext do
          buf(i) = it.next()
          i += 1
        val c = buf.take(i)
        produce(c).flatMap(_ => go())

    go()

  import scala.math.Numeric.Implicits.given

  /** the naturals: 0, 1, 2, ... in chunks */
  def nats[N: Numeric as N](size: Int = 64): Chunks[N] =
    generate(N.zero)(identity)(_ + N.one)(size)

  /** the Fibonacci numbers, in chunks */
  def fibs[N: Numeric as N](size: Int = 64): Chunks[N] =
    generate((N.zero, N.one))(_._1)((x, y) => (y, x + y))(size)

  import !.*

  /** defer one step: nothing before the bind runs at construction */
  private[okay] inline def defer[X](inline x: => Producer[X]): Producer[X] =
    pure[Produce, Unit](()).flatMap(_ => x)

  /** the empty chunk — what a producer of chunks ENDS in (its final
   * `Pure`, which the stream instance never reads), and the `end` a
   * `Source.toProducer` over chunks is given. Public since
   * producer-drains: a consumer spelling `ArraySeq.empty` and hoping
   * it was the same thing was right, and should not have to hope */
  def emptyChunk[B]: Chunk[B] = ArraySeq.empty[AnyRef].asInstanceOf[Chunk[B]]

  /**
   * The chunk a `Bind(Inject(c), k)` node carries.
   *
   * `case Inject(c)` needs no such thing — the refinement gives the
   * type back — but under a `Bind` the element is the BIND's
   * intermediate, which is existential, so the type is known only
   * from the surrounding `Chunks[A]` and nothing about the value says
   * so. Named once here rather than asserted at each of the eight
   * places that pattern appears.
   */
  private[okay] def bound[A](c: Any): Chunk[A] = c.asInstanceOf[Chunk[A]]

  /** the end of a chunked stream */
  private[okay] def end[B]: Chunks[B] = pure(emptyChunk)

  /** pull one chunk: the pure step of a chunked stream */
  private[okay] def pull[A](p: Chunks[A]): Option[(Chunk[A], Chunks[A])] =
    summon[Stream[Producer, okay.Pure]].uncons(p).runWith

  /**
   * The chunk-in, chunk-out transformers: each stage is a tight array
   * pass and the result is still Chunks, so downstream keeps the
   * amortization. Spelled as functions, like Stream.map — the postfix
   * names belong to the monad (Free's map transforms the answer). The
   * op-value casts are the identity-signature discipline, as in the
   * Producer stream instance.
   */
  /**
   * Map, with the element type carried as far as the call site knows
   * it. `inline` here and an ordinary recursion below: the chunk
   * mapper is specialized ONCE, where `B` may still be concrete, and
   * handed to the recursion as a value — so every chunk it maps is
   * unboxed, not only the first. Where `B` is abstract (through
   * `Pipeline`, or any generic caller) `mapper` falls back and this
   * is exactly the code it was before.
   */
  inline def map[A, B](p: Chunks[A])(inline f: A => B): Chunks[B] =
    mapWith(p)(ChunkBuf.mapper[A, B](f))

  /** the recursion behind the inline `map`. Public because an inline
   * method may only reach members at least as accessible as itself —
   * a private one makes the compiler synthesize an accessor whose
   * name is unstable across compiler versions, which breaks a
   * downstream JAR on a mere recompile. */
  def mapWith[A, B](p: Chunks[A])(g: Chunk[A] => Chunk[B]): Chunks[B] = defer:
    (p.resume: @unchecked) match
      case Pure(_) => end
      case Inject(c) => produce(g(c))
      case Bind(Inject(c), k) =>
        produce(g(bound[A](c))).flatMap(_ => mapWith(k(c))(g))

  /** keep the elements satisfying pred (empty result chunks are skipped) */
  inline def filter[A](p: Chunks[A])(inline pred: A => Boolean): Chunks[A] =
    filterWith(p)(ChunkBuf.filterer[A](pred))

  /** the recursion behind the inline `filter` — public for the same
   * binary-compatibility reason as `mapWith` */
  def filterWith[A](p: Chunks[A])(g: Chunk[A] => Chunk[A]): Chunks[A] = defer:
    (p.resume: @unchecked) match
      case Pure(_) => end
      case Inject(c) => produce(g(c))
      case Bind(Inject(c), k) =>
        val fc = g(bound[A](c))
        if fc.isEmpty then filterWith(k(c))(g)
        else produce(fc).flatMap(_ => filterWith(k(c))(g))

  /** the first n elements (the last chunk truncated) */
  def take[A](p: Chunks[A])(n: Int): Chunks[A] = defer:
    if n <= 0 then end
    else (p.resume: @unchecked) match
      case Pure(_) => end
      case Inject(c) => produce(c.take(n))
      case Bind(Inject(c), k) =>
        val ca = bound[A](c)
        if ca.length >= n then produce(ca.take(n))
        else produce(ca).flatMap(_ => take(k(c))(n - ca.length))

  /** all but the first n elements */
  def drop[A](p: Chunks[A])(n: Int): Chunks[A] = defer:
    if n <= 0 then p
    else (p.resume: @unchecked) match
      case Pure(_) => end
      case Inject(c) => produce(c.drop(n))
      case Bind(Inject(c), k) =>
        val ca = bound[A](c)
        if ca.length <= n then drop(k(c))(n - ca.length)
        else produce(ca.drop(n)).flatMap(_ => k(c))

  /** the longest prefix satisfying pred */
  def takeWhile[A](p: Chunks[A])(pred: A => Boolean): Chunks[A] = defer:
    (p.resume: @unchecked) match
      case Pure(_) => end
      case Inject(c) =>
        val ca = bound[A](c)
        produce(ca.takeWhile(pred))
      case Bind(Inject(c), k) =>
        val ca = bound[A](c)
        val i = ca.indexWhere(a => !pred(a))
        if i < 0 then produce(ca).flatMap(_ => takeWhile(k(c))(pred))
        else produce(ca.take(i))

  /** the rest, after the longest prefix satisfying pred */
  def dropWhile[A](p: Chunks[A])(pred: A => Boolean): Chunks[A] = defer:
    (p.resume: @unchecked) match
      case Pure(_) => end
      case Inject(c) => produce(c.dropWhile(pred))
      case Bind(Inject(c), k) =>
        val ca = bound[A](c)
        val i = ca.indexWhere(a => !pred(a))
        if i < 0 then dropWhile(k(c))(pred)
        else if i == 0 then k(c)
        else produce(ca.drop(i)).flatMap(_ => k(c))

  /**
   * The terminal, SPECIALIZED: the step is known where the fold is
   * written, so it inlines and nothing boxes.
   *
   * The same move as `ChunkBuf.mapper`, for the same reason and with a
   * larger payoff. `Fold[A, S]` is `add(s: S, a: A): S`, generic in
   * both — so summing a `Chunks[Long]` boxes the accumulator on the way
   * in and again on the way out, and the element on the way in, three
   * allocations per element, none of which a megamorphic call site
   * lets the JIT remove. Measured over 10k Longs in chunks of 64:
   * `fold` 36.1us, this 2.5us — 14x, and it lands on the floor (a hand
   * loop over the same chunks is 2.6us).
   *
   * `fold` stays for folds that arrive as DATA — an `Aggregator`'s,
   * a Collector's, one chosen at runtime — where there is nothing to
   * inline and no way around the interface. Those are also the
   * distributed paths, where 36us per 10k is noise.
   */
  inline def foldLeft[A, S](p: Chunks[A])(z: S)(inline f: (S, A) => S): S =
    var s = z
    val it = summon[Stream[Producer, okay.Pure]].iterator(p)
    while it.hasNext do
      val c = it.next()
      var i = 0
      while i < c.length do
        s = f(s, c(i))
        i += 1
    s

  /** how many elements — `foldLeft`, so the counter stays a `long` */
  inline def count[A](p: Chunks[A]): Long = foldLeft(p)(0L)((n, _) => n + 1L)

  /**
   * The terminal: run a Fold, an inner while per chunk.
   *
   * For a fold that arrives as DATA there is no step to inline, so the
   * one thing left is to ask what its accumulator is. The four
   * specialized shapes declare their step where the type is already
   * primitive, so `addLong` and friends erase unboxed and the
   * accumulator stays in a register for the whole scan — which the
   * measurements say is essentially all of the cost. The element stays
   * generic and boxed on read, because `A` is not known here and,
   * measured, that half is nearly free.
   */
  def fold[A, S](p: Chunks[A])(using fo: Fold[A, S]): S = fo match
    // the element type is erased, so these tests see only the shape —
    // the same unavoidable `@unchecked` any collection's type test has
    case l: Fold.OfLong[A @unchecked] =>
      foldLeft(p)(l.initLong)((s, a) => l.addLong(s, a))
    case i: Fold.OfInt[A @unchecked] =>
      foldLeft(p)(i.initInt)((s, a) => i.addInt(s, a))
    case d: Fold.OfDouble[A @unchecked] =>
      foldLeft(p)(d.initDouble)((s, a) => d.addDouble(s, a))
    case b: Fold.OfBoolean[A @unchecked] =>
      foldLeft(p)(b.initBoolean)((s, a) => b.addBoolean(s, a))
    case _ =>
      var s = fo.init
      val it = summon[Stream[Producer, okay.Pure]].iterator(p)
      while it.hasNext do
        val c = it.next()
        var i = 0
        while i < c.length do
          s = fo.add(s, c(i))
          i += 1
      s

  /**
   * The writer carrier's own chunk-aware fold — the analogue of
   * `foldLeft` above, for a `Feed[Chunk[A]]` (or `Source[Chunk[A]]`
   * under G) instead of a `Chunks[A]` (producer-to-writer-carrier,
   * the chunk-aware fold stage 0 found missing).
   *
   * PARITY, MEASURED, when called directly with a literal step: a
   * fresh `Chunks.foldLeftWriter(feed)(z)(literal step)` call site
   * measures WITHIN NOISE of `Chunks.foldLeft` — three rounds,
   * medians 5.00 vs 4.76 us/op (N=10000/64) — this is the shape
   * `okay-cluster`'s own call sites (`Flows.scala`, `Job.scala`)
   * already use, calling `Chunks.foldLeft` with a literal step, never
   * through a `Fold` instance. Built on `Writer.foldWith`: it steps
   * once per TOLD CHUNK, exactly like the bare `Writer.fold` below;
   * the per-element `while` runs INSIDE that one step, in the SAME
   * non-recursive scope where `f` is written, so the `inline`
   * parameter substitutes cleanly (a first, hand-rolled walker put
   * the per-element loop inside a recursive local `loop` instead and
   * measured 17.6us — `inline` does not follow a call through a
   * nested recursive def's own boundary).
   *
   * `foldWriter` BELOW — the `Fold`-INSTANCE-dispatched form, what
   * `Chunks.fold`/`agg.fold` need (`Bulk.scala`, `Pipeline.scala`,
   * `Acceptance.scala`) — does NOT reach this parity, and the gap is
   * real, not measurement noise: three rounds, median 17.30 us/op,
   * across three shapes tried (a hand-rolled walker, this `foldWith`-
   * based one, and this one with `foldWriter` ALSO marked `inline`) —
   * 3.5x `foldLeftWriter` called directly (5.00, SAME carrier, SAME
   * per-element arithmetic, no dispatch — isolates the dispatch tax
   * specifically) and 6.8x `Chunks.fold`'s own Fold-dispatched 2.54.
   * The one isolating fact in hand: the ONLY difference between the
   * fast direct call and the slow dispatched one is that the
   * dispatched step captures the MATCHED `Fold.OfLong` instance and
   * calls a method ON it (`l.addLong(s,a)`) rather than being a fully
   * literal expression (`s + a`) — so the cost is a virtual dispatch
   * that fails to devirtualize somewhere in the three layers of
   * `inline` this shape asks the compiler to flatten, not (as first
   * suspected) the recursive-def boundary, which the `foldWith`-based
   * rewrite already fixed for the direct case. `Chunks.fold` has the
   * identical `l.addLong` call and does NOT pay this cost, so the
   * difference is specific to going through THIS combinator's extra
   * inlined layers, not to virtual dispatch on a matched `Fold` in
   * general — diagnosing further needs a profiler this environment
   * does not have (JITWatch or `-prof perfasm`); see specs/producer-
   * to-writer-carrier.md's Results and the sprint entry before
   * retrying, so a second attempt does not repeat the same three.
   *
   * SHIP THE HALF THAT WORKS: `foldLeftWriter` is safe to use now, at
   * parity. `foldWriter` stays, correct and tested, for call sites
   * that need `Fold`-instance dispatch — Producer stays the faster
   * choice for THOSE until this gap closes.
   */
  inline def foldLeftWriter[A, S, G[+_]](p: Unit ! (Writer % Chunk[A] + G))(z: S)
                                        (inline f: (S, A) => S)
                                        (using okay.TypeableK[Writer % Chunk[A]]): (S, Unit) ! G =
    Writer.foldWith[Chunk[A], S, Unit, G](p)(z): (s, c) =>
      var acc = s
      var i = 0
      while i < c.length do
        acc = f(acc, c(i))
        i += 1
      acc

  /**
   * `foldLeftWriter`, dispatched on a `Fold` instance the way
   * `Chunks.fold` dispatches on one — CORRECT (tested against
   * `Chunks.fold` on the same data) but NOT at parity with it; see
   * `foldLeftWriter`'s own doc for the measured gap. Kept non-`inline`:
   * marking it `inline` was one of several shapes tried and measured
   * no faster, and non-`inline` at least keeps a literal
   * `G=Nothing`/`Pure` compiling at a fresh call site (an unrelated
   * inliner limitation `foldLeftWriter`, `inline`, cannot avoid: use
   * `G=Async` there instead).
   *
   * ROOT CAUSE, FOUND (2026-09-19, `-prof gc`/`-prof jfr`/`javap` — all
   * in-JDK, no external profiler needed; an earlier draft of this
   * comment said the diagnosis needed one, which was wrong). `-prof gc`
   * on a fresh call: 249,584 B/op dispatched vs 12,656 for
   * `foldLeftWriter` called directly — ~10,000 boxed `java.lang.Long`,
   * one per element (N=10000). `-prof jfr`'s allocation stack traces
   * name it exactly: `ArraySeq$ofLong.apply` -> `boxToLong` ->
   * `Fold$OfLong.addLong`, present in the dispatched path's profile and
   * absent from the direct call's. The box itself is NOT the defect —
   * `Fold.OfLong[A]`'s own `addLong(s: Long, a: A): Long` takes its
   * element generically by design (a fold over `A`, not over `Long`),
   * so a synthetic bridge boxes on every call; `Chunks.fold` makes the
   * IDENTICAL call (confirmed via `javap`: same `ArraySeq.apply` ->
   * `boxToLong` -> `addLong` bytecode sequence) and pays nothing,
   * because the JIT's escape analysis proves the box never escapes
   * `Chunks.foldLeft`'s small, standalone compiled loop and eliminates
   * it. The SAME analysis fails inside `Writer.foldWith`'s bigger
   * resume/split/Bind tailrec trampoline, so the box becomes a real
   * allocation there.
   *
   * RULED OUT, with evidence: raising `-XX:MaxInlineLevel` and
   * `-XX:FreqInlineSize` well past their defaults changed nothing
   * (still exactly 249,584 B/op) — not a simple inlining-budget
   * problem. Pulling the per-chunk consuming loop into its own small,
   * standalone method (`private def foldChunkLong(c, z, l): Long`,
   * tried and reverted) ALSO changed nothing: `javap` confirms the JIT
   * re-inlines it straight back into the trampoline (it is a small,
   * hot, `invokespecial` callee — exactly what C2 inlines by default),
   * reproducing the same combined compiled unit either way. Escape
   * analysis is not gated by SOURCE-level method boundaries, only by
   * what actually ends up in one compiled unit after the JIT's own
   * inlining decisions — Scala-level refactoring cannot out-maneuver
   * that on its own.
   *
   * FIXED (2026-09-19), by doing exactly what the paragraph above this
   * one used to say was blocked: `writerStreamIn`'s `.iterator` walks
   * the tree through repeated small `uncons` calls instead of
   * `Writer.foldWith`'s fused resume/split/Bind trampoline, so the
   * per-chunk consuming loop below sits in its own small compiled
   * unit — and escape analysis eliminates the ELEMENT box there, the
   * ~10,000-boxed-`Long`s problem this doc used to describe. First cut
   * (producer-writer-carrier-foldwriter-eager, using `.iterator`'s
   * DEFAULT `Iterator.unfold` implementation): 18.14 -> 6.29 us/op,
   * 249,584 -> 32,952 B/op. Second cut, same day
   * (writer-stream-specialized-iterator): `writerStreamIn` grew its
   * OWN hand-specialized, mutable-state `iterator` override — mirroring
   * `Stream[Producer, Pure]`'s own override in Generate.scala, using
   * `Handler[G].handle` (comonadic, one value per forwarded operation)
   * instead of building and running a program per step — closing the
   * `Option`+`Either`+`Free`-node-per-chunk tax the default walk still
   * paid. MEASURED, 3 rounds, N=10000/64: 6.29 -> 5.43 us/op, 32,952 ->
   * 12,688 B/op — now matching `foldLeftWriter`'s own direct-call
   * baseline (12,656 B/op, 5.0us) almost exactly; `-prof jfr` confirms
   * `Right`/`Some` samples are gone entirely. Total from the original
   * dispatched form: 18.14 -> 5.43 us/op (3.3x faster), 249,584 ->
   * 12,688 B/op (19.7x less garbage).
   *
   * The remaining ~2x against `Chunks.fold`'s 2.55us is NOT a Writer
   * defect: it is the SAME gap `foldLeftWriter`'s own direct call
   * already has and was accepted as "at parity" for (its own doc,
   * above) — the cost of walking a `Free`-tree program at all
   * (`resume`, `Bind` chains) versus `Chunks.foldLeft`'s specialized,
   * non-program iterator. Closing THAT is a different, larger question
   * than this combinator's own dispatch tax, which is what this doc
   * originally set out to fix.
   * CORRECTED (producer-writer-carrier-pure-iterator, 2026-09-19): the
   * paragraph above is wrong about the cause. Both numbers it compares
   * (5.0 and 5.43) are loops written INSIDE a JMH benchmark method;
   * the same loop in an ordinary method measures 2.63 over the PURE
   * writer stream's `iterator` (Writer.scala) against `Chunks.fold`'s
   * 2.53 — parity, `Say` node included. `Chunks[A]` is Pure, so THAT
   * is the walk a retyped `Chunks.fold` takes; this combinator and
   * `foldLeftWriter` serve a genuinely G-effectful `Source[Chunk[A]]`
   * only. Spec Results has the table.
   *
   * The "API contract" obstacle the earlier draft worried about is
   * real but not a blocker: `.iterator` needs a `Handler[G]` and runs
   * eagerly, so this signature is narrowed from an arbitrary `G[+_]`
   * to `Async` specifically, gated on `CanBlock` (the same capability
   * every other blocking door in this library already requires) —
   * `async { ... }` wraps that eager walk back into a SUSPENDED
   * program (`Async.Run`, not run until `.runWith`), so the RETURN
   * value is still composable, only the row it accepts is narrower.
   * This was safe to do because `foldWriter` had ZERO production call
   * sites when this landed, so no caller's contract broke.
   *
   * IT DOES NOT ACTUALLY UNBLOCK `Bulk.scala`/`Pipeline.scala`/
   * `Acceptance.scala` (2026-09-19, corrected) — an earlier version of
   * this comment claimed it did, because their `G` is `Async`-shaped;
   * that missed that `CanBlock` is ALSO required, and all three live
   * in cross-platform shared source that also builds for JS, where
   * there is no `CanBlock` and no `Handler[Async]` at all
   * (`src/main/scala-js/Platform.scala`: JS drives `Async` through a
   * callback-based `Scheduler`/`Fiber`, not blocking). A future caller
   * needing a truly arbitrary `G` — or specifically a JS-compatible
   * walk, driven by callbacks/the event loop rather than an eager
   * blocking `Iterator` — would need its own overload and its own
   * design pass; not written here. See
   * backlog.d/okay-core/foldwriter-js-incompatible.md.
   */
  def foldWriter[A, S](p: Unit ! (Writer % Chunk[A] + Async))(using fo: Fold[A, S])
                       (using CanBlock): (S, Unit) ! Async =
    async:
      val it = writerStreamIn[Unit, Async].iterator(p)
      val result: S = fo match
        case l: Fold.OfLong[A @unchecked] =>
          var s = l.initLong
          while it.hasNext do
            val c = it.next(); var i = 0
            while i < c.length do { s = l.addLong(s, c(i)); i += 1 }
          s
        case n: Fold.OfInt[A @unchecked] =>
          var s = n.initInt
          while it.hasNext do
            val c = it.next(); var i = 0
            while i < c.length do { s = n.addInt(s, c(i)); i += 1 }
          s
        case d: Fold.OfDouble[A @unchecked] =>
          var s = d.initDouble
          while it.hasNext do
            val c = it.next(); var i = 0
            while i < c.length do { s = d.addDouble(s, c(i)); i += 1 }
          s
        case b: Fold.OfBoolean[A @unchecked] =>
          var s = b.initBoolean
          while it.hasNext do
            val c = it.next(); var i = 0
            while i < c.length do { s = b.addBoolean(s, c(i)); i += 1 }
          s
        case _ =>
          var s = fo.init
          while it.hasNext do
            val c = it.next(); var i = 0
            while i < c.length do { s = fo.add(s, c(i)); i += 1 }
          s
      (result, ())

  /**
   * Pair two chunked streams elementwise, realigning chunk boundaries:
   * each emitted chunk is the overlap window of the two current
   * chunks; the stream ends at the shorter side. Lazy, one window at
   * a time.
   */
  def zip[A, B](pa: Chunks[A], pb: Chunks[B]): Chunks[(A, B)] =
    def go(ca: Chunk[A], ia: Int, ra: Chunks[A],
           cb: Chunk[B], ib: Int, rb: Chunks[B]): Chunks[(A, B)] = defer:
      if ia >= ca.length then pull(ra) match
        case None => end
        case Some((c, r)) => go(c, 0, r, cb, ib, rb)
      else if ib >= cb.length then pull(rb) match
        case None => end
        case Some((c, r)) => go(ca, ia, ra, c, 0, r)
      else
        val n = math.min(ca.length - ia, cb.length - ib)
        val buf = ChunkBuf[(A, B)](n)
        var i = 0
        while i < n do
          buf(i) = (ca(ia + i), cb(ib + i))
          i += 1
        produce(buf.chunk).flatMap(_ => go(ca, ia + n, ra, cb, ib + n, rb))

    go(emptyChunk, 0, pa, emptyChunk, 0, pb)

  /**
   * Normalize chunk sizes (the content unchanged, the tail shorter):
   * filter shrinks chunks and merge mixes sizes — rechunk restores the
   * amortization downstream. A full buffer is handed off, not copied.
   */
  inline def rechunk[A](p: Chunks[A])(size: Int = 64): Chunks[A] =
    rechunkWith(p)(ChunkBuf.factory[A](size))(size)

  /** the recursion behind the inline `rechunk` — public for the same
   * binary-compatibility reason as `mapWith` */
  def rechunkWith[A](p: Chunks[A])(fresh: () => ChunkBuf[A])(size: Int): Chunks[A] =
    def go(buf: ChunkBuf[A], have: Int, rest: Chunks[A]): Chunks[A] = defer:
      pull(rest) match
        case None =>
          if have == 0 then end
          else produce(buf.take(have))
        case Some((c, r)) =>
          val room = size - have
          if c.length < room then
            var i = 0
            while i < c.length do
              buf(have + i) = c(i)
              i += 1
            go(buf, have + c.length, r)
          else
            var i = 0
            while i < room do
              buf(have + i) = c(i)
              i += 1
            val leftover = c.drop(room)
            val next = if leftover.isEmpty then r else produce(leftover).flatMap(_ => r)
            produce(buf.chunk).flatMap(_ => go(fresh(), 0, next))

    go(fresh(), 0, p)

  /**
   * Pipe a chunked producer into an ELEMENTWISE consumer: the
   * consumer's logic stays per element (Take.await), the transport
   * stays chunked — an await is served by an array index, the tree
   * steps once per chunk. The consumer drives; when the chunks end,
   * every further await answers None.
   */
  def pipe[W, B](p: Chunks[W])(c: B ! Take % W): B = {
    import scala.annotation.tailrec

    @tailrec def fetch(ch: Chunk[W], i: Int, rest: Chunks[W]): (Option[W], Chunk[W], Int, Chunks[W]) =
      if i < ch.length then (Some(ch(i)), ch, i + 1, rest)
      else pull(rest) match
        case Some((c2, r)) => fetch(c2, 0, r)
        case None => (None, ch, i, end)

    @tailrec def loop(ch: Chunk[W], i: Int, rest: Chunks[W], c: B ! Take % W): B = (c.resume: @unchecked) match
      case Pure(b) => b
      case Inject(Take.Await()) => fetch(ch, i, rest)._1
      case Bind(Inject(Take.Await()), k) =>
        val (o, ch2, i2, r2) = fetch(ch, i, rest)
        loop(ch2, i2, r2, k(o))

    loop(emptyChunk, 0, p, c)
  }

  /** the non-specializing chunk map, for callers that hold `f` as a
   * value (Parallel's per-chunk fibers); `ChunkBuf.mapper` is the
   * specializing form the inline `map` uses */
  private[okay] def mapChunk[A, B](c: Chunk[A])(f: A => B): Chunk[B] =
    val n = c.length
    val buf = ChunkBuf[B](n)
    var i = 0
    while i < n do
      buf(i) = f(c(i))
      i += 1
    buf.chunk

  extension [A](p: Chunks[A])
    /** the element view: one tree step per chunk, an index per element */
    def elements: Iterator[A] =
      // TRIED AND REFUTED (close-the-gaps, 2026-09-06): one cursor
      // over the chunk walk instead of `flatMap(_.iterator)`, to
      // drop the second iterator protocol per element. A/B medians
      // 23.3 -> 22.5, inside the run's own noise (controls moved
      // 4-5%). The per-element cost here is boxing through
      // Iterator[A], not the protocol; the chunk-native path
      // (`Chunks.map`/`fold`) is the one that avoids it, at 9.5.
      summon[Stream[Producer, okay.Pure]].iterator(p).flatMap(_.iterator)

    /** the chunks, memoized (first-order: see merge) */
    def toLazyList: LazyList[Chunk[A]] =
      LazyList.from(summon[Stream[Producer, okay.Pure]].iterator(p))
}
