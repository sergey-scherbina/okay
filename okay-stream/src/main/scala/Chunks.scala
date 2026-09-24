package okay

import scala.collection.immutable.ArraySeq

/**
 * A chunked stream is an ordinary pure writer stream of whole batches
 * — `Feed[Chunk[A]]`, nothing new in the stream layer, elements are
 * polymorphic. What changes is the arithmetic: the freer tree steps
 * once per CHUNK, and an element inside costs an array index — the
 * amortization the chunked runtimes (ZStream, fs2, kyo) are built on.
 * Generators below fill each chunk in a tight while-loop, so no tree
 * node is ever paid per element; merge of chunked streams is the
 * existing Channel.merge applied to Chunks values — one queue
 * operation per chunk, for free.
 *
 * It was `Producer[Chunk[A]]` until producer-to-writer-carrier
 * (2026-09-19): the identity signature put the chunk in the ANSWER
 * position, so `pure(c)` type-checked as a chunk and emitted nothing.
 * On the writer carrier a chunk is `Writer.tell(c)` — a `Say` node the
 * walk matches, `Handler[Pure]` only — and the stream instance is
 * `Stream[[W] =>> Unit ! Writer % W, Pure]` (Writer.scala), whose
 * hand-specialized `iterator` measured at parity with the producer
 * walk (specs/producer-to-writer-carrier.md, Results).
 */
type Chunks[A] = Feed[Chunk[A]]

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
    def go(s: A): Chunks[B] = defer:
      val (c, cur) = fill(s)
      Writer.tell(c).flatMap(_ => go(cur))

    go(a)

  /** the numbers from until (exclusive), a short tail chunk if needed */
  def range(from: Long, until: Long, size: Int = 64): Chunks[Long] =
    def go(s: Long): Chunks[Long] = defer:
      if s >= until then end
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
        Writer.tell(ArraySeq.unsafeWrapArray(arr)).flatMap(_ => go(s + n))

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
        Writer.tell(scala.collection.immutable.ArraySeq.unsafeWrapArray(arr))
          .flatMap(_ => go(from + n))

    go(0)

  inline def fromIterator[A](it: Iterator[A], size: Int = 64): Chunks[A] =
    fromIteratorWith(it)(ChunkBuf.factory[A](size))(size)

  /** the recursion behind the inline `fromIterator` — public for the
   * same binary-compatibility reason as `mapWith` */
  def fromIteratorWith[A](it: Iterator[A])(fresh: () => ChunkBuf[A])
                         (size: Int): Chunks[A] =
    def go(): Chunks[A] = defer:
      if !it.hasNext then end
      else
        val buf = fresh()
        var i = 0
        while i < size && it.hasNext do
          buf(i) = it.next()
          i += 1
        val c = buf.take(i)
        Writer.tell(c).flatMap(_ => go())

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
  private[okay] inline def defer[X](inline x: => Feed[X]): Feed[X] =
    pure[Writer % X, Unit](()).flatMap(_ => x)

  /** the empty chunk — what a G-effectful `Chunk[X] ! Produce + G`
   * ends in (its final `Pure`, which its stream instance never reads),
   * and the `end` a `Source.toProducer` over chunks is given. A
   * `Chunks[A]` itself ends in `()` now. Public since
   * producer-drains: a consumer spelling `ArraySeq.empty` and hoping
   * it was the same thing was right, and should not have to hope */
  def emptyChunk[B]: Chunk[B] = ArraySeq.empty[AnyRef].asInstanceOf[Chunk[B]]

  /** the end of a chunked stream: its answer, which is no chunk */
  private[okay] def end[B]: Chunks[B] = pure(())

  /** pull one chunk: the pure step of a chunked stream */
  private[okay] def pull[A](p: Chunks[A]): Option[(Chunk[A], Chunks[A])] =
    Writer.uncons(p).toOption

  /**
   * The chunk-in, chunk-out transformers: each stage is a tight array
   * pass and the result is still Chunks, so downstream keeps the
   * amortization. Spelled as functions, like Stream.map — the postfix
   * names belong to the monad (Free's map transforms the answer).
   * `Say` is Writer's only constructor, so the match refines the
   * chunk's type — no cast, unlike the identity signature this had.
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
      case Return(_) => end
      case Inject(Writer.Say(c)) => Writer.tell(g(c))
      case Bind(Inject(Writer.Say(c)), k) =>
        Writer.tell(g(c)).flatMap(_ => mapWith(k(()))(g))

  /** keep the elements satisfying pred (empty result chunks are skipped) */
  inline def filter[A](p: Chunks[A])(inline pred: A => Boolean): Chunks[A] =
    filterWith(p)(ChunkBuf.filterer[A](pred))

  /** the recursion behind the inline `filter` — public for the same
   * binary-compatibility reason as `mapWith` */
  def filterWith[A](p: Chunks[A])(g: Chunk[A] => Chunk[A]): Chunks[A] = defer:
    (p.resume: @unchecked) match
      case Return(_) => end
      case Inject(Writer.Say(c)) => Writer.tell(g(c))
      case Bind(Inject(Writer.Say(c)), k) =>
        val fc = g(c)
        if fc.isEmpty then filterWith(k(()))(g)
        else Writer.tell(fc).flatMap(_ => filterWith(k(()))(g))

  /** the first n elements (the last chunk truncated) */
  def take[A](p: Chunks[A])(n: Int): Chunks[A] = defer:
    if n <= 0 then end
    else (p.resume: @unchecked) match
      case Return(_) => end
      case Inject(Writer.Say(c)) => Writer.tell(c.take(n))
      case Bind(Inject(Writer.Say(c)), k) =>
        if c.length >= n then Writer.tell(c.take(n))
        else Writer.tell(c).flatMap(_ => take(k(()))(n - c.length))

  /** all but the first n elements */
  def drop[A](p: Chunks[A])(n: Int): Chunks[A] = defer:
    if n <= 0 then p
    else (p.resume: @unchecked) match
      case Return(_) => end
      case Inject(Writer.Say(c)) => Writer.tell(c.drop(n))
      case Bind(Inject(Writer.Say(c)), k) =>
        if c.length <= n then drop(k(()))(n - c.length)
        else Writer.tell(c.drop(n)).flatMap(_ => k(()))

  /** the longest prefix satisfying pred */
  def takeWhile[A](p: Chunks[A])(pred: A => Boolean): Chunks[A] = defer:
    (p.resume: @unchecked) match
      case Return(_) => end
      case Inject(Writer.Say(c)) =>
        Writer.tell(c.takeWhile(pred))
      case Bind(Inject(Writer.Say(c)), k) =>
        val i = c.indexWhere(a => !pred(a))
        if i < 0 then Writer.tell(c).flatMap(_ => takeWhile(k(()))(pred))
        else Writer.tell(c.take(i))

  /** the rest, after the longest prefix satisfying pred */
  def dropWhile[A](p: Chunks[A])(pred: A => Boolean): Chunks[A] = defer:
    (p.resume: @unchecked) match
      case Return(_) => end
      case Inject(Writer.Say(c)) => Writer.tell(c.dropWhile(pred))
      case Bind(Inject(Writer.Say(c)), k) =>
        val i = c.indexWhere(a => !pred(a))
        if i < 0 then dropWhile(k(()))(pred)
        else if i == 0 then k(())
        else Writer.tell(c.drop(i)).flatMap(_ => k(()))

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
    val it = feedStream[Unit].iterator(p)
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
      val it = feedStream[Unit].iterator(p)
      while it.hasNext do
        val c = it.next()
        var i = 0
        while i < c.length do
          s = fo.add(s, c(i))
          i += 1
      s

  /**
   * `fold` with a stop (specs/fold-until.md): `done` is checked per
   * element inside the chunk's `while`, and BEFORE `hasNext` pulls
   * the next chunk — the second check is the one that saves a
   * production, since the iterator's `hasNext` is what steps the
   * tree.
   */
  def foldUntil[A, S, R](p: Chunks[A])(using fo: FoldUntil[A, S, R]): R = fo match
    // dispatched on the accumulator as `fold` is (fold-until-unboxed:
    // 27.9 us boxed against 1.0 for the same loop at `long`)
    case l: FoldUntil.OfLong[A @unchecked, R @unchecked] =>
      var s = l.initLong
      val it = feedStream[Unit].iterator(p)
      while !l.doneLong(s) && it.hasNext do
        val c = it.next()
        var i = 0
        while i < c.length && !l.doneLong(s) do
          s = l.addLong(s, c(i))
          i += 1
      l.endLong(s)
    case n: FoldUntil.OfInt[A @unchecked, R @unchecked] =>
      var s = n.initInt
      val it = feedStream[Unit].iterator(p)
      while !n.doneInt(s) && it.hasNext do
        val c = it.next()
        var i = 0
        while i < c.length && !n.doneInt(s) do
          s = n.addInt(s, c(i))
          i += 1
      n.endInt(s)
    case d: FoldUntil.OfDouble[A @unchecked, R @unchecked] =>
      var s = d.initDouble
      val it = feedStream[Unit].iterator(p)
      while !d.doneDouble(s) && it.hasNext do
        val c = it.next()
        var i = 0
        while i < c.length && !d.doneDouble(s) do
          s = d.addDouble(s, c(i))
          i += 1
      d.endDouble(s)
    case b: FoldUntil.OfBoolean[A @unchecked, R @unchecked] =>
      var s = b.initBoolean
      val it = feedStream[Unit].iterator(p)
      while !b.doneBoolean(s) && it.hasNext do
        val c = it.next()
        var i = 0
        while i < c.length && !b.doneBoolean(s) do
          s = b.addBoolean(s, c(i))
          i += 1
      b.endBoolean(s)
    case _ =>
      var s = fo.init
      val it = feedStream[Unit].iterator(p)
      while !fo.done(s) && it.hasNext do
        val c = it.next()
        var i = 0
        while i < c.length && !fo.done(s) do
          s = fo.add(s, c(i))
          i += 1
      fo.end(s)

  /**
   * `foldLeft` for a G-EFFECTFUL chunked writer stream — a
   * `Source[Chunk[A]]`, or any `Unit ! Writer % Chunk[A] + G` — with
   * the step inlined into a per-chunk `while`, on `Writer.foldWith`.
   * A `Chunks[A]` is pure and folds with `foldLeft` above; this is for
   * the row that also performs G. The per-element loop sits in the
   * same non-recursive scope as `f`, which is what lets `inline`
   * substitute the step (a recursive local def would box it into a
   * `Function2` per element — measured, 3.5x). Parity with `foldLeft`
   * at a literal step; the investigation that built it, with every
   * number, is specs/producer-to-writer-carrier.md (Results).
   */
  inline def foldLeftWriter[A, S, G[+_]](p: Unit ! Writer % Chunk[A] + G)(z: S)
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
   * `fold` for a G-effectful chunked writer stream, dispatched on a
   * `Fold` instance the way `Chunks.fold` is — JVM and Native only:
   * it walks `writerStreamIn`'s eager `iterator` under `Handler[Async]`
   * (needs `CanBlock`, which JS does not have) and wraps the walk in
   * `async { ... }`, so the answer is still a suspended program. The
   * eager walk is the point: it gives the per-chunk loop its own small
   * compiled unit, where escape analysis removes the element box that
   * `Fold.OfLong.addLong`'s generic signature makes — inside
   * `Writer.foldWith`'s fused trampoline that box was real, one per
   * element, 3.3x slower and 20x the garbage. A `Chunks[A]` is pure
   * and folds with `fold` above; a JS caller of a G-effectful chunk
   * fold has no combinator yet and none has asked. The whole
   * investigation — three shapes tried, `-prof gc`/`jfr`, `javap`,
   * the two fixes that did nothing and the one that did — is
   * specs/producer-to-writer-carrier.md (Results).
   */
  def foldWriter[A, S](p: Unit ! Writer % Chunk[A] + Async)(using fo: Fold[A, S])
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
        Writer.tell(buf.chunk).flatMap(_ => go(ca, ia + n, ra, cb, ib + n, rb))

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
          else Writer.tell(buf.take(have))
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
            val next = if leftover.isEmpty then r else Writer.tell(leftover).flatMap(_ => r)
            Writer.tell(buf.chunk).flatMap(_ => go(fresh(), 0, next))

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
      case Return(b) => b
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
      feedStream[Unit].iterator(p).flatMap(_.iterator)

    /** the chunks, memoized (first-order: see merge) */
    def toLazyList: LazyList[Chunk[A]] =
      LazyList.from(feedStream[Unit].iterator(p))
}
