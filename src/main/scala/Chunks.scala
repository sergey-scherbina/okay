package okay

import scala.collection.immutable.ArraySeq

/** a chunk: an immutable indexed batch of elements (O(1) index, no
 * copy over the generation array) */
type Chunk[+A] = ArraySeq[A]

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
        val buf = ChunkBuf[Long](n)
        var i = 0
        while i < n do
          buf(i) = s + i
          i += 1
        produce(buf.chunk).flatMap(_ => go(s + n))

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

  def fromIterator[A](it: Iterator[A], size: Int = 64): Chunks[A] =
    def go(): Chunks[A] = pure[Produce, Unit](()).flatMap: _ =>
      if !it.hasNext then end
      else
        val buf = ChunkBuf[A](size)
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

  private[okay] def emptyChunk[B]: Chunk[B] = ArraySeq.empty[AnyRef].asInstanceOf[Chunk[B]]

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
      case Effect(c) => produce(g(c.asInstanceOf[Chunk[A]]))
      case Bind(Effect(c), k) =>
        produce(g(c.asInstanceOf[Chunk[A]])).flatMap(_ => mapWith(k(c))(g))

  /** keep the elements satisfying pred (empty result chunks are skipped) */
  inline def filter[A](p: Chunks[A])(inline pred: A => Boolean): Chunks[A] =
    filterWith(p)(ChunkBuf.filterer[A](pred))

  /** the recursion behind the inline `filter` — public for the same
   * binary-compatibility reason as `mapWith` */
  def filterWith[A](p: Chunks[A])(g: Chunk[A] => Chunk[A]): Chunks[A] = defer:
    (p.resume: @unchecked) match
      case Pure(_) => end
      case Effect(c) => produce(g(c.asInstanceOf[Chunk[A]]))
      case Bind(Effect(c), k) =>
        val fc = g(c.asInstanceOf[Chunk[A]])
        if fc.isEmpty then filterWith(k(c))(g)
        else produce(fc).flatMap(_ => filterWith(k(c))(g))

  /** the first n elements (the last chunk truncated) */
  def take[A](p: Chunks[A])(n: Int): Chunks[A] = defer:
    if n <= 0 then end
    else (p.resume: @unchecked) match
      case Pure(_) => end
      case Effect(c) => produce(c.asInstanceOf[Chunk[A]].take(n))
      case Bind(Effect(c), k) =>
        val ca = c.asInstanceOf[Chunk[A]]
        if ca.length >= n then produce(ca.take(n))
        else produce(ca).flatMap(_ => take(k(c))(n - ca.length))

  /** all but the first n elements */
  def drop[A](p: Chunks[A])(n: Int): Chunks[A] = defer:
    if n <= 0 then p
    else (p.resume: @unchecked) match
      case Pure(_) => end
      case Effect(c) => produce(c.asInstanceOf[Chunk[A]].drop(n))
      case Bind(Effect(c), k) =>
        val ca = c.asInstanceOf[Chunk[A]]
        if ca.length <= n then drop(k(c))(n - ca.length)
        else produce(ca.drop(n)).flatMap(_ => k(c))

  /** the longest prefix satisfying pred */
  def takeWhile[A](p: Chunks[A])(pred: A => Boolean): Chunks[A] = defer:
    (p.resume: @unchecked) match
      case Pure(_) => end
      case Effect(c) =>
        val ca = c.asInstanceOf[Chunk[A]]
        produce(ca.takeWhile(pred))
      case Bind(Effect(c), k) =>
        val ca = c.asInstanceOf[Chunk[A]]
        val i = ca.indexWhere(a => !pred(a))
        if i < 0 then produce(ca).flatMap(_ => takeWhile(k(c))(pred))
        else produce(ca.take(i))

  /** the rest, after the longest prefix satisfying pred */
  def dropWhile[A](p: Chunks[A])(pred: A => Boolean): Chunks[A] = defer:
    (p.resume: @unchecked) match
      case Pure(_) => end
      case Effect(c) => produce(c.asInstanceOf[Chunk[A]].dropWhile(pred))
      case Bind(Effect(c), k) =>
        val ca = c.asInstanceOf[Chunk[A]]
        val i = ca.indexWhere(a => !pred(a))
        if i < 0 then dropWhile(k(c))(pred)
        else if i == 0 then k(c)
        else produce(ca.drop(i)).flatMap(_ => k(c))

  /** the terminal: run a Fold, an inner while per chunk */
  def fold[A, S](p: Chunks[A])(using fo: Fold[A, S]): S =
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
  def rechunk[A](p: Chunks[A])(size: Int = 64): Chunks[A] =
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
            produce(buf.chunk).flatMap(_ => go(ChunkBuf[A](size), 0, next))

    go(ChunkBuf[A](size), 0, p)

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
      case Effect(Take.Await()) => fetch(ch, i, rest)._1
      case Bind(Effect(Take.Await()), k) =>
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
      summon[Stream[Producer, okay.Pure]].iterator(p).flatMap(_.iterator)

    /** the chunks, memoized (first-order: see merge) */
    def toLazyList: LazyList[Chunk[A]] =
      LazyList.from(summon[Stream[Producer, okay.Pure]].iterator(p))
}
