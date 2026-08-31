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
   * `Array[AnyRef]` and a cast, and NOT `Array[A]` — which would be
   * the obvious signature and does not work.
   *
   * The reason is upstream, in the producers. `generate`, `unfold`,
   * `fromIterator` and the rest are generic in their element type and
   * have no `ClassTag[A]`, so `new Array[A](size)` is not something
   * they can write. What they CAN allocate is `Array[AnyRef]`, fill
   * elementwise, and hand here. Give this method `Array[A]` and every
   * one of them stops compiling — the array they hold is
   * `Array[AnyRef]`, so `A` would infer as `AnyRef` and the result
   * would be a `Chunk[AnyRef]`, not a `Chunk[A]`.
   *
   * So the cast does not disappear if the signature changes; it moves
   * to each call site. Here it is one line, in one place, and it says
   * what it knows: the array was filled with `A`s and only `A`s.
   *
   * `ofChars` below is the exception that shows the rule — there the
   * element type is known statically, a primitive `Array[Char]` can
   * be allocated, and no cast is needed at all.
   */
  private[okay] inline def wrap[A](arr: Array[AnyRef]): Chunk[A] =
    ArraySeq.unsafeWrapArray(arr).asInstanceOf[Chunk[A]]

  /**
   * Unfold with a tight per-chunk loop: size elements of f over the
   * unfolding seed per emitted chunk. Construction is lazy — the
   * first chunk is computed at the first pull, one chunk at a time.
   */
  def generate[A, B](a: A)(f: A => B)(g: A => A)(size: Int = 64): Chunks[B] =
    def go(s: A): Chunks[B] = pure[Produce, Unit](()).flatMap: _ =>
      val arr = new Array[AnyRef](size)
      var cur = s
      var i = 0
      while i < size do
        arr(i) = f(cur).asInstanceOf[AnyRef]
        cur = g(cur)
        i += 1
      produce(wrap[B](arr)).flatMap(_ => go(cur))

    go(a)

  /** the numbers from until (exclusive), a short tail chunk if needed */
  def range(from: Long, until: Long, size: Int = 64): Chunks[Long] =
    def go(s: Long): Chunks[Long] = pure[Produce, Unit](()).flatMap: _ =>
      if s >= until then pure(ArraySeq.empty)
      else
        val n = math.min(size.toLong, until - s).toInt
        val arr = new Array[AnyRef](n)
        var i = 0
        while i < n do
          arr(i) = (s + i).asInstanceOf[AnyRef]
          i += 1
        produce(wrap[Long](arr)).flatMap(_ => go(s + n))

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
        val arr = new Array[AnyRef](size)
        var i = 0
        while i < size && it.hasNext do
          arr(i) = it.next().asInstanceOf[AnyRef]
          i += 1
        val c = if i == size then wrap[A](arr)
                else wrap[A](java.util.Arrays.copyOf(arr, i))
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
  def map[A, B](p: Chunks[A])(f: A => B): Chunks[B] = defer:
    (p.resume: @unchecked) match
      case Pure(_) => end
      case Effect(c) => produce(mapChunk(c.asInstanceOf[Chunk[A]])(f))
      case Bind(Effect(c), k) =>
        produce(mapChunk(c.asInstanceOf[Chunk[A]])(f)).flatMap(_ => map(k(c))(f))

  /** keep the elements satisfying pred (empty result chunks are skipped) */
  def filter[A](p: Chunks[A])(pred: A => Boolean): Chunks[A] = defer:
    (p.resume: @unchecked) match
      case Pure(_) => end
      case Effect(c) => produce(filterChunk(c.asInstanceOf[Chunk[A]])(pred))
      case Bind(Effect(c), k) =>
        val fc = filterChunk(c.asInstanceOf[Chunk[A]])(pred)
        if fc.isEmpty then filter(k(c))(pred)
        else produce(fc).flatMap(_ => filter(k(c))(pred))

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
        val arr = new Array[AnyRef](n)
        var i = 0
        while i < n do
          arr(i) = (ca(ia + i), cb(ib + i))
          i += 1
        produce(wrap[(A, B)](arr)).flatMap(_ => go(ca, ia + n, ra, cb, ib + n, rb))

    go(emptyChunk, 0, pa, emptyChunk, 0, pb)

  /**
   * Normalize chunk sizes (the content unchanged, the tail shorter):
   * filter shrinks chunks and merge mixes sizes — rechunk restores the
   * amortization downstream. A full buffer is handed off, not copied.
   */
  def rechunk[A](p: Chunks[A])(size: Int = 64): Chunks[A] =
    def go(buf: Array[AnyRef], have: Int, rest: Chunks[A]): Chunks[A] = defer:
      pull(rest) match
        case None =>
          if have == 0 then end
          else produce(wrap[A](java.util.Arrays.copyOf(buf, have)))
        case Some((c, r)) =>
          val room = size - have
          if c.length < room then
            var i = 0
            while i < c.length do
              buf(have + i) = c(i).asInstanceOf[AnyRef]
              i += 1
            go(buf, have + c.length, r)
          else
            var i = 0
            while i < room do
              buf(have + i) = c(i).asInstanceOf[AnyRef]
              i += 1
            val leftover = c.drop(room)
            val next = if leftover.isEmpty then r else produce(leftover).flatMap(_ => r)
            produce(wrap[A](buf)).flatMap(_ => go(new Array[AnyRef](size), 0, next))

    go(new Array[AnyRef](size), 0, p)

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

  private[okay] def mapChunk[A, B](c: Chunk[A])(f: A => B): Chunk[B] =
    val n = c.length
    val arr = new Array[AnyRef](n)
    var i = 0
    while i < n do
      arr(i) = f(c(i)).asInstanceOf[AnyRef]
      i += 1
    wrap[B](arr)

  private def filterChunk[A](c: Chunk[A])(pred: A => Boolean): Chunk[A] =
    val n = c.length
    val arr = new Array[AnyRef](n)
    var i = 0
    var j = 0
    while i < n do
      val a = c(i)
      if pred(a) then
        arr(j) = a.asInstanceOf[AnyRef]
        j += 1
      i += 1
    if j == n then c
    else wrap[A](java.util.Arrays.copyOf(arr, j))

  extension [A](p: Chunks[A])
    /** the element view: one tree step per chunk, an index per element */
    def elements: Iterator[A] =
      summon[Stream[Producer, okay.Pure]].iterator(p).flatMap(_.iterator)

    /** the chunks, memoized (first-order: see merge) */
    def toLazyList: LazyList[Chunk[A]] =
      LazyList.from(summon[Stream[Producer, okay.Pure]].iterator(p))
}
