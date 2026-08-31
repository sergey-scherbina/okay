package okay

import scala.collection.immutable.ArraySeq
import scala.compiletime.summonFrom
import scala.reflect.ClassTag

/**
 * A chunk under construction: the ONE place in this library that
 * asserts an element type.
 *
 * An OPAQUE TYPE over the same `Array[AnyRef]` the producers were
 * already allocating by hand — so it erases to exactly that, with no
 * object to allocate and no indirection to inline away. The generic
 * producers have no `ClassTag[A]` and cannot allocate `Array[A]`;
 * they fill an untyped array and assert once. That was true before,
 * but the assertions were spread over twenty-eight sites, one per
 * fill loop and one per caller conjuring an untyped array. Here
 * `update` takes an `A` and `chunk` answers a `Chunk[A]`, and no
 * caller casts anything.
 *
 * Four alternatives were tried and measured, per 64 elements, all in
 * `ChunkBuildBenchmark`:
 *
 *   this (opaque over Array[AnyRef])        84ns
 *   a real class instead of an opaque type  2x   (escape analysis
 *                                                does not remove it)
 *   ArraySeq.untagged.newBuilder            3.2x (no casts at all)
 *   ArrayBuffer, ending in toArray          3.9x
 *   ArrayBuffer, ending in ArraySeq.from    4.5x
 *
 * `ArrayBuffer` is the tidiest to read and the slowest to run, for
 * three reasons that stack: a capacity check on every append, an
 * `Array[AnyRef]` backing that boxes primitives whatever the element
 * type is, and a COPY to reach the immutable chunk it has to end as
 * (its internal array is private, so there is no way to hand it over
 * without copying). The third reason is the one that matters most
 * here: being always boxed, it would also foreclose the
 * specialization below, which is worth 21% on the stream lane.
 *
 * A `ClassTag[A]` would also remove them, and costs nothing at
 * runtime (a ClassTag'd `Array[String]` measured 65.7ns against this
 * one's 76.7 — the same). It was tried, and the cost is not speed but
 * REACH, in three widening waves:
 *
 *   1. the leaf builders — `generate`, `fromIterator`, `rechunk`,
 *      `mapChunk`, `filterChunk`, `Pipe.chunked`;
 *   2. then the combinators they back — `Chunks.map`, `.filter`,
 *      `nats`, `fibs`, which is the public streaming API, so every
 *      user's generic function over `Chunks[A]` needs the evidence
 *      too;
 *   3. then `Pipeline.chunks`, where it STOPS: the reified operator
 *      tree has existential intermediates (`case Mapped[A, B](src:
 *      Pipeline[A], f: A => B)`), and no ClassTag for `A` can be
 *      recovered when compiling a `Mapped` node. It would have to be
 *      stored IN the tree — a change to the data structure, not to a
 *      signature, and one that would then have to be produced at
 *      every place a pipeline is built.
 *
 * So the trade is not "cast versus ClassTag". It is three casts here
 * against a ClassTag in the public type of `map`, and a ClassTag
 * field in the operator tree that P6 exists to keep clean.
 *
 * And `ChunkBuf` ITSELF cannot be the specialized thing, which was
 * worth checking rather than assuming. `opaque type ChunkBuf[A] =
 * Array[A]` with an `inline` `apply` and `summonFrom` looks like it
 * should work — it compiles, a direct `ChunkBuf[Long](n)` really is a
 * `long[]`, and a generic caller falls back to `Object[]` and reads
 * back correctly. Then hand that generically-built buffer to anything
 * that knows `A = Long`:
 *
 *   ClassCastException: [Ljava.lang.Object; cannot be cast to [J
 *
 * Because inside this file `ChunkBuf[Long]` REDUCES to `Array[Long]`,
 * and for a buffer the fallback built, that is false. It is the match
 * type's failure again wearing a different shape, and for the same
 * reason: the BUFFER's type was made to depend on `A`.
 *
 * That is exactly what the specialization below avoids. `tabulate`,
 * `mapper`, `filterer` and `filler` answer a `Chunk[A]`, which is an
 * `ArraySeq[A]` — and `ArraySeq.ofLong` and an `ofRef` of boxed Longs
 * are BOTH honest `ArraySeq[Long]`. The specialization lives at the
 * boundary whose type tolerates either backing, never in a type that
 * claims one.
 *
 * A MATCH TYPE was tried too — `type Backing[A] = A match { case Long
 * => Array[Long]; … case _ => Array[AnyRef] }` with an
 * `inline erasedValue[A] match` to allocate. It specializes, and it
 * needs no ClassTag: asked for a `Long` buffer directly it produces
 * `long[]`. It also fails in the one place that matters, and fails
 * UNSOUNDLY.
 *
 * Called from a generic function — which is what `map`, `generate`
 * and `fromIterator` are — `A` is abstract, the inline match cannot
 * reduce, and it falls to the `Array[AnyRef]` branch. That much is
 * merely the same non-specialization we have. The problem is that the
 * TYPE still reduces: inside this file, where the extensions live,
 * `Backing[Long]` is `Array[Long]`, while the value built generically
 * is an `Object[]`. Reading it gives
 * `ClassCastException: [Ljava.lang.Object; cannot be cast to [J`.
 * Measured, not feared.
 *
 * The condition match-type specialization needs — the element type
 * static at the allocation site — is exactly the condition `Staged`
 * already exploits, and there the win is 10x (1.6us against this
 * path's 16.9), not a few percent. The chunked path exists for the
 * case where the type is NOT static, which is the case that cannot
 * be specialized by any of these means.
 */
opaque type ChunkBuf[A] = Array[AnyRef]

object ChunkBuf {
  /** room for n elements */
  def apply[A](n: Int): ChunkBuf[A] = new Array[AnyRef](n)

  /**
   * A chunk from something already sequenced.
   *
   * Every interop module was writing this by hand and each needed a
   * cast to do it — `xs.toArray[Any].asInstanceOf[Array[AnyRef]]`,
   * `it.asInstanceOf[Iterator[AnyRef]].toArray` — because `toArray`
   * wants a ClassTag and the only one available for an abstract `A`
   * is the one you get by lying about the type. Here the lie is
   * `update`'s, already told, once.
   */
  def of[A](xs: IterableOnce[A]): Chunk[A] =
    val known = xs.knownSize
    // an unknown size costs one pass to learn it, which is what the
    // `toArray` these callers used was doing internally anyway
    val seq = if known >= 0 then xs else xs.iterator.toVector
    val n = if known >= 0 then known else seq.asInstanceOf[Vector[A]].length
    val buf = ChunkBuf[A](n)
    val it = seq.iterator
    var i = 0
    while it.hasNext && i < n do
      buf(i) = it.next()
      i += 1
    buf.take(i)

  /**
   * An index-driven chunk that is UNBOXED when the element type is
   * known where it is written.
   *
   * The soundness that the match-type attempt lacked comes from
   * choosing only the BACKING, never the type: `ArraySeq.ofLong` and
   * an `ArraySeq.ofRef` of boxed Longs are both honest
   * `ArraySeq[Long]`, so either branch answers the type it promises.
   * `summonFrom` finds a `ClassTag[A]` exactly when `A` is concrete
   * at the call site and takes the other branch when it is not —
   * verified both ways in `TestChunkSpec`.
   *
   * It composes across inline hops: an inline caller of an inline
   * caller of this still specializes, and the first NON-inline
   * generic boundary falls back, correctly. That is the whole of what
   * can be threaded through staging, and it is not nothing — measured
   * on 64 Longs, unboxed against boxed: map 14.6ns against 117.8
   * (8.1x), construction 17.2 against 91.5 (5.3x), a summing read 8.7
   * against 18.3 (2.1x). `java.lang.Long` caches only -128..127, so
   * every other element is an allocation; the same experiment on
   * `Char` was worth 8%, which is why this had to be measured per
   * type and not assumed.
   *
   * Used where the fill is index-driven. `fromIterator` and `rechunk`
   * are driven by an external source and hold the buffer across
   * pulls, so they stay on the untyped path.
   */
  inline def tabulate[A](n: Int)(inline f: Int => A): Chunk[A] =
    summonFrom {
      case ct: ClassTag[A] =>
        val arr = ct.newArray(n)
        var i = 0
        while i < n do
          arr(i) = f(i)
          i += 1
        ArraySeq.unsafeWrapArray(arr)
      case _ =>
        val buf = ChunkBuf[A](n)
        var i = 0
        while i < n do
          buf(i) = f(i)
          i += 1
        buf.chunk
    }

  /**
   * A chunk-to-chunk map, SPECIALIZED once where it is written.
   *
   * `Chunks.map` recurses over the stream, and an inline method
   * cannot recurse — so the specialization is done here, at the call
   * site, and handed to the recursion as an ordinary function value
   * with the `ClassTag` captured in its closure. Every chunk the
   * recursion maps then lands in a `long[]`, not only the first.
   */
  inline def mapper[A, B](inline f: A => B): Chunk[A] => Chunk[B] =
    summonFrom {
      case ct: ClassTag[B] =>
        (c: Chunk[A]) =>
          val n = c.length
          val arr = ct.newArray(n)
          var i = 0
          while i < n do
            arr(i) = f(c(i))
            i += 1
          ArraySeq.unsafeWrapArray(arr)
      case _ =>
        (c: Chunk[A]) =>
          val n = c.length
          val buf = ChunkBuf[B](n)
          var i = 0
          while i < n do
            buf(i) = f(c(i))
            i += 1
          buf.chunk
    }

  /**
   * `mapper` when the ClassTag arrives as a VALUE rather than from
   * the call site — which is what a reified operator tree can offer:
   * it captured the tag when the node was built, and hands it back
   * when the node is compiled. Same specialization, from data.
   */
  def taggedMapper[A, B](f: A => B)(using ct: ClassTag[B]): Chunk[A] => Chunk[B] =
    (c: Chunk[A]) =>
      val n = c.length
      val arr = ct.newArray(n)
      var i = 0
      while i < n do
        arr(i) = f(c(i))
        i += 1
      ArraySeq.unsafeWrapArray(arr)

  /**
   * The same for filter. The result length is not known until the
   * pass is done, so it fills to the source's length and trims — and
   * returns the source itself when nothing was dropped, which is the
   * common case and costs no allocation at all.
   */
  inline def filterer[A](inline pred: A => Boolean): Chunk[A] => Chunk[A] =
    summonFrom {
      case ct: ClassTag[A] =>
        (c: Chunk[A]) =>
          val n = c.length
          val arr = ct.newArray(n)
          var i = 0
          var j = 0
          while i < n do
            val a = c(i)
            if pred(a) then
              arr(j) = a
              j += 1
            i += 1
          if j == n then c
          else ArraySeq.unsafeWrapArray(arr.slice(0, j))
      case _ =>
        (c: Chunk[A]) =>
          val n = c.length
          val buf = ChunkBuf[A](n)
          var i = 0
          var j = 0
          while i < n do
            val a = c(i)
            if pred(a) then
              buf(j) = a
              j += 1
            i += 1
          if j == n then c else buf.take(j)
    }

  /**
   * An unfold, specialized where the element type is known: the same
   * shape as `Chunks.generate`, which recurses, so the per-chunk
   * filler is built once here and handed to the recursion.
   */
  inline def filler[S, A](inline f: S => A)(inline g: S => S)(size: Int)
  : S => (Chunk[A], S) =
    summonFrom {
      case ct: ClassTag[A] =>
        (s0: S) =>
          val arr = ct.newArray(size)
          var cur = s0
          var i = 0
          while i < size do
            arr(i) = f(cur)
            cur = g(cur)
            i += 1
          (ArraySeq.unsafeWrapArray(arr), cur)
      case _ =>
        (s0: S) =>
          val buf = ChunkBuf[A](size)
          var cur = s0
          var i = 0
          while i < size do
            buf(i) = f(cur)
            cur = g(cur)
            i += 1
          (buf.chunk, cur)
    }

  /**
   * `of`, specialized: the interop modules that hand over an already
   * sequenced collection get an unboxed chunk when their element type
   * is concrete, which for Kafka records or JDBC rows it usually is.
   */
  inline def ofSpecialized[A](xs: IterableOnce[A]): Chunk[A] =
    summonFrom {
      case ct: ClassTag[A] =>
        val known = xs.knownSize
        val seq = if known >= 0 then xs else xs.iterator.toVector
        val n = if known >= 0 then known else seq.asInstanceOf[Vector[A]].length
        val arr = ct.newArray(n)
        val it = seq.iterator
        var i = 0
        while it.hasNext && i < n do
          arr(i) = it.next()
          i += 1
        if i == n then ArraySeq.unsafeWrapArray(arr)
        else ArraySeq.unsafeWrapArray(arr.slice(0, i))
      case _ => of(xs)
    }

  extension [A](buf: ChunkBuf[A]) {
    /** the assertion, once: what goes in is an A */
    inline def update(i: Int, a: A): Unit = buf(i) = a.asInstanceOf[AnyRef]

    inline def capacity: Int = buf.length

    /** the chunk, whole — the array is not copied */
    inline def chunk: Chunk[A] =
      ArraySeq.unsafeWrapArray(buf).asInstanceOf[Chunk[A]]

    /** the chunk, trimmed to n — for a source that ended early */
    inline def take(n: Int): Chunk[A] =
      if n == buf.length then buf.chunk
      else ArraySeq.unsafeWrapArray(java.util.Arrays.copyOf(buf, n))
        .asInstanceOf[Chunk[A]]
  }
}
