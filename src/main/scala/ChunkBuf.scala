package okay

import scala.collection.immutable.ArraySeq
import scala.compiletime.summonFrom
import scala.reflect.ClassTag

/**
 * A chunk under construction: the ONE place in this library that
 * asserts an element type, and the one buffer there is.
 *
 * An OPAQUE TYPE over an EXISTENTIAL array, and both halves of that
 * are load-bearing.
 *
 * Opaque, so it has no runtime representation — the buffer IS the
 * array, with no object to allocate and no indirection to inline
 * away. (A value class would nearly do; an opaque type does it
 * without the corner cases where a value class boxes anyway.)
 *
 * Existential — `Array[?]`, not `Array[A]` — so it claims nothing
 * about its backing and nothing about it can be false. That is the
 * difference from two attempts that failed: `opaque type ChunkBuf[A]
 * = Array[A]` and a match type on the backing both made the buffer's
 * TYPE depend on `A`, and then a generically built buffer
 * contradicted itself with a ClassCastException. Here `apply` picks
 * the backing with `summonFrom` and the type says nothing either way;
 * writing dispatches through `ScalaRunTime.array_update`, which costs
 * nothing where there is nothing to unbox (a chunk of Strings: 66.9ns
 * against 64.5 for a ClassTag'd array) and pays for itself many times
 * where there is (a chunk of Longs: 14.8ns against 90.9 boxed).
 *
 * Four alternatives were measured against it, per 64 elements:
 * a real class instead of an opaque type 2x (escape analysis does not
 * remove it), `ArraySeq.untagged.newBuilder` 3.2x, `ArrayBuffer` ending
 * in `toArray` 3.9x, `ArrayBuffer` ending in `ArraySeq.from` 4.5x. And
 * two that were not slow but unsound, above. The numbers and the
 * refutations are in `ChunkBuildBenchmark` and src/jmh/history.tsv.
 */
opaque type ChunkBuf[A] = Array[?]

/**
 * A buffer that specializes AND survives across calls.
 *
 * `ChunkBuf` cannot do the first (its backing is fixed at
 * `Array[AnyRef]`) and `ChunkBuf.tabulate` cannot do the second (it
 * owns its whole fill loop inside one inline block). `fromIterator`
 * and `rechunk` need both: they fill from an external source, a pull
 * at a time, holding the buffer in between.
 *
 * The trick that makes it sound where two earlier attempts were not:
 * the backing type is EXISTENTIAL. `Array[?]` claims nothing, so
 * there is nothing to be false about when the fallback allocates an
 * `Array[AnyRef]` for what is statically a `TaggedBuf[Long]` — which
 * is exactly the case that crashed both `opaque type ChunkBuf[A] =
 * Array[A]` and the match type. Writing dispatches on the runtime
 * array instead.
 *
 * Measured, per 64 elements: at `Long` 43.1ns against 89.7 for the
 * boxed path — 2.1x — and at `String` 65.0 against 64.9, so the
 * dispatch costs nothing where there is nothing to unbox.
 * `ChunkBuf.tabulate` is still better where it applies (15.3ns), and
 * this is for where it cannot.
 */
object ChunkBuf {
  /** room for n elements */
  /**
   * Room for n elements, UNBOXED when the element type is known here.
   *
   * `summonFrom` finds a `ClassTag[A]` exactly when `A` is concrete at
   * the call site and falls back when it is not — and both answers are
   * honest, because the backing type is existential and claims
   * nothing. That is the difference from `opaque type ChunkBuf[A] =
   * Array[A]`, which was tried and crashed: there the type asserted a
   * backing the fallback had not built.
   */
  inline def apply[A](n: Int): ChunkBuf[A] =
    summonFrom {
      case ct: ClassTag[A] => ct.newArray(n)
      case _ => new Array[AnyRef](n)
    }

  /**
   * A buffer that is deliberately NOT specialized: what a caller with
   * an abstract element type gets, and what a test needs when it
   * wants to exercise the fallback on purpose.
   */
  def boxed[A](n: Int): ChunkBuf[A] = new Array[AnyRef](n)

  /** the same for a recursion that needs a fresh buffer per chunk and
   * cannot be inline itself — resolved once, where A may be concrete */
  inline def factory[A](size: Int): () => ChunkBuf[A] =
    summonFrom {
      case ct: ClassTag[A] => () => ct.newArray(size)
      case _ => () => new Array[AnyRef](size)
    }

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
    // an unknown size costs one pass to learn it, which is what the
    // `toArray` these callers used was doing internally anyway
    val (seq, n) = sized(xs)
    val buf = ChunkBuf[A](n)
    val it = seq.iterator
    var i = 0
    while it.hasNext && i < n do
      buf(i) = it.next()
      i += 1
    buf.take(i)

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
        val (seq, n) = sized(xs)
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

  /** the sequence with its size known: as is when it says, else
   * materialized once */
  private def sized[A](xs: IterableOnce[A]): (IterableOnce[A], Int) =
    val known = xs.knownSize
    if known >= 0 then (xs, known)
    else
      val v = xs.iterator.toVector
      (v, v.length)

  /** THE array kernel, once: an array whose component type is A's
   * runtime representation (boxed, or the specialized primitive)
   * IS a Chunk[A] — `update` told that lie at every write, this is
   * where the reading side repeats it. `raw` comes from reflection
   * (java.lang.reflect.Array.newInstance answers Object) */
  private def wrap[A](raw: AnyRef): Chunk[A] =
    ArraySeq.unsafeWrapArray(raw.asInstanceOf[Array[?]]).asInstanceOf[Chunk[A]]

  extension [A](buf: ChunkBuf[A]) {
    /** the assertion, once: what goes in is an A */
    inline def update(i: Int, a: A): Unit =
      scala.runtime.ScalaRunTime.array_update(buf, i, a)

    inline def capacity: Int = buf.length

    /** the chunk, whole — the array is not copied */
    inline def chunk: Chunk[A] = wrap[A](buf)

    /** trimmed to n, into an array of the SAME component type — or
     * every stream's short final chunk falls back to boxed */
    inline def take(n: Int): Chunk[A] =
      if n == buf.length then buf.chunk
      else
        val out = java.lang.reflect.Array.newInstance(buf.getClass.getComponentType, n)
        System.arraycopy(buf, 0, out, 0, n)
        wrap[A](out)
  }
}
