package okay

import scala.collection.immutable.ArraySeq

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
 * `ArraySeq.untagged.newBuilder[A]` would remove the casts outright,
 * with no ClassTag — measured at 5x per chunk, and a real class
 * (rather than this opaque one) at 2x. Both are in
 * `ChunkBuildBenchmark`, with the numbers in src/jmh/history.tsv.
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
