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
