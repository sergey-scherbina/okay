package okay.java

import okay.{Chunk, ChunkBuf, Chunks}
import java.util.Spliterator
import java.util.function.{Consumer, DoubleConsumer, IntConsumer, LongConsumer}
import java.util.stream.{DoubleStream, IntStream, LongStream, Stream, StreamSupport}

/**
 * `Chunks` and `java.util.stream`, in both directions.
 *
 * The two are closer than they look. A `Spliterator` is a pull source
 * that can hand over a PREFIX of itself (`trySplit`) — which is what
 * a chunk already is, so the bridge is chunk-for-chunk rather than
 * element-for-element, and `trySplit` costs nothing because the split
 * point is a boundary the producer already chose.
 *
 * The primitive streams matter more than they look too. `LongStream`
 * is unboxed on the JDK side, and since okay's chunks specialize
 * (`ChunkBuf.tabulate` and friends), a `LongStream` becomes a
 * `Chunks[Long]` backed by `long[]` with no boxing at the seam at
 * all. That is what `Spliterator.OfLong` is for, and it would have
 * been unreachable before the chunks could be unboxed.
 */
object Streams {

  // ------------------------------------------------------- okay -> jdk

  /**
   * A chunked producer as a Spliterator. `trySplit` hands over one
   * chunk, which is a real split (the consumer may take it to another
   * thread) at no cost, because the boundary was already there.
   *
   * SIZED is not claimed: a producer need not know its length, and
   * claiming a size the JDK then trusts is how a parallel stream
   * silently drops elements.
   */
  def spliterator[A](p: Chunks[A]): Spliterator[A] = new Spliterator[A]:
    private var rest: Chunks[A] = p
    private var current: Chunk[A] = Chunks.emptyChunk[A]
    private var at = 0

    private def advance(): Boolean =
      while at >= current.length do
        Chunks.pull(rest) match
          case None => return false
          case Some((c, r)) => current = c; rest = r; at = 0
      true

    def tryAdvance(action: Consumer[? >: A]): Boolean =
      if !advance() then false
      else { action.accept(current(at)); at += 1; true }

    /** one whole chunk, which is a boundary the producer chose */
    def trySplit(): Spliterator[A] =
      if at < current.length then
        val head = current.drop(at)
        at = current.length
        chunkSpliterator(head)
      else
        Chunks.pull(rest) match
          case None => null
          case Some((c, r)) => rest = r; chunkSpliterator(c)

    def estimateSize(): Long = Long.MaxValue
    def characteristics(): Int = Spliterator.IMMUTABLE | Spliterator.ORDERED

  /** one chunk as a Spliterator: an indexed, sized, splittable range,
   * which is what the JDK's parallel machinery wants */
  private def chunkSpliterator[A](c: Chunk[A]): Spliterator[A] =
    java.util.Spliterators.spliterator(
      c.toArray[Any].asInstanceOf[Array[AnyRef]], 0, c.length,
      Spliterator.IMMUTABLE | Spliterator.ORDERED)
      .asInstanceOf[Spliterator[A]]

  /** a chunked producer as a Stream; `parallel` splits per chunk */
  def stream[A](p: Chunks[A], parallel: Boolean = false): Stream[A] =
    StreamSupport.stream(spliterator(p), parallel)

  // ---------------------------------------------- okay -> jdk, unboxed

  /**
   * Chunks of Longs as a `LongStream` — primitive on both sides.
   *
   * The chunks this library builds for a concrete element type are
   * backed by `long[]` (see `ChunkBuf.tabulate`), and the JDK's
   * `Spliterator.OfLong` reads primitives, so a chunk whose backing
   * IS a `long[]` is handed over as that array with no copy and no
   * boxing. A chunk that arrived boxed — from a generic producer that
   * could not specialize — still works, one unbox per element, which
   * is what it would have cost anyway.
   */
  def longStream(p: Chunks[Long], parallel: Boolean = false): LongStream =
    StreamSupport.longStream(longSpliterator(p), parallel)

  def intStream(p: Chunks[Int], parallel: Boolean = false): IntStream =
    StreamSupport.intStream(intSpliterator(p), parallel)

  def doubleStream(p: Chunks[Double], parallel: Boolean = false): DoubleStream =
    StreamSupport.doubleStream(doubleSpliterator(p), parallel)

  /** the backing array when the chunk is already primitive, else a
   * copy — asked once per chunk, not once per element */
  private def longsOf(c: Chunk[Long]): Array[Long] =
    c.asInstanceOf[scala.collection.immutable.ArraySeq[Long]].unsafeArray match
      case a: Array[Long] => a
      case _ => c.toArray[Long]

  private def intsOf(c: Chunk[Int]): Array[Int] =
    c.asInstanceOf[scala.collection.immutable.ArraySeq[Int]].unsafeArray match
      case a: Array[Int] => a
      case _ => c.toArray[Int]

  private def doublesOf(c: Chunk[Double]): Array[Double] =
    c.asInstanceOf[scala.collection.immutable.ArraySeq[Double]].unsafeArray match
      case a: Array[Double] => a
      case _ => c.toArray[Double]

  private def longSpliterator(p: Chunks[Long]): Spliterator.OfLong =
    new Spliterator.OfLong:
      private var rest = p
      private var cur: Array[Long] = Array.empty
      private var at = 0
      private def advance(): Boolean =
        while at >= cur.length do
          Chunks.pull(rest) match
            case None => return false
            case Some((c, r)) => cur = longsOf(c); rest = r; at = 0
        true
      def tryAdvance(a: LongConsumer): Boolean =
        if !advance() then false else { a.accept(cur(at)); at += 1; true }
      def trySplit(): Spliterator.OfLong =
        if !advance() then null
        else
          val head = java.util.Arrays.copyOfRange(cur, at, cur.length)
          at = cur.length
          java.util.Spliterators.spliterator(head, 0, head.length,
            Spliterator.IMMUTABLE | Spliterator.ORDERED)
      def estimateSize(): Long = Long.MaxValue
      def characteristics(): Int = Spliterator.IMMUTABLE | Spliterator.ORDERED

  private def intSpliterator(p: Chunks[Int]): Spliterator.OfInt =
    new Spliterator.OfInt:
      private var rest = p
      private var cur: Array[Int] = Array.empty
      private var at = 0
      private def advance(): Boolean =
        while at >= cur.length do
          Chunks.pull(rest) match
            case None => return false
            case Some((c, r)) => cur = intsOf(c); rest = r; at = 0
        true
      def tryAdvance(a: IntConsumer): Boolean =
        if !advance() then false else { a.accept(cur(at)); at += 1; true }
      def trySplit(): Spliterator.OfInt =
        if !advance() then null
        else
          val head = java.util.Arrays.copyOfRange(cur, at, cur.length)
          at = cur.length
          java.util.Spliterators.spliterator(head, 0, head.length,
            Spliterator.IMMUTABLE | Spliterator.ORDERED)
      def estimateSize(): Long = Long.MaxValue
      def characteristics(): Int = Spliterator.IMMUTABLE | Spliterator.ORDERED

  private def doubleSpliterator(p: Chunks[Double]): Spliterator.OfDouble =
    new Spliterator.OfDouble:
      private var rest = p
      private var cur: Array[Double] = Array.empty
      private var at = 0
      private def advance(): Boolean =
        while at >= cur.length do
          Chunks.pull(rest) match
            case None => return false
            case Some((c, r)) => cur = doublesOf(c); rest = r; at = 0
        true
      def tryAdvance(a: DoubleConsumer): Boolean =
        if !advance() then false else { a.accept(cur(at)); at += 1; true }
      def trySplit(): Spliterator.OfDouble =
        if !advance() then null
        else
          val head = java.util.Arrays.copyOfRange(cur, at, cur.length)
          at = cur.length
          java.util.Spliterators.spliterator(head, 0, head.length,
            Spliterator.IMMUTABLE | Spliterator.ORDERED)
      def estimateSize(): Long = Long.MaxValue
      def characteristics(): Int = Spliterator.IMMUTABLE | Spliterator.ORDERED

  // ------------------------------------------------------- jdk -> okay

  /**
   * A Stream as chunks, `size` elements at a time. The stream is
   * consumed once — as its own contract already says — and the chunks
   * arrive lazily, one pull at a time.
   */
  def chunks[A](s: Stream[A], size: Int = 64): Chunks[A] =
    fromSpliterator(s.spliterator(), size)

  def fromSpliterator[A](sp: Spliterator[A], size: Int = 64): Chunks[A] =
    def go(): Chunks[A] = Chunks.defer:
      val buf = ChunkBuf[A](size)
      var n = 0
      val sink: Consumer[A] = a => { buf(n) = a; n += 1 }
      while n < size && sp.tryAdvance(sink) do ()
      if n == 0 then Chunks.end
      else okay.produce(buf.take(n)).flatMap(_ => go())
    go()

  /**
   * A LongStream as UNBOXED chunks. The JDK side is primitive and so
   * is ours, so nothing boxes crossing the seam — the same for
   * `ints` and `doubles` below.
   */
  def longs(s: LongStream, size: Int = 64): Chunks[Long] =
    val sp = s.spliterator()
    def go(): Chunks[Long] = Chunks.defer:
      val arr = new Array[Long](size)
      var n = 0
      val sink: LongConsumer = a => { arr(n) = a; n += 1 }
      while n < size && sp.tryAdvance(sink) do ()
      if n == 0 then Chunks.end
      else okay.produce(wrapLong(arr, n)).flatMap(_ => go())
    go()

  def ints(s: IntStream, size: Int = 64): Chunks[Int] =
    val sp = s.spliterator()
    def go(): Chunks[Int] = Chunks.defer:
      val arr = new Array[Int](size)
      var n = 0
      val sink: IntConsumer = a => { arr(n) = a; n += 1 }
      while n < size && sp.tryAdvance(sink) do ()
      if n == 0 then Chunks.end
      else okay.produce(wrapInt(arr, n)).flatMap(_ => go())
    go()

  def doubles(s: DoubleStream, size: Int = 64): Chunks[Double] =
    val sp = s.spliterator()
    def go(): Chunks[Double] = Chunks.defer:
      val arr = new Array[Double](size)
      var n = 0
      val sink: DoubleConsumer = a => { arr(n) = a; n += 1 }
      while n < size && sp.tryAdvance(sink) do ()
      if n == 0 then Chunks.end
      else okay.produce(wrapDouble(arr, n)).flatMap(_ => go())
    go()

  private def wrapLong(a: Array[Long], n: Int): Chunk[Long] =
    scala.collection.immutable.ArraySeq.unsafeWrapArray(
      if n == a.length then a else java.util.Arrays.copyOf(a, n))

  private def wrapInt(a: Array[Int], n: Int): Chunk[Int] =
    scala.collection.immutable.ArraySeq.unsafeWrapArray(
      if n == a.length then a else java.util.Arrays.copyOf(a, n))

  private def wrapDouble(a: Array[Double], n: Int): Chunk[Double] =
    scala.collection.immutable.ArraySeq.unsafeWrapArray(
      if n == a.length then a else java.util.Arrays.copyOf(a, n))
}
