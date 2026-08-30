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

  private inline def wrap[A](arr: Array[AnyRef]): Chunk[A] =
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

  import scala.math.Numeric.Implicits.given

  /** the naturals: 0, 1, 2, ... in chunks */
  def nats[N: Numeric as N](size: Int = 64): Chunks[N] =
    generate(N.zero)(identity)(_ + N.one)(size)

  /** the Fibonacci numbers, in chunks */
  def fibs[N: Numeric as N](size: Int = 64): Chunks[N] =
    generate((N.zero, N.one))(_._1)((x, y) => (y, x + y))(size)

  /** merge two chunked streams by readiness: the existing Channel.merge,
   * one queue operation per chunk (type args spelled out — inference
   * abstracts the wrong slot through the nested alias) */
  def merge[A](s: Chunks[A], t: Chunks[A], capacity: Int = Int.MaxValue)
              (using Scheduler): Channel[Chunk[A]] =
    Channel.merge[Chunk[A], Producer, Zero, Producer, Zero](s, t, capacity)

  extension [A](p: Chunks[A])
    /** the element view: one tree step per chunk, an index per element */
    def elements: Iterator[A] =
      summon[Stream[Producer, Zero]].iterator(p).flatMap(_.iterator)

    /** the chunks, memoized (first-order: see merge) */
    def toLazyList: LazyList[Chunk[A]] =
      LazyList.from(summon[Stream[Producer, Zero]].iterator(p))
}
