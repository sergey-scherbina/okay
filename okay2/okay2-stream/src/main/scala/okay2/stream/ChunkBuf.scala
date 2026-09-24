package okay2.stream

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

/**
 * A chunk under construction: the one buffer there is.
 *
 * The Scala 3 core's `ChunkBuf` is an opaque type over an existential
 * array whose backing is chosen by `summonFrom` at each inline
 * expansion site, so a chunk of Longs fills a `long[]` wherever the
 * element type is concrete. Scala 2 has neither opaque types nor
 * `summonFrom`: this is a plain class over an `Array[AnyRef]`, and a
 * primitive-backed chunk comes from the two places that know their
 * type — `Chunks.range` writes a `long[]` directly, and `tagged` takes
 * a `ClassTag` where the caller has one (`Pipeline.Mapped` carries it).
 */
final class ChunkBuf[A] private (private val arr: Array[AnyRef]) {
  def update(i: Int, a: A): Unit = arr(i) = a.asInstanceOf[AnyRef]
  def apply(i: Int): A = arr(i).asInstanceOf[A]
  def length: Int = arr.length
  /** the chunk over the whole array: the buffer's array leaves owning it */
  def chunk: Chunk[A] = ArraySeq.unsafeWrapArray(arr).asInstanceOf[Chunk[A]]
  /** the first n, copied */
  def take(n: Int): Chunk[A] = ArraySeq.unsafeWrapArray(java.util.Arrays.copyOf(arr, n)).asInstanceOf[Chunk[A]]
}

object ChunkBuf {
  def apply[A](size: Int): ChunkBuf[A] = new ChunkBuf[A](new Array[AnyRef](size))

  /** a fresh buffer per call */
  def factory[A](size: Int): () => ChunkBuf[A] = () => apply[A](size)

  /** a chunk mapper over a boxed array */
  def mapper[A, B](f: A => B): Chunk[A] => Chunk[B] = c => {
    val n = c.length
    val buf = apply[B](n)
    var i = 0
    while (i < n) { buf(i) = f(c(i)); i += 1 }
    buf.chunk
  }

  /** a chunk mapper whose output is UNBOXED where B is primitive: the
   * tag says what array to fill */
  def taggedMapper[A, B](f: A => B)(implicit ct: ClassTag[B]): Chunk[A] => Chunk[B] = c => {
    val n = c.length
    val out = ct.newArray(n)
    var i = 0
    while (i < n) { out(i) = f(c(i)); i += 1 }
    ArraySeq.unsafeWrapArray(out)
  }

  /** a chunk filter: the survivors, in order, in a chunk of their own */
  def filterer[A](p: A => Boolean): Chunk[A] => Chunk[A] = c => {
    val buf = apply[A](c.length)
    var i = 0
    var n = 0
    while (i < c.length) { val a = c(i); if (p(a)) { buf(n) = a; n += 1 }; i += 1 }
    if (n == c.length) c else buf.take(n)
  }

  /** `size` elements of f over the unfolding seed, and the seed after them */
  def filler[A, B](f: A => B)(g: A => A)(size: Int): A => (Chunk[B], A) = a => {
    val buf = apply[B](size)
    var s = a
    var i = 0
    while (i < size) { buf(i) = f(s); s = g(s); i += 1 }
    (buf.chunk, s)
  }
}
