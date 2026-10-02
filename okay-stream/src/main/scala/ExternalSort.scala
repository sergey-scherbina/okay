package okay

import scala.collection.mutable
import Chunks.elements

/**
 * An element as bytes, for a sort that spills (chunks-external-sort).
 * okay-stream cannot see okay-codec (the dependency runs the other way),
 * so the one thing the sort needs of an element is said here. Any type
 * with a `Schema` gets one from okay-codec, through CBOR:
 * `import okay.codec.RunCodecs.given`.
 */
trait RunCodec[A]:
  def encode(a: A): Array[Byte]
  def decode(b: Array[Byte]): A

object RunCodec:
  def bytes[A](enc: A => Array[Byte], dec: Array[Byte] => A): RunCodec[A] = new:
    def encode(a: A): Array[Byte] = enc(a)
    def decode(b: Array[Byte]): A = dec(b)

  private def long(n: Long): Array[Byte] =
    val b = new Array[Byte](8)
    var i = 0
    while i < 8 do { b(i) = (n >>> (56 - 8 * i)).toByte; i += 1 }
    b
  private def unlong(b: Array[Byte], at: Int = 0): Long =
    var n = 0L
    var i = 0
    while i < 8 do { n = (n << 8) | (b(at + i) & 0xffL); i += 1 }
    n

  given RunCodec[Long] = bytes(long, unlong(_))
  given RunCodec[Int] = bytes(n => long(n.toLong), b => unlong(b).toInt)
  given RunCodec[Double] = bytes(d => long(java.lang.Double.doubleToRawLongBits(d)), b => java.lang.Double.longBitsToDouble(unlong(b)))
  given RunCodec[Boolean] = bytes(b => Array(if b then 1.toByte else 0.toByte), _(0) != 0)
  given RunCodec[String] = bytes(_.getBytes("UTF-8"), new String(_, "UTF-8"))
  given [A, B](using a: RunCodec[A], b: RunCodec[B]): RunCodec[(A, B)] = bytes(
    (x, y) =>
      val (ea, eb) = (a.encode(x), b.encode(y))
      long(ea.length.toLong) ++ ea ++ eb,
    bs =>
      val n = unlong(bs).toInt
      (a.decode(bs.slice(8, 8 + n)), b.decode(bs.slice(8 + n, bs.length))))

/**
 * Where a sort's runs go (chunks-external-sort): a run is appended once,
 * read back once, and deleted. On JVM and Native the default is temporary
 * files (`spillToTempFiles`, scala-jvm-native); `Spill.memory` holds runs
 * in the heap — for a platform with no disk (JS has no default, so a sort
 * there without one does not compile) and for a test that counts runs.
 */
trait Spill:
  def open(): Spill.Run

object Spill:
  trait Run:
    def append(bytes: Array[Byte]): Unit
    /** done writing: the run is readable from here on */
    def seal(): Unit
    def records(): Iterator[Array[Byte]]
    def delete(): Unit

  /** runs in the heap, counted: `opened` runs, `live` not yet deleted */
  final class Memory extends Spill:
    @volatile var opened = 0
    @volatile var live = 0
    def open(): Run =
      synchronized { opened += 1; live += 1 }
      new Run:
        private val buf = mutable.ArrayBuffer.empty[Array[Byte]]
        private var gone = false
        def append(bytes: Array[Byte]): Unit = { buf += bytes; () }
        def seal(): Unit = ()
        def records(): Iterator[Array[Byte]] = buf.iterator
        def delete(): Unit = Memory.this.synchronized { if !gone then { gone = true; live -= 1; buf.clear() } }

  def memory: Memory = new Memory

/**
 * THE EXTERNAL SORT (chunks-external-sort; Knuth vol. 3 §5.4, the
 * run-and-merge sort): the input is read in RUNS of `budget` elements,
 * each sorted in memory (stable) and spilled; the runs are then merged by
 * a heap over one cursor per run, and the output is a `Chunks[A]` read
 * lazily from the heap — memory is one run while reading and one element
 * per run while merging, never the input. A run that is the whole input
 * is not spilled at all. The sort is STABLE: equal keys come out in input
 * order (a run's own sort is stable, and the heap breaks a tie by run).
 *
 * Nothing is read until the first pull, and then the whole input is — a
 * sort cannot emit its first element sooner. The runs are deleted when
 * the output is read to its end; an output abandoned before its end
 * leaves its runs to the `Spill` (temporary files are deleted at JVM
 * exit). No recursion: every walk is a loop.
 */
object ExternalSort:
  def sortBy[A, K](p: Chunks[A], budget: Int)(key: A => K)
                  (using ord: Ordering[K], codec: RunCodec[A], spill: Spill): Chunks[A] =
    require(budget >= 1, "a sort's run holds at least one element")
    Chunks.defer:
      val it = p.elements
      val first = mutable.ArrayBuffer.empty[A]
      while it.hasNext && first.length < budget do first += it.next()
      if !it.hasNext then Chunks.fromIterator(first.sortBy(key).iterator)
      else
        val runs = mutable.ArrayBuffer.empty[Spill.Run]
        def spillRun(xs: mutable.ArrayBuffer[A]): Unit =
          val r = spill.open()
          for x <- xs.sortBy(key) do r.append(codec.encode(x))
          r.seal()
          runs += r
          xs.clear()
        spillRun(first)
        val buf = mutable.ArrayBuffer.empty[A]
        while it.hasNext do
          buf += it.next()
          if buf.length >= budget then spillRun(buf)
        if buf.nonEmpty then spillRun(buf)
        Chunks.fromIterator(merged(runs.toVector, key))

  /** the k-way merge: the smallest head first, a tie to the earlier run */
  private def merged[A, K](runs: Vector[Spill.Run], key: A => K)
                          (using ord: Ordering[K], codec: RunCodec[A]): Iterator[A] =
    final class Head(val a: A, val k: K, val run: Int)
    val cursors = runs.map(_.records())
    val heap = mutable.PriorityQueue.empty[Head](using
      Ordering.fromLessThan[Head]((x, y) => { val c = ord.compare(x.k, y.k); c > 0 || (c == 0 && x.run > y.run) }))
    def pull(i: Int): Unit =
      if cursors(i).hasNext then
        val a = codec.decode(cursors(i).next())
        heap.enqueue(Head(a, key(a), i))
    runs.indices.foreach(pull)
    new Iterator[A]:
      private var cleaned = false
      def hasNext: Boolean =
        val more = heap.nonEmpty
        if !more && !cleaned then { cleaned = true; runs.foreach(_.delete()) }
        more
      def next(): A =
        if !hasNext then throw java.util.NoSuchElementException("the sorted output has ended")
        val h = heap.dequeue()
        pull(h.run)
        h.a
