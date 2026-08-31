package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.collection.immutable.ArraySeq

/**
 * What `Fold` costs over primitives.
 *
 * `Fold[A, S]` is `init: S` and `add(s: S, a: A): S`, both generic —
 * so over a `Chunks[Long]` summing into a `Long`, every step boxes the
 * accumulator on the way in and out, and the element on the way in.
 * That is the same question the chunks themselves answered (8x for
 * Longs), asked one layer up, and it has to be measured rather than
 * assumed: the chars experiment was 8% where 23% was predicted, and
 * the Longs one was 8x where chars had been 8%.
 *
 * The floor is a hand-written while loop over the same unboxed chunks.
 * Between them sits what an inline, specialized fold could reach.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FoldBoxBenchmark {

  val n = 10000
  val size = 64

  /** unboxed chunks: long[] backings, as Chunks.range now builds */
  def longs: Chunks[Long] = Chunks.range(0L, n.toLong, size)

  /** the same as one materialized chunk list, so the loop lanes below
   * measure folding and not producing */
  val materialized: Vector[Chunk[Long]] =
    def go(p: Chunks[Long], acc: Vector[Chunk[Long]]): Vector[Chunk[Long]] =
      Chunks.pull(p) match
        case None => acc
        case Some((c, r)) => go(r, acc :+ c)
    go(longs, Vector.empty)

  // ---- the shipped path

  @Benchmark
  def foldSum: Long = Chunks.fold(longs)(using Fold.sum[Long])

  @Benchmark
  def foldCount: Long = Chunks.fold(longs)(using Fold.count[Long])

  // ---- the floor: the same chunks, a hand loop, nothing generic

  @Benchmark
  def loopSum: Long =
    var acc = 0L
    var j = 0
    while j < materialized.length do
      val c = materialized(j)
      val arr = c.asInstanceOf[ArraySeq[Long]].unsafeArray.asInstanceOf[Array[Long]]
      var i = 0
      while i < arr.length do
        acc += arr(i)
        i += 1
      j += 1
    acc

  /** the same loop but through the Chunk's own apply, so the only
   * difference from `foldGeneric` below is the Fold indirection */
  @Benchmark
  def loopSumViaChunk: Long =
    var acc = 0L
    var j = 0
    while j < materialized.length do
      val c = materialized(j)
      var i = 0
      while i < c.length do
        acc += c(i)
        i += 1
      j += 1
    acc

  /** the generic step, over the materialized chunks: isolates what the
   * `Fold[A, S]` interface costs from what producing costs */
  @Benchmark
  def foldGeneric: Long =
    val fo = Fold.sum[Long]
    var s = fo.init
    var j = 0
    while j < materialized.length do
      val c = materialized(j)
      var i = 0
      while i < c.length do
        s = fo.add(s, c(i))
        i += 1
      j += 1
    s

  /** an INLINE fold: the step is known where the loop is written, so
   * nothing boxes and nothing dispatches — the shape a specialized
   * Fold would have to reach */
  inline def foldInline[A, S](chunks: Vector[Chunk[A]])(z: S)(inline f: (S, A) => S): S =
    var s = z
    var j = 0
    while j < chunks.length do
      val c = chunks(j)
      var i = 0
      while i < c.length do
        s = f(s, c(i))
        i += 1
      j += 1
    s

  @Benchmark
  def foldInlined: Long = foldInline(materialized)(0L)(_ + _)

  // ---- the shipped specialization

  @Benchmark
  def foldLeftSum: Long = Chunks.foldLeft(longs)(0L)(_ + _)

  @Benchmark
  def foldLeftCount: Long = Chunks.count(longs)

  /** the same, but the step goes through a Numeric — which is what
   * `Fold.sum` uses, so this says whether the typeclass survives being
   * inlined or is itself the cost */
  @Benchmark
  def foldLeftNumeric: Long =
    val N = summon[Numeric[Long]]
    Chunks.foldLeft(longs)(N.zero)(N.plus)

  // ---- which half of the boxing is it?
  //
  // `Fold` boxes in two places: the accumulator (S, in and out of
  // `add`) and the element (A, into `add`). A specialized SUBTRAIT
  // could fix the first for folds that arrive as data, but not the
  // second — `Chunks.fold` is generic in A, so the read boxes anyway.
  // Worth knowing the split before building that.

  /** accumulator generic, element read directly: what a `FoldLong`
   * subtrait could NOT fix */
  @Benchmark
  def boxAccumulatorOnly: Long =
    val fo = Fold.sum[Long]
    var s: Any = fo.init
    var j = 0
    while j < materialized.length do
      val c = materialized(j)
      var i = 0
      while i < c.length do
        s = fo.add(s.asInstanceOf[Long], c(i))
        i += 1
      j += 1
    s.asInstanceOf[Long]

  /** element boxed on read, accumulator a raw long: the other half */
  @Benchmark
  def boxElementOnly: Long =
    var s = 0L
    var j = 0
    while j < materialized.length do
      val c: Chunk[?] = materialized(j)
      var i = 0
      while i < c.length do
        s += c(i).asInstanceOf[Long]
        i += 1
      j += 1
    s
}
