package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.collection.immutable.ArraySeq

/**
 * Can the casts in `Chunks` be removed, and what does removing them
 * cost?
 *
 * They exist because the generic producers have no `ClassTag[A]`, so
 * they cannot allocate `Array[A]`; they allocate `Array[AnyRef]`, fill
 * it, and `wrap` asserts the element type once.
 *
 * `ArraySeq.untagged.newBuilder[A]` needs no ClassTag either and
 * answers a TYPED `ArraySeq[A]` (backed by `Object[]`, verified), so
 * the cast can go. What it replaces is `arr(i) = x` with a builder's
 * `addOne`, on the hottest allocation path in the streaming layer —
 * which is a question for a measurement, not for an opinion.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ChunkBuildBenchmark {

  val size = 64
  val src: Array[String] = Array.tabulate(size)(i => s"element $i")

  /** what ships: an untyped array, filled by index, asserted once */
  @Benchmark
  def arrayThenCast: Chunk[String] =
    val arr = new Array[AnyRef](size)
    var i = 0
    while i < size do
      arr(i) = src(i).asInstanceOf[AnyRef]
      i += 1
    ArraySeq.unsafeWrapArray(arr).asInstanceOf[Chunk[String]]

  /** the cast-free alternative: a typed builder, no ClassTag */
  @Benchmark
  def untaggedBuilder: Chunk[String] =
    val b = ArraySeq.untagged.newBuilder[String]
    b.sizeHint(size)
    var i = 0
    while i < size do
      b += src(i)
      i += 1
    b.result()

  /** the floor: a ClassTag'd array, which the producers cannot have */
  @Benchmark
  def typedArray: Chunk[String] =
    val arr = new Array[String](size)
    var i = 0
    while i < size do
      arr(i) = src(i)
      i += 1
    ArraySeq.unsafeWrapArray(arr)

  // the same three at the size a partially-filled chunk takes, since
  // `fromIterator` copies when the source runs out mid-chunk
  @Benchmark
  def arrayThenCastPartial: Chunk[String] =
    val arr = new Array[AnyRef](size)
    var i = 0
    while i < size / 2 do
      arr(i) = src(i).asInstanceOf[AnyRef]
      i += 1
    ArraySeq.unsafeWrapArray(java.util.Arrays.copyOf(arr, i))
      .asInstanceOf[Chunk[String]]

  @Benchmark
  def untaggedBuilderPartial: Chunk[String] =
    val b = ArraySeq.untagged.newBuilder[String]
    b.sizeHint(size)
    var i = 0
    while i < size / 2 do
      b += src(i)
      i += 1
    b.result()
}
