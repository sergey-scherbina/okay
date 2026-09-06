package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * Merging two streams by readiness: 2 x N elements through a merge,
 * summed. okay merges through a Channel fed by two Loom fibers;
 * fs2 and ZIO merge on their concurrent runtimes. This measures the
 * fixed cost of concurrent plumbing, not throughput under load.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class MergeBenchmark {

  final val N = 500

  @Benchmark
  def okayChannelMerge(): Long =
    Channel.merge(LazyList.range(0L, N.toLong), LazyList.range(N.toLong, 2L * N))
      .toLazyList.foldLeft(0L)(_ + _)

  @Benchmark
  def okayChunksMerge(): Long =
    val merged = Chunks.range(0, N) merge Chunks.range(N, 2L * N)
    var sum = 0L
    var c = merged.receiveBlocking()
    while c.isDefined do
      sum += c.get.sum
      c = merged.receiveBlocking()
    sum

  /** the merge in the program shape: what the Writer walk adds over
   * the raw channel merge above */
  @Benchmark
  def okaySourceMerge(): Long =
    (Source.of(LazyList.range(0L, N.toLong)) merge Source.of(LazyList.range(N.toLong, 2L * N)))
      .toLazyList.foldLeft(0L)(_ + _)

  /** DIAGNOSTIC (channel-merge-regression follow-up): ONE Source,
   * no Channel.merge, no fiber, no Async at all — isolates the raw
   * cost of wrapping a LazyList as a Writer program and draining it,
   * against the native LazyList doing the exact same walk */
  @Benchmark
  def okaySourceSingleDrain(): Long =
    Source.of(LazyList.range(0L, 2L * N)).toLazyList.foldLeft(0L)(_ + _)

  @Benchmark
  def rawLazyListDrain(): Long =
    LazyList.range(0L, 2L * N).foldLeft(0L)(_ + _)

  /**
   * DIAGNOSTIC, not fs2's price: `Stream.range` in 3.10.2 is
   * `emit(o) ++ go(o + step)` (Stream.scala:3981), a singleton chunk
   * per element, so this merges 2xN one-element chunks through fs2's
   * concurrency machinery -- its own scaladoc says use `emits` for one
   * chunk. Kept, renamed, because what it measures is real (a
   * genuinely one-at-a-time source) and because it was the only fs2
   * row for a week (fs2-chunked-merge-lanes, 2026-09-06).
   */
  @Benchmark
  def fs2MergeSingletons(): Long =
    import cats.effect.IO
    import cats.effect.unsafe.implicits.global
    fs2.Stream.range(0L, N.toLong).covary[IO]
      .merge(fs2.Stream.range(N.toLong, 2L * N).covary[IO])
      .compile.fold(0L)(_ + _).unsafeRunSync()

  /** fs2 chunk-native: each side one chunk, the way `ZStream.range`
   * hands ZIO an array and `Chunks.range` hands okay one */
  @Benchmark
  def fs2MergeChunkNative(): Long =
    import cats.effect.IO
    import cats.effect.unsafe.implicits.global
    fs2.Stream.emits(0L until N.toLong).covary[IO]
      .merge(fs2.Stream.emits(N.toLong until 2L * N).covary[IO])
      .compile.fold(0L)(_ + _).unsafeRunSync()

  @Benchmark
  def zioMerge(): Long =
    import _root_.zio.*
    val s = _root_.zio.stream.ZStream.range(0, N).map(_.toLong)
      .merge(_root_.zio.stream.ZStream.range(N, 2 * N).map(_.toLong))
      .runFold(0L)(_ + _)
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(s).getOrThrowFiberFailure())
}
