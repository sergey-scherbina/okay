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
    val merged = Chunks.merge(Chunks.range(0, N), Chunks.range(N, 2L * N))
    var sum = 0L
    var c = merged.receive()
    while c.isDefined do
      sum += c.get.sum
      c = merged.receive()
    sum

  @Benchmark
  def fs2Merge(): Long =
    import cats.effect.IO
    import cats.effect.unsafe.implicits.global
    fs2.Stream.range(0L, N.toLong).covary[IO]
      .merge(fs2.Stream.range(N.toLong, 2L * N).covary[IO])
      .compile.fold(0L)(_ + _).unsafeRunSync()

  @Benchmark
  def zioMerge(): Long =
    import _root_.zio.*
    val s = _root_.zio.stream.ZStream.range(0, N).map(_.toLong)
      .merge(_root_.zio.stream.ZStream.range(N, 2 * N).map(_.toLong))
      .runFold(0L)(_ + _)
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(s).getOrThrowFiberFailure())
}
