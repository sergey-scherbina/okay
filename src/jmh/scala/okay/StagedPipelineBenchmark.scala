package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * The whole-stage box of specs/staged-pipelines.md: the standard
 * pipeline lane (range -> map -> filter -> take(1000) -> sum),
 * staged as one inline-fused while-loop, against the bare Iterator
 * floor and the interpreted operator tree.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class StagedPipelineBenchmark {

  @Benchmark
  def stagedFold(): Long =
    Staged.fold(
      Staged.take(
        Staged.filter(
          Staged.map(Staged.range(0, 1000000), _ * 2),
          _ % 3 == 0),
        1000))(0L)(_ + _)

  @Benchmark
  def stdIterator(): Long =
    Iterator.range(0L, 1000000L).map(_ * 2).filter(_ % 3 == 0).take(1000).sum

  @Benchmark
  def interpretedTree(): Long =
    Pipeline.fold(
      Pipeline.range(0, 1000000).map(_ * 2).filter(_ % 3 == 0).take(1000))(
      using Fold.sum[Long])
}
