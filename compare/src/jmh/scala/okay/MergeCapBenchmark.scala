package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * `Source.merge` across the CAPACITY it is given, which is the one
 * variable never swept (growing-small-capacity, 2026-09-08).
 *
 * The default buffer behind `Channel.apply` is being changed from a
 * plain ring to `growing`, and every measurement supports it except
 * one: `Source.merge` at its default `capacity = 64` reads 3.5x
 * slower under growing, while the same buffer at capacity 1024 with
 * many producers is the fastest lane in the suite. Three explanations
 * have been measured and refuted -- the part scan, the part sizing,
 * and a per-element consumer, which `Source.merge` is not.
 *
 * So: sweep the capacity. This lives in its own class rather than as
 * a @Param on MergeBenchmark because a param there would multiply
 * every lane in that file by three for one question.
 *
 * `Source.merge` runs exactly TWO producers, one fiber per side,
 * which is also the count at which `Growing` first grows.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class MergeCapBenchmark {

  final val N = 500

  @Param(Array("64", "256", "1024"))
  var cap: Int = 64

  @Benchmark
  def sourceMergeAtCapacity(): Long =
    Source.of(LazyList.range(0L, N.toLong))
      .merge(Source.of(LazyList.range(N.toLong, 2L * N)), capacity = cap)
      .toLazyList.foldLeft(0L)(_ + _)
}
