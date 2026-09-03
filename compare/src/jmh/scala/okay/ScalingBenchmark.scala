package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * merge-scaling-shape: is `!.resume`'s Bind-rotation cost in the
 * Source path QUADRATIC in the element count, or CONSTANT per
 * element? The whole question of whether rewriting the kernel to a
 * type-aligned continuation queue (reflection without remorse) has a
 * measured justification hangs on this: that rewrite removes
 * QUADRATIC behaviour on left-nested binds, and does nothing for a
 * constant per-element closure allocation.
 *
 * Same bodies as MergeBenchmark's, with N a @Param — read the
 * numbers PER ELEMENT (divide by 2N), not as totals. `rawLazyList`
 * is the control: whatever IT does per element across the sweep is
 * the platform's own scaling, not ours.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ScalingBenchmark {

  @Param(Array("250", "500", "1000", "2000"))
  var n: Int = 500

  /** the isolated interpretation cost: ONE Source, no Channel, no
   * fiber, no Async — this is where a quadratic Bind shape, if there
   * is one, must show up cleanly */
  @Benchmark
  def sourceSingleDrain(): Long =
    Source.of(LazyList.range(0L, 2L * n)).toLazyList.foldLeft(0L)(_ + _)

  /** the control: the same walk with no program layer at all */
  @Benchmark
  def rawLazyListDrain(): Long =
    LazyList.range(0L, 2L * n).foldLeft(0L)(_ + _)

  /** the full shape the question came from */
  @Benchmark
  def sourceMerge(): Long =
    (Source.of(LazyList.range(0L, n.toLong)) merge Source.of(LazyList.range(n.toLong, 2L * n)))
      .toLazyList.foldLeft(0L)(_ + _)

  /** the same merge without the Writer layer: separates the channel's
   * own scaling from the interpretation's */
  @Benchmark
  def channelMerge(): Long =
    Channel.merge(LazyList.range(0L, n.toLong), LazyList.range(n.toLong, 2L * n))
      .toLazyList.foldLeft(0L)(_ + _)
}
