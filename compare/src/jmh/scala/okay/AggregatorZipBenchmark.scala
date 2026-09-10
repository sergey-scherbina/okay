package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * WHAT A COMPOSED STATISTIC ALLOCATES, which is the number that
 * decides this and not the clock.
 *
 * docs/benchmarks.md §20 measured that the arithmetic being
 * COMPOSABLE was two thirds of okay's single-core distance from a
 * hand-written fold, and `Aggregator.summary` answered it for the
 * common triple. This benchmark is the general case beside it, and it
 * is run for `gc.alloc.rate.norm` — BYTES PER OPERATION — first:
 * allocation is exact and independent of what else the machine is
 * doing, where wall-clock on a shared box moves 30% and cannot
 * resolve the effect at all. Time is reported too, and read second.
 *
 *   sbt "compare/Jmh/run -prof gc .*AggregatorZipBenchmark.*"
 *
 * THE THREE LANES are the same two statistics over the same data:
 * `zip` (the general composition, a `Tuple2` and a box per side),
 * `zipLong` (the `OfLong` specialization, one flat object), and
 * `summary` (the flat count/sum/min/max, one object for four
 * statistics) as the floor.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class AggregatorZipBenchmark {

  final val N = 10000

  private var xs: Array[Long] = null

  @Setup(Level.Trial)
  def setup(): Unit =
    val a = new Array[Long](N)
    var i = 0
    var h = 987654321L
    while i < N do
      h = h * 6364136223846793005L + 1442695040888963407L
      a(i) = (h >>> 40) % 600L
      i += 1
    xs = a

  private val tupled: Aggregator[Long, (Long, Long), (Long, Long)] =
    Aggregator.count[Long].zip(Aggregator.sumLong)

  private val flat: Aggregator[Long, Aggregator.Longs2, (Long, Long)] =
    Aggregator.count[Long].zipLong(Aggregator.sumLong)

  private val summary: Aggregator[Long, Aggregator.Summary, Aggregator.Summary] =
    Aggregator.summary[Long](identity)

  // NOT inline: an inline method reading a private field generates an
  // unstable accessor, and the point here is to measure the
  // aggregator's allocation rather than the harness's
  private def fold[Acc, Out](agg: Aggregator[Long, Acc, Out], data: Array[Long]): Out =
    var acc = agg.init
    var i = 0
    while i < data.length do
      acc = agg.add(acc, data(i))
      i += 1
    agg.present(acc)

  /** the general composition: a tuple and a box per side, per element */
  @Benchmark
  def zipTuple(): (Long, Long) = fold(tupled, xs)

  /** the OfLong specialization: one flat object per element */
  @Benchmark
  def zipFlat(): (Long, Long) = fold(flat, xs)

  /** four statistics in one flat object — the floor */
  @Benchmark
  def summaryFlat(): Aggregator.Summary = fold(summary, xs)
}
