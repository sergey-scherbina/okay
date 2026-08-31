package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * What an `Aggregator` costs, and where.
 *
 * `Fold`'s accumulator turned out to be essentially the whole cost of
 * a fold (29.4us against 2.8 for the element read), and specializing
 * it was worth 4.4x. `Aggregator` has the same `add(acc: Acc, in: In)`
 * shape one layer up, and `Aggregator.fold` hands it over as a plain
 * generic `Fold` — so none of that specialization reaches Spark, a
 * java Collector, or the cluster, which are exactly the callers that
 * cannot inline anything.
 *
 * But the suspicion here is a different and larger one. `mean` is
 * `sum zip count`, so its accumulator is a `(N, Long)`; `variance`
 * carries `(Long, Double, Double)`. Those TUPLES are allocated fresh
 * on every element — an object plus its boxed fields per step, against
 * the two boxes a flat accumulator costs. If that is what the numbers
 * say, it is a bigger hole than the one just closed, and it is in the
 * aggregator people actually run over large data.
 *
 * The floors are hand-written loops carrying primitive locals.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class AggregatorBoxBenchmark {

  val n = 10000
  val size = 64

  def longs: Chunks[Long] = Chunks.range(0L, n.toLong, size)
  def doubles: Chunks[Double] = Chunks.map(Chunks.range(0L, n.toLong, size))(_.toDouble)

  // ---- flat accumulators: what the Fold specialization already fixed
  //      one layer down, measured here through an Aggregator

  @Benchmark
  def aggCount: Long =
    Chunks.fold(longs)(using Aggregator.count[Long].fold)

  @Benchmark
  def aggSum: Long =
    Chunks.fold(longs)(using Aggregator.sum[Long].fold)

  // ---- tuple accumulators: the suspicion

  @Benchmark
  def aggMean: Aggregator.Mean =
    Chunks.fold(doubles)(using Aggregator.mean[Double].fold)

  @Benchmark
  def aggVariance: Aggregator.Variance =
    Chunks.fold(doubles)(using Aggregator.variance[Double].fold)

  // ---- the floors

  /** count and sum, by hand */
  @Benchmark
  def loopSum: Long = Chunks.foldLeft(longs)(0L)(_ + _)

  /** mean's two statistics in primitive locals — no tuple, no boxes */
  @Benchmark
  def loopMean: Double =
    var s = 0.0
    var c = 0L
    Chunks.foldLeft(doubles)(())((_, x) => { s += x; c += 1L })
    if c == 0 then Double.NaN else s / c

  /** Welford in three primitive locals: the same arithmetic the
   * aggregator does, with nothing allocated per step */
  @Benchmark
  def loopVariance: Double =
    var cnt = 0L
    var mean = 0.0
    var m2 = 0.0
    Chunks.foldLeft(doubles)(()) { (_, x) =>
      cnt += 1L
      val d = x - mean
      mean += d / cnt
      m2 += d * (x - mean)
    }
    if cnt == 0 then Double.NaN else m2 / cnt
}
