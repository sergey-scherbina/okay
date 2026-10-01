package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.Direct.*
import scala.language.implicitConversions

/**
 * `Delim.collect` with `Delim.emit` in a direct block, N elements: the
 * library's own generator door (delim-internal-shift0 measured `emit`'s
 * capture as `shift0` here — DelimBenchmark.delimGenerator spells its
 * emit with a raw `Delim.shift` and does not reach this door).
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class CollectBenchmark {

  final val N = 1000

  type R = Delim + Pure

  def count(i: Int)(using Delim.Emitting[Int]): Unit ! R = direct:
    if i < N then
      !Delim.emit(i)
      !count(i + 1)

  @Benchmark
  def collectEmit(): Int = !.run(Delim.collect[Int, Pure](count(0))).length
}
