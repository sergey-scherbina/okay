package okay


import okay.freer.*


import okay.freer.given
import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.Direct.*
import scala.language.implicitConversions

/**
 * `Shift.collect` with `Shift.emit` in a direct block, N elements: the
 * library's own generator door (delim-internal-shift0 measured `emit`'s
 * capture as `shift0` here — DelimBenchmark.delimGenerator spells its
 * emit with a raw `Shift.shift` and does not reach this door).
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class CollectBenchmark {

  final val N = 1000

  type R = Shift % ? + Pure

  def count(i: Int)(using Shift.Emitting[Int]): Unit ! R = direct:
    if i < N then
      !Shift.emit(i)
      !count(i + 1)

  @Benchmark
  def collectEmit(): Int = !.run(Shift.collect[Int, Pure](count(0))).length
}
