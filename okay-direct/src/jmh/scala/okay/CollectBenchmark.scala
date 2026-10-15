package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.Direct.*
import okay.std.Writer
import scala.language.implicitConversions

/**
 * `Shift.collect` with `Shift.emit` in a direct block, N elements: the
 * library's own generator door (delim-internal-shift0 measured `emit`'s
 * capture as `shift0` here — DelimBenchmark.delimGenerator spells its
 * emit with a raw `Shift.shift` and does not reach this door).
 * ON THE KERNEL (freer-on-core): okay-freer's capture-based `Shift.emit`
 * has no counterpart; the door that collects what a direct block emits
 * is `Writer.collect` over `Writer.tell`, which this lane now measures.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class CollectBenchmark {

  final val N = 1000

  type R = Writer % Int

  def count(i: Int): Unit ! R = direct:
    if i < N then
      Writer.tell(i).?
      count(i + 1).?

  @Benchmark
  def collectEmit(): Int = Writer.collect(count(0)).run._1.length
}
