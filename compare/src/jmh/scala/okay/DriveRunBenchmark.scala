package okay


import okay.freer.*


import okay.std.*
import okay.freer.given
import okay.std.given
import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * The callback drive's per-operation cost (ready-merge-cancel-under-
 * consumer-ops, 2026-09-27): 10 000 `Async.Run` in one chain, run by
 * `Async.runAsync` — the `Drive` loop `own`, `adaptive` and JS run every
 * fiber on — and nothing else. The drive's `Run` arm learned to
 * recognise a cancel scope's markers by class; this lane is where two
 * class tests per operation would show, and nowhere else would.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class DriveRunBenchmark {

  final val N = 10000

  private def chain(i: Int): Int ! Async =
    if i == N then pure(i) else async(i + 1).flatMap(chain)

  @Benchmark
  def runChain(): Int = Await.result(Async.runAsync(chain(0)), Duration.Inf)
}
