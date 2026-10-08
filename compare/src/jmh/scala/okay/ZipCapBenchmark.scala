package okay




import okay.freer.given
import okay.std.given
import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * `Source.zip` at a small and a default ring (resume-late-small-ring-cost,
 * 2026-09-29): a fiber per side, the pairing on the caller's thread, so
 * every late answer to a blocked side comes from a thread outside the
 * pool — the shape `DriveTask.resumeLate` decides for. The lost-pairs
 * law's own shape: a counter on one side, a LazyList on the other.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ZipCapBenchmark {

  final val N = 2000L

  @Param(Array("7", "64"))
  var cap: Int = 7

  @Benchmark
  def zipAtCapacity(): Int =
    Source.zip(Source.range(0, N), Source.of(LazyList.range(0L, N)), capacity = cap).runCollect.runWith.size
}
