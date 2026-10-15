package okay.std.bench

import okay.!
import okay.std.*
import okay.given

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * The program lane of the classic's StagedBenchmark: `effCont24`, 24 binds each performing a `Produce`, the
 * productions answered by `Answers[Produce]` (classic: `Classic[Free]`'s flatMap/perform, run by `runWith`; here
 * the root's `flatMap`/`produce`, run by `Stream.runWith`, the kernel's `Answering`). `cont24`, `func24` (okay-freer's
 * `Cps`/`Func` carriers alone) and `effInline24` (freer's `Interpr` over `Func`) have no counterpart here.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class StagedBenchmark {

  inline def effSteps(inline n: Int)(m: Int ! Produce): Int ! Produce =
    inline if n == 0 then m
    else effSteps(n - 1)(m.flatMap(x => produce(x + 1)))

  @Benchmark
  def effCont24(): Int = okay.std.Stream.runWith(effSteps(24)(produce(0)))
}
