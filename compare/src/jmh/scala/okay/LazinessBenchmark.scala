package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * The price of eager pure-bind evaluation: building the 10k-bind chain
 * WITHOUT running it. Under the lazy contract (okay, cats-effect IO)
 * construction is cheap node building and the work stays in the run;
 * under eager construction (kyo) the build already performs the
 * computation — speculative construction, program-as-configuration and
 * build-many-run-few patterns pay full price. Compare with the full
 * build+run lanes in CompareBenchmark.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class LazinessBenchmark {

  final val N = 10000

  @Benchmark
  def okayBuild(): Any =
    (1 to N).foldLeft(pure[Produce, Int](0))((m, _) => m.flatMap(x => produce(x + 1)))

  @Benchmark
  def catsIOBuild(): Any =
    (1 to N).foldLeft(cats.effect.IO.pure(0))((m, _) => m.flatMap(x => cats.effect.IO(x + 1)))

  @Benchmark
  def kyoBuild(): Any =
    import _root_.kyo.*
    (1 to N).foldLeft(0: Int < Any)((m, _) => m.flatMap((x: Int) => x + 1))
}
