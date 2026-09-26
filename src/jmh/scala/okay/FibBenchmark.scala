package okay

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import !.*

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FibBenchmark {

  /** the cont-stack road this fork runs, printed once (ContStackRoad) */
  @Setup(Level.Trial)
  def road(): Unit = ContStackRoad.announce()

  @Benchmark
  def fib10(): Any =
    fibs[Int, Producer].next(10).peek

  @Benchmark
  def fib50(): Any =
    fibs[Int, Producer].next(50).peek

  @Benchmark
  def fib100(): Any =
    fibs[BigInt, Producer].next(100).peek

  @Benchmark
  def fib1000(): Any =
    fibs[BigInt, Producer].next(1000).peek
}
