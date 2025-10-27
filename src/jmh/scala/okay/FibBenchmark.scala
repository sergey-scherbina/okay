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

  @Benchmark
  def fib10(): Any =
    fib[Int, Producer].next(10).?

  @Benchmark
  def fib50(): Any =
    fib[Int, Producer].next(50).?

  @Benchmark
  def fib100(): Any =
    fib[BigInt, Producer].next(100).?

  @Benchmark
  def fib1000(): Any =
    fib[BigInt, Producer].next(1000).?
}
