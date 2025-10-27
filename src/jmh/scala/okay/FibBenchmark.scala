package okay

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 2, timeUnit = TimeUnit.SECONDS)
@Fork(3)
class FibBenchmark {

  @Benchmark
  def fib10(): List[Int] =
    fib[Int, LazyList].take(10).toList

  @Benchmark
  def fib50(): List[Int] =
    fib[Int, LazyList].take(50).toList

  @Benchmark
  def fib100(): List[BigInt] =
    fib[BigInt, LazyList].take(100).toList

  @Benchmark
  def fib1000(): List[BigInt] =
    fib[BigInt, LazyList].take(1000).toList
}
