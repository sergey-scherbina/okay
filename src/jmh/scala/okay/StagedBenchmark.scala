package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class StagedBenchmark {

  inline def steps[M[_, _, _]](inline n: Int)(m: M[Int, Int, Int]): M[Int, Int, Int] =
    inline if n == 0 then m
    else steps(n - 1)(staged[M].flatMap(m)(x => staged[M].pure(x + 1)))

  @Benchmark
  def cont24(): Int = reset(steps[Cont](24)(Cont.Pure(0)))

  @Benchmark
  def func24(): Int = steps[Func](24)(staged[Func].pure(0))(identity)
}
