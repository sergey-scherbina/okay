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
    else steps(n - 1)(Control[M].flatMap(m)(x => Control[M].pure(x + 1)))

  @Benchmark
  def cont24(): Int = reset(steps[Cont](24)(Cont.Pure(0)))

  @Benchmark
  def func24(): Int = steps[Func](24)(Control[Func].pure(0))(identity)

  inline def effSteps[M[_[+_], _]](inline n: Int)(m: M[Produce, Int]): M[Produce, Int] =
    inline if n == 0 then m
    else effSteps(n - 1)(Effects[M].flatMap(m)(x => Effects[M].perform[Produce, Int](x + 1)))

  @Benchmark
  def effCont24(): Int = effSteps[Free](24)(produce(0)).runIn[Cont]

  @Benchmark
  def effFunc24(): Int = effSteps[Free](24)(produce(0)).runIn[Func]

  inline def effInlineSteps[C[_, _, _]](inline n: Int)(m: C[Int, Int, Int],
                                                      h: Interpr[Produce, C, Int]): C[Int, Int, Int] =
    inline if n == 0 then m
    else effInlineSteps(n - 1)(Control[C].flatMap(m)(x => h(x + 1)), h)

  @Benchmark
  def effInline24(): Int =
    val h = interpr[Func, Produce, Int]
    effInlineSteps[Func](24)(h(0), h)(identity)
}
