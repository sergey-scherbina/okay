package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

import !.*

/**
 * Nondeterminism across the ecosystem: a binary choice nested D deep
 * (2^(D+1) leaves), all branches collected. The plain List monad is
 * the floor reference. okay's handler is multi-shot (the captured
 * continuation runs once per alternative); kyo has Choice; atnos-eff
 * the List effect. cats-effect and ZIO have no nondeterminism effect.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ChoiceBenchmark {

  final val D = 12

  @Benchmark
  def stdList(): Int =
    def go(d: Int): List[Int] = List(0, 1).flatMap: x =>
      if d == 0 then List(x) else go(d - 1).map(x + _)
    go(D).size

  @Benchmark
  def okayChoice(): Int =
    def go(d: Int): Int ! Choose = choose(0, 1).flatMap: x =>
      if d == 0 then pure(x) else go(d - 1).map(x + _)
    !.run(runChoice[Int, Nothing](go(D))).size

  @Benchmark
  def kyoChoice(): Int =
    import _root_.kyo.*
    def go(d: Int): Int < Choice = Choice.get(Seq(0, 1)).flatMap: (x: Int) =>
      if d == 0 then (x: Int < Choice) else go(d - 1).flatMap((y: Int) => (x + y: Int < Choice))
    Choice.run(go(D)).eval.size

  @Benchmark
  def atnosList(): Int =
    import org.atnos.eff.*
    import org.atnos.eff.all.*
    import org.atnos.eff.syntax.all.*
    type S = Fx.fx1[List]
    def go(d: Int): Eff[S, Int] = values[S, Int](0, 1).flatMap: x =>
      if d == 0 then Eff.pure(x) else go(d - 1).map(x + _)
    go(D).runList.run.size
}
