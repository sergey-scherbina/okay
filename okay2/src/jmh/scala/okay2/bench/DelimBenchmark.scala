package okay2.bench

import org.openjdk.jmh.annotations.{State => JmhState, _}
import java.util.concurrent.TimeUnit

import okay2._

/**
 * The Scala 3 core's DelimBenchmark lanes on okay2's `Shift` machine (okay2-delim-perf): what a delimiter, a `$`
 * and a capture cost, N = 1000 a run. The lanes the okay2 perf twins (delimiter witness, dollar shots, split in
 * one pass) are priced on, as the core's were.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class DelimBenchmark {

  final val N = 1000

  type Row = Shift[Any] + Pure

  /** a capture per element: `collect`'s `emit`, the generator */
  @Benchmark
  def delimGenerator(): Int =
    !.run(Shift.collect[Int, Pure] { e =>
      def go(i: Int): Unit ! Row =
        if (i >= N) pure[Row, Unit](())
        else Shift.emit(e)(i).flatMap(_ => go(i + 1))
      go(0)
    }).length

  /** a delimiter pushed and popped, nothing captured */
  @Benchmark
  def delimPushOnly(): Int =
    !.run(Shift.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if (i >= N) pure[Row, Int](i)
        else Shift.push[Int, Pure](Shift.prompt[Int])(pure[Row, Int](i)).flatMap(_ => go(i + 1))
      go(0)
    })

  /** a `$` entered and left through its `ret`, nothing captured */
  @Benchmark
  def delimDollarOnly(): Int =
    !.run(Shift.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if (i >= N) pure[Row, Int](i)
        else Shift.dollar[Int, Int, Pure](Shift.prompt[Int])(x => pure[Row, Int](x + 1))(pure[Row, Int](i)).flatMap(_ => go(i + 1))
      go(0)
    })

  /** a `$` whose body captures to it once and resumes */
  @Benchmark
  def delimDollarResume(): Int =
    !.run(Shift.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if (i >= N) pure[Row, Int](i)
        else {
          val p = Shift.prompt[Int]
          Shift.dollar[Int, Int, Pure](p)(x => pure[Row, Int](x + 1))(
            Shift.shift0[Int, Int, Pure](p)(k => k(i))).flatMap(_ => go(i + 1))
        }
      go(0)
    })
}
