package okay.std.bench

import okay.std.*
import okay.std.given

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * The classic lanes step a `fibs` producer `n` operations by its `Answers` (okay-freer's `next`, a `resume` loop
 * over the tree) and `peek` the next answer. NEAREST EQUIVALENT on the new road: the same producer (`fibs` over
 * `Put[Producer]`, a program `A ! Produce`) walked by the std's linear view — the kernel's `Step`, one stepped
 * operation an element (`Stream[Producer, Pure].iterator`) — `n` elements dropped and the next one read. Same
 * elements stepped, same value answered; the stepping is the kernel's, not freer's `resume`.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FibBenchmark {

  private val St = summon[okay.std.Stream[Producer, okay.Pure]]

  @Benchmark
  def fib10(): Any =
    St.iterator(fibs[Int, Producer]).drop(10).next()

  @Benchmark
  def fib50(): Any =
    St.iterator(fibs[Int, Producer]).drop(50).next()

  @Benchmark
  def fib100(): Any =
    St.iterator(fibs[BigInt, Producer]).drop(100).next()

  @Benchmark
  def fib1000(): Any =
    St.iterator(fibs[BigInt, Producer]).drop(1000).next()
}
