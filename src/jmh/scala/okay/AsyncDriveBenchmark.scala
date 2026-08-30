package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.concurrent.Await as ScalaAwait
import scala.concurrent.duration.Duration

/**
 * The two Async terminals on the same 10k-operation chain: runWith
 * executes each op in place on the current (virtual) thread; runAsync
 * drives the tree through callbacks — the event-loop runner that JS
 * uses, measured here on the JVM to price the universality.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class AsyncDriveBenchmark {

  def chain(n: Int): Int ! Async =
    if n == 0 then pure(0)
    else async(1).flatMap(x => chain(n - 1).map(_ + x))

  @Benchmark
  def runWith10k(): Int = chain(10000).runWith

  @Benchmark
  def runAsync10k(): Int =
    ScalaAwait.result(Async.runAsync(chain(10000)), Duration.Inf)
}
