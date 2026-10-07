package okay


import okay.freer.*
import okay.freer.given
import org.openjdk.jmh.annotations.{State, *}
import java.util.concurrent.TimeUnit
import scala.concurrent.Await as ScalaAwait
import scala.concurrent.duration.Duration

/**
 * The two Async terminals on the same 10k-operation chain: runWith
 * executes each op in place on the current (virtual) thread; runAsync
 * drives the tree through callbacks — the event-loop runner that JS
 * uses, measured here on the JVM to price the universality. The
 * `cont` lanes are the same chain on the machine (cont-first-module):
 * answered in place (`AsyncCont.run`, no capture) and driven through
 * callbacks (`AsyncCont.runAsync`, every operation stops the machine).
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

  def chainCont(n: Int): okay.![Int, okay.+:[Async, okay.Pure]] =
    if n == 0 then okay.pure(0)
    else AsyncCont.async(1).flatMap(x => chainCont(n - 1).map(_ + x))

  @Benchmark
  def contRun10k(): Int = AsyncCont.run(chainCont(10000))

  @Benchmark
  def contRunAsync10k(): Int =
    ScalaAwait.result(AsyncCont.runAsync(chainCont(10000)), Duration.Inf)
}
