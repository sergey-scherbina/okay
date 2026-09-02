package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.concurrent.Await as ScalaAwait
import scala.concurrent.duration.Duration

/**
 * The channel's two hot paths, priced so that channel-on-STM (stm
 * lane) can be held to "no loss": the buffer path (offer, then a
 * receive that finds the element) and the program path (send and
 * receive as Async programs driven by runAsync — the Await
 * handshake per element).
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ChannelBenchmark {

  var c: Channel[Int] = scala.compiletime.uninitialized

  @Setup(Level.Iteration)
  def up(): Unit = c = Channel[Int]()

  @Benchmark
  def offerReceive1k(): Int =
    var i = 0
    while i < 1000 do { c.offer(i): Unit; i += 1 }
    var s = 0
    i = 0
    while i < 1000 do { s += c.receiveBlocking().get; i += 1 }
    s

  def pairs(n: Int, acc: Int): Int ! Async =
    if n == 0 then pure(acc)
    else c.send(n).flatMap(_ => c.receive).flatMap(v => pairs(n - 1, acc + v.get))

  @Benchmark
  def sendReceiveProgram1k(): Int =
    ScalaAwait.result(Async.runAsync(pairs(1000, 0)), Duration.Inf)
}
