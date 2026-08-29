package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

import !.*

/**
 * Stream pipelines across the ecosystem: map, filter, take(N), sum —
 * the everyday shape. Iterator is the floor reference. okay lanes:
 * the Producer through the uncons combinators (landing in LazyList),
 * and the pure LazyList generator. The kyo source is bounded (its
 * emit loop needs a bound, as in GeneratorBenchmark); the bound is
 * sized so take(N) is what ends every lane.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class StreamOpsBenchmark {

  final val N = 1000

  @Benchmark
  def stdIterator(): Int =
    Iterator.from(0).map(_ * 2).filter(_ % 3 == 0).take(N).sum

  @Benchmark
  def okayLazyList(): Int =
    nats[Int, LazyList].map(_ * 2).filter(_ % 3 == 0).take(N).sum

  @Benchmark
  def okayProducer(): Int =
    Stream.map(nats[Int, Producer])(_ * 2).filter(_ % 3 == 0).take(N).sum

  @Benchmark
  def fs2Stream(): Int =
    fs2.Stream.iterate(0)(_ + 1).map(_ * 2).filter(_ % 3 == 0).take(N)
      .compile.fold(0)(_ + _)

  @Benchmark
  def zioStream(): Int =
    import _root_.zio.*
    val s = _root_.zio.stream.ZStream.iterate(0)(_ + 1)
      .map(_ * 2).filter(_ % 3 == 0).take(N).runFold(0)(_ + _)
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(s).getOrThrowFiberFailure())

  @Benchmark
  def kyoStream(): Int =
    import _root_.kyo.*
    Stream:
      Loop(0): i =>
        if i > 3 * N + 3 then Loop.done
        else Emit.value(Chunk(i)).andThen(Loop.continue(i + 1))
    .map((x: Int) => x * 2).filter((x: Int) => x % 3 == 0).take(N)
      .runFold(0)((a: Int, v: Int) => a + v).eval
}
