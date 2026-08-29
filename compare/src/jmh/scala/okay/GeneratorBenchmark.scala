package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

import !.*

/**
 * Generators across the ecosystem: the N-th Fibonacci number by
 * unfolding one element at a time. Iterator is the floor reference;
 * note that fs2 and ZStream are chunk-oriented, so per-element unfold
 * is their worst case — real streaming would batch.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class GeneratorBenchmark {

  final val N = 1000

  @Benchmark
  def stdIterator(): Int =
    Iterator.unfold((0, 1))((a, b) => Some((a, (b, a + b)))).drop(N - 1).next()

  @Benchmark
  def stdLazyList(): Int =
    LazyList.unfold((0, 1))((a, b) => Some((a, (b, a + b))))(N - 1)

  @Benchmark
  def okayLazyList(): Int =
    fibs[Int, LazyList](N - 1)

  @Benchmark
  def okayProducer(): Any =
    fibs[Int, Producer].next(N - 1).?

  @Benchmark
  def fs2Stream(): Option[Int] =
    fs2.Stream.unfold((0, 1))((a, b) => Some((a, (b, a + b)))).take(N).compile.last

  @Benchmark
  def kyoStream(): Int =
    import _root_.kyo.*
    Stream:
      Loop((0, 1, N)): (a, b, k) =>
        if k == 0 then Loop.done
        else Emit.value(Chunk(a)).andThen(Loop.continue(b, a + b, k - 1))
    .take(N).runFold(0)((_, v: Int) => v).eval

  @Benchmark
  def zioStream(): Option[Int] =
    import _root_.zio.*
    val s = _root_.zio.stream.ZStream.unfold((0, 1))((a, b) => Some((a, (b, a + b)))).take(N).runLast
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(s).getOrThrowFiberFailure())
}
