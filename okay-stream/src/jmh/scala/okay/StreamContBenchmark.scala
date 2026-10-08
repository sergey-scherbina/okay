package okay

import okay.std.Writer
import org.openjdk.jmh.annotations.{State, *}
import java.util.concurrent.TimeUnit

/**
 * The machine's stream twin beside the classic Source (stream-twin), on the same pipeline: a range of 100 000
 * collected, and the same range mapped first. The classic's is a Writer program walked by `runCollect`; the
 * twin's a pull, one Step per element, over the machine's Async. Both drives finish inline (no Await pends).
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class StreamContBenchmark {

  final val N = 100000L

  @Benchmark
  def classicRange(): Int = Async.runAsync(Source.range(0, N).runCollect).value.get.get.size

  @Benchmark
  def contRange(): Int = AsyncCont.runAsync(StreamCont.range(0, N).toVector).value.get.get.size

  @Benchmark
  def classicRangeMap(): Int = Async.runAsync(Writer.map(Source.range(0, N))(_ * 2).runCollect).value.get.get.size

  @Benchmark
  def contRangeMap(): Int = AsyncCont.runAsync(StreamCont.range(0, N).map(_ * 2).toVector).value.get.get.size
}
