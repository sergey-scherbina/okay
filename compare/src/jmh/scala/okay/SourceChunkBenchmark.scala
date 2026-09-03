package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * source-merge-chunked: the CEILING of chunking Source.merge
 * internally.
 *
 * `Chunks.merge` is 10.7us where `Source.merge` is 299.7us on the
 * same 2x500, because it does one queue operation per CHUNK rather
 * than per element. The obvious question is why the per-element
 * merge does not chunk underneath and re-tell elementwise, keeping
 * its API. The obstacle is semantic — a merge is defined by
 * READINESS, and a fixed batch makes an element that is ready now
 * wait for the batch to fill — so before designing an adaptive form
 * that only batches when the consumer is already behind, measure
 * what the best case is even worth.
 *
 * `chunkedCeiling` is that best case, built from existing pieces
 * only: chunk each source, merge the chunk streams through the very
 * same `Source.merge`, unchunk on the way out. Same elements, same
 * channel, one queue operation per `k` of them.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 8, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
class SourceChunkBenchmark {

  @Param(Array("500", "2000"))
  var n: Int = 500

  @Param(Array("16", "64"))
  var k: Int = 64

  private def left: Source[Long] = Source.of(LazyList.range(0L, n.toLong))
  private def right: Source[Long] = Source.of(LazyList.range(n.toLong, 2L * n))

  /** today's path, per element through the channel */
  @Benchmark
  def elementwise(): Long =
    (left merge right).toLazyList.foldLeft(0L)(_ + _)

  /** the ceiling: one queue operation per k elements, same API shape
   * in and out (a Source[Long] either way) */
  @Benchmark
  def chunkedCeiling(): Long =
    def chunker: Unit ! (Take % Long + (Writer % Chunk[Long] + Async)) =
      !.widen[Unit, Take % Long + Writer % Chunk[Long], Async](Stage.chunked[Long](k))
    def flattener: Unit ! (Take % Chunk[Long] + (Writer % Long + Async)) =
      !.widen[Unit, Take % Chunk[Long] + Writer % Long, Async](Stage.unchunk[Long])
    val ac: Source[Chunk[Long]] = through(left)(chunker)
    val bc: Source[Chunk[Long]] = through(right)(chunker)
    val flat: Source[Long] = through(ac merge bc)(flattener)
    flat.toLazyList.foldLeft(0L)(_ + _)

  /** the shipped combinator, on the same elements */
  @Benchmark
  def mergeChunkParam(): Long =
    left.merge(right, chunked = true).toLazyList.foldLeft(0L)(_ + _)

  /** the same flag with a larger ELEMENT budget: capacity is the
   * knob that buys the rest of the win back */
  @Benchmark
  def mergeChunkedRoomy(): Long =
    left.merge(right, capacity = 1024, chunked = true).toLazyList.foldLeft(0L)(_ + _)

  /** the flusher fiber's standing cost: a flush window far longer
   * than the run, so it never fires and only its presence is paid */
  @Benchmark
  def mergeChunkedFlusher(): Long =
    left.merge(right, capacity = 1024, chunked = true, flushAfter = Some(30000))
      .toLazyList.foldLeft(0L)(_ + _)
}
