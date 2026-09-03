package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

/**
 * Chunking and flushing across the three libraries that have all
 * three shapes: merge two streams elementwise, merge them in chunks,
 * and merge them in chunks with a TIME bound on a partial one.
 *
 * fs2 spells the last `groupWithin`, ZIO spells it `groupedWithin`,
 * okay spells it `flushAfter` — so this is a like-for-like table
 * rather than one library's feature against another's absence. Every
 * lane consumes the same 2xN elements and folds them to one Long, so
 * the number is the plumbing and not the arithmetic.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ChunkFlushBenchmark {

  final val N = 2000
  final val K = 16

  /** the chunk size is the axis the cross-library comparison turns
   * on: ZIO's streams are chunked by construction (4096 by default),
   * so comparing our 16 against their natural size measures the size,
   * not the library */
  // 4096 is deliberately absent: a stage that accumulates more than
  // ~2000 elements without emitting overflows the stack in `through`
  // (pre-existing, reproduced on b8c65c7 without any of this lane's
  // code — filed as chunk-stack-safety)
  @Param(Array("16", "256", "1024"))
  var k: Int = 16

  private def l: Source[Long] = Source.of(LazyList.range(0L, N.toLong))
  private def r: Source[Long] = Source.of(LazyList.range(N.toLong, 2L * N))

  // ── okay ─────────────────────────────────────────────────────────

  @Benchmark
  def okayElementwise(): Long =
    (l merge r).toLazyList.foldLeft(0L)(_ + _)

  @Benchmark
  def okayChunked(): Long =
    l.merge(r, capacity = 1024, chunked = true).toLazyList.foldLeft(0L)(_ + _)

  /** the same, composed from the orthogonal combinators instead of
   * the fused flag — the API question, measured */
  @Benchmark
  def okayChunkedComposed(): Long =
    l.chunked(k).merge(r.chunked(k), capacity = 64).unchunked.toLazyList.foldLeft(0L)(_ + _)

  @Benchmark
  def okayChunkedFlush(): Long =
    l.merge(r, capacity = 1024, chunked = true, flushAfter = Some(1000))
      .toLazyList.foldLeft(0L)(_ + _)

  // ── fs2 ──────────────────────────────────────────────────────────

  private def fs2Pair =
    import cats.effect.IO
    (fs2.Stream.range(0L, N.toLong).covary[IO], fs2.Stream.range(N.toLong, 2L * N).covary[IO])

  @Benchmark
  def fs2Elementwise(): Long =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    val (a, b) = fs2Pair
    a.merge(b).compile.fold(0L)(_ + _).unsafeRunSync()

  @Benchmark
  def fs2Chunked(): Long =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    val (a, b) = fs2Pair
    a.merge(b).chunkN(k).flatMap(fs2.Stream.chunk).compile.fold(0L)(_ + _).unsafeRunSync()

  @Benchmark
  def fs2GroupWithin(): Long =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    val (a, b) = fs2Pair
    a.merge(b).groupWithin(K, 1.second).flatMap(fs2.Stream.chunk)
      .compile.fold(0L)(_ + _).unsafeRunSync()

  // ── ZIO ──────────────────────────────────────────────────────────

  private def runZio[A](z: _root_.zio.ZIO[Any, Any, A]): A =
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  private def zioPair =
    import _root_.zio.stream.ZStream
    (ZStream.range(0, N).map(_.toLong), ZStream.range(N, 2 * N).map(_.toLong))

  @Benchmark
  def zioElementwise(): Long =
    val (a, b) = zioPair
    runZio(a.merge(b).runFold(0L)(_ + _))

  @Benchmark
  def zioChunked(): Long =
    val (a, b) = zioPair
    runZio(a.merge(b).grouped(k).flattenChunks.runFold(0L)(_ + _))

  @Benchmark
  def zioGroupedWithin(): Long =
    import _root_.zio.*
    val (a, b) = zioPair
    runZio(a.merge(b).groupedWithin(K, Duration.fromMillis(1000)).flattenChunks
      .runFold(0L)(_ + _))

  /**
   * The question the chunk-size curve really asks: ZIO is not merging
   * a per-element stream in chunks, it never HAS a per-element stream
   * — ZStream is chunked by construction. okay's equivalent is
   * `Chunks`, a stream of chunks from the start, which never builds a
   * program node per element at all. This is the like-for-like pair,
   * and the one the curve above was not.
   */
  @Benchmark
  def okayChunksNative(): Long =
    val merged = Chunks.range(0, N.toLong) merge Chunks.range(N.toLong, 2L * N)
    var sum = 0L
    var c = merged.receiveBlocking()
    while c.isDefined do { sum += c.get.sum; c = merged.receiveBlocking() }
    sum
}
