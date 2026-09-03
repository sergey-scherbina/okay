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

  // ── Channel.buffer: the other per-element channel consumer ───────

  /** a producer run ahead into a bounded channel, read one element
   * per transaction — the shape buffer has always had */
  @Benchmark
  def bufferPerElement(): Long =
    Writer.of(Channel.buffer(1024)(LazyList.range(0L, 2L * N)))
      .toLazyList.foldLeft(0L)(_ + _)

  /** the same, read in batches */
  @Benchmark
  def bufferDrained(): Long =
    Channel.buffer(1024)(LazyList.range(0L, 2L * N)).drained
      .toLazyList.foldLeft(0L)(_ + _)

  // ── is the SOURCE the difference? ────────────────────────────────
  // Profiling okayChunkedComposed put ~130 samples in LazyList itself
  // (state$lzycompute, unfold) against 132 in `resume` — so a large
  // part of what looked like our chunking cost is the benchmark's
  // source, a lazy list allocating a cell per element, against ZIO's
  // ZStream.range which emits arrays. These hold everything else
  // fixed and vary only that.

  private val listL: List[Long] = (0L until N.toLong).toList
  private val listR: List[Long] = (N.toLong until 2L * N).toList

  @Benchmark
  def chunkedFromList(): Long =
    Source.of(listL).chunked(k).merge(Source.of(listR).chunked(k), capacity = 64)
      .unchunked.toLazyList.foldLeft(0L)(_ + _)

  /** the generated range: no collection, no cell per element */
  @Benchmark
  def chunkedFromRange(): Long =
    Source.range(0, N.toLong).chunked(k)
      .merge(Source.range(N.toLong, 2L * N).chunked(k), capacity = 64)
      .unchunked.toLazyList.foldLeft(0L)(_ + _)

  @Benchmark
  def elementwiseFromRange(): Long =
    (Source.range(0, N.toLong) merge Source.range(N.toLong, 2L * N))
      .toLazyList.foldLeft(0L)(_ + _)

  // ── like-for-like, forced ────────────────────────────────────────
  // The table used to read okay's PER-ELEMENT merge against ZIO's
  // chunk-native one, which is not a comparison: ZStream has no
  // per-element representation, its "element" is a slot in an array.
  // Both libraries can be MADE to work per element (a chunk of one),
  // so the honest row asks all three the same question rather than
  // pricing what one of them does not do.

  @Benchmark
  def zioPerElement(): Long =
    import _root_.zio.stream.ZStream
    val a = ZStream.range(0, N, 1).map(_.toLong)
    val b = ZStream.range(N, 2 * N, 1).map(_.toLong)
    runZio(a.merge(b).runFold(0L)(_ + _))

  @Benchmark
  def fs2PerElement(): Long =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    val (a, b) = fs2Pair
    a.unchunk.merge(b.unchunk).compile.fold(0L)(_ + _).unsafeRunSync()

  /** fs2 at its own natural chunking, the chunk-native row */
  @Benchmark
  def fs2ChunkNative(): Long =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    val (a, b) = fs2Pair
    a.merge(b).chunks.map(_.foldLeft(0L)(_ + _)).compile.fold(0L)(_ + _).unsafeRunSync()

  // ── does the array-native representation degrade at size 1 too? ──
  // The operator's question: why not just replace Source with Chunks
  // entirely, the array-native shape that beats ZIO's own chunk-
  // native default 5.7x? Hypothesis: an array-of-chunks
  // representation pays a chunk allocation per PRODUCTION regardless
  // of size, so at size=1 -- what a genuinely one-at-a-time live
  // source (LLM tokens, SSE) forces -- it should degrade the same way
  // ZStream(chunkSize=1) measured 12x worse than Source.merge.

  @Benchmark
  def chunksMergeSize1(): Long =
    val merged = Chunks.range(0, N.toLong, 1) merge Chunks.range(N.toLong, 2L * N, 1)
    var sum = 0L
    var c = merged.receiveBlocking()
    while c.isDefined do { sum += c.get.sum; c = merged.receiveBlocking() }
    sum

  @Benchmark
  def chunksMergeSize16(): Long =
    val merged = Chunks.range(0, N.toLong, 16) merge Chunks.range(N.toLong, 2L * N, 16)
    var sum = 0L
    var c = merged.receiveBlocking()
    while c.isDefined do { sum += c.get.sum; c = merged.receiveBlocking() }
    sum
}
