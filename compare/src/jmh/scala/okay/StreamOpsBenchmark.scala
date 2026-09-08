package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit


/**
 * Stream pipelines across the ecosystem: map, filter, take(N), sum —
 * the everyday shape. Iterator is the floor reference. okay lanes:
 * the Producer through the uncons combinators (landing in LazyList),
 * and the pure LazyList generator. The kyo source is bounded (its
 * emit loop needs a bound, as in GeneratorBenchmark); the bound is
 * sized so take(N) is what ends every lane. kyoStream hand-emits
 * SINGLETON chunks — kyo's worst case; kyoStreamRange is kyo's own
 * chunked source (`Stream.range`, 4096-element chunks), the lane a
 * kyo user would write for this pipeline (kyo-fair-lanes).
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
  def okayIterator(): Int =
    nats[Int, Producer].iterator.map(_ * 2).filter(_ % 3 == 0).take(N).sum

  @Benchmark
  def okayChunks(): Int =
    import Chunks.elements
    Chunks.nats[Int]().elements.map(_ * 2).filter(_ % 3 == 0).take(N).sum

  @Benchmark
  def okayChunksTransform(): Int =
    Chunks.fold(
      Chunks.take(
        Chunks.filter(
          Chunks.map(Chunks.nats[Int]())(_ * 2))(_ % 3 == 0))(N))(using Fold.sum[Int])

  /**
   * THE CHUNK SIZE, MADE COMPARABLE (lane-fairness, 2026-09-08).
   *
   * `okayChunks` and `okayChunksTransform` above use `Chunks.nats`'s
   * DEFAULT size of 64, so they cross 47 chunk boundaries over this
   * pipeline. Every competitor lane gets the whole stream as ONE
   * chunk: `fs2.Stream.emits` by construction, `ZStream.range` and
   * kyo's `Stream.range` because their default chunk (4096) is larger
   * than the input. The asymmetry runs AGAINST us and okay wins the
   * table anyway, which is why it went unnoticed -- but a comparison
   * unfair in our own disfavour is still unfair, and a later change to
   * the default would move the row for a reason nobody could see.
   *
   * This lane is the transformer pipeline at one chunk, so the default
   * can be priced rather than guessed at. Both stay: 64 is what a
   * caller gets without asking, one chunk is what the competitors are
   * measured with.
   */
  @Benchmark
  def okayChunksTransformWide(): Int =
    Chunks.fold(
      Chunks.take(
        Chunks.filter(
          Chunks.map(Chunks.nats[Int](3 * N + 4))(_ * 2))(_ % 3 == 0))(N))(using Fold.sum[Int])

  /** the whole-stage form: inline combinators beta-reduce the
   * pipeline into one while-loop (specs/staged-pipelines.md); §5's
   * `Staged` column, until chunked-source-sweep measured in another
   * session from the rest of this file */
  @Benchmark
  def okayStaged(): Long =
    Staged.fold(
      Staged.take(
        Staged.filter(
          Staged.map(Staged.range(0L, 3L * N + 4), (x: Long) => x * 2),
          (x: Long) => x % 3 == 0),
        N))(0L)((s: Long, x: Long) => s + x)

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

  @Benchmark
  def kyoStreamRange(): Int =
    import _root_.kyo.*
    Stream.range(0, 3 * N + 4)
      .map((x: Int) => x * 2).filter((x: Int) => x % 3 == 0).take(N)
      .runFold(0)((a: Int, v: Int) => a + v).eval

  // ── chunked-source-sweep: the other two libraries' CHUNKED sources,
  //    in the same file so §5 can be one session. `ZStream.range` is
  //    ZIO's chunked source (4096 a chunk); `fs2.Stream.emits` is one
  //    chunk and, with no effect, compiles PURE -- no runtime, no
  //    unsafeRunSync (benchmark-fairness-audit found both 17x/60x
  //    under the per-element lanes above, in its own session) ──────

  @Benchmark
  def zioStreamRange(): Int =
    import _root_.zio.*
    val s = _root_.zio.stream.ZStream.range(0, 3 * N + 4)
      .map(_ * 2).filter(_ % 3 == 0).take(N).runFold(0)(_ + _)
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(s).getOrThrowFiberFailure())

  @Benchmark
  def fs2StreamEmits(): Int =
    fs2.Stream.emits(0 until 3 * N + 4).map(_ * 2).filter(_ % 3 == 0).take(N)
      .compile.fold(0)(_ + _)
}
