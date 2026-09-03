package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * idiomatic-api-compare: not a forced mode on either side, but each
 * library's own most efficient, idiomatic way to do four things —
 * verified from zio-streams' actual sources (2.1.14), not memory:
 *
 *   - send a COLLECTION: `ZStream.fromIterable` (chunks at its
 *     `DefaultChunkSize`, 4096) against `Source.of` / `Chunks.range`.
 *   - send by an EFFECTFUL STEP, no collection: `ZStream.unfold(s)(f)`
 *     — its own source shows this is `Chunk.single(a)` per step, ZIO's
 *     real per-element path, not an imposed one — against
 *     `Source.range` (a generator, no `LazyList` cell per element).
 *   - read the WHOLE stream: `runFold`/`runCollect` against
 *     `.toLazyList.foldLeft`.
 *   - read ONE AT A TIME, under an effectful callback: `runForeach`
 *     (verified: `ZSink.foreach` over the same chunk machinery, not a
 *     separate fast path) against `Source` through `Drain` where a
 *     channel exists to batch behind, and `toLazyList` (an ordinary
 *     `uncons` walk) where it does not.
 *
 * One number per cell, N=4000 (single stream, not a merge — this
 * isolates production/consumption from the channel-drain and chunked-
 * merge lanes already measured elsewhere).
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class IdiomaticApiBenchmark {

  final val N = 4000

  private def runZio[A](z: _root_.zio.ZIO[Any, Any, A]): A =
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  // ── send a COLLECTION, read it whole ──────────────────────────────

  private val list: List[Long] = (0L until N.toLong).toList

  @Benchmark
  def okayCollectionWhole(): Long =
    Source.of(list).toLazyList.foldLeft(0L)(_ + _)

  /** the API question: does staying IN the program (runCollect, no
   * per-element parking) actually cost less than toLazyList's
   * per-pull CanBlock? */
  @Benchmark
  def okayCollectionRunCollect(): Long =
    Source.of(list).runCollect.map(_.sum).runWith

  @Benchmark
  def zioCollectionWhole(): Long =
    runZio(_root_.zio.stream.ZStream.fromIterable(list).runSum)

  // ── send by an EFFECTFUL STEP (no collection), read it whole ──────

  @Benchmark
  def okayStepWhole(): Long =
    Source.range(0, N.toLong).toLazyList.foldLeft(0L)(_ + _)

  /** the general form, at the row zioStepWhole compares against */
  @Benchmark
  def okayStepWholeUnfold(): Long =
    Source.unfold(0L)(i => if i < N then Some((i, i + 1)) else None).toLazyList.foldLeft(0L)(_ + _)

  @Benchmark
  def zioStepWhole(): Long =
    runZio(_root_.zio.stream.ZStream.unfold(0L)(i => if i < N then Some((i, i + 1)) else None).runSum)

  // ── read ONE AT A TIME under a callback, from a collection source ──

  @Benchmark
  def okayCollectionForeach(): Long =
    var sum = 0L
    Source.of(list).toLazyList.foreach(sum += _)
    sum

  @Benchmark
  def okayCollectionRunForeach(): Long =
    var sum = 0L
    Source.of(list).runForeach(x =>
      okay.effect[Async, Unit](Async.Run(() => sum += x))).runWith
    sum

  @Benchmark
  def zioCollectionForeach(): Long =
    runZio(_root_.zio.Ref.make(0L).flatMap(r =>
      _root_.zio.stream.ZStream.fromIterable(list).runForeach(x => r.update(_ + x)) *> r.get))

  // ── read ONE AT A TIME, from a channel (Drain applies) ─────────────

  @Benchmark
  def okayChannelForeach(): Long =
    var sum = 0L
    Channel.buffer(1024)(list).drained.toLazyList.foreach(sum += _)
    sum

  @Benchmark
  def zioChannelForeach(): Long =
    import _root_.zio.*
    runZio(for
      q <- Queue.bounded[Long](1024)
      _ <- ZIO.foreachDiscard(list)(q.offer).fork
      r <- Ref.make(0L)
      _ <- zio.stream.ZStream.fromQueue(q).take(N.toLong).runForeach(x => r.update(_ + x))
      s <- r.get
    yield s)
}
