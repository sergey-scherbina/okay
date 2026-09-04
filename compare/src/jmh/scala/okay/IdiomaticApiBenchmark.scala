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

  // EVERY LANE NAMES WHAT IT IS. The suffix says the granularity --
  // `elem` walks one element at a time, `chunk` walks arrays -- and
  // whether the consumer MEMOISES, because `toLazyList` allocates a
  // cell per element and buys re-observability that `runForeach` and
  // `runSum` do not pay for.
  //
  // It is a naming rule rather than a comment because the same
  // mistake has now appeared three times in this repository: §6b
  // priced our per-element merge against ZStream's chunk-of-4096 and
  // called both "elementwise"; the guarantee table put our
  // elementwise lanes beside zio lanes running through ZStream; and
  // the two rows below compared `Source.of(list).toLazyList` against
  // `ZStream.fromIterable(list).runSum` -- mismatched on granularity
  // AND memoisation at once. With the properties in the names, a
  // mismatched pair is visible without reading the bodies.

  // ── send a COLLECTION, read it whole ──────────────────────────────

  private val list: List[Long] = (0L until N.toLong).toList

  @Benchmark
  def okayCollection_elem_lazyList(): Long =
    Source.of(list).toLazyList.foldLeft(0L)(_ + _)

  /** the API question: does staying IN the program (runCollect, no
   * per-element parking) actually cost less than toLazyList's
   * per-pull CanBlock? */
  @Benchmark
  def okayCollection_elem_runCollect(): Long =
    Source.of(list).runCollect.map(_.sum).runWith

  /**
   * The LIKE-FOR-LIKE partner for `zioCollection_chunk_runSum`, and
   * the one this table was missing. `ZStream.fromIterable` makes one
   * chunk of the whole collection and `runSum` walks the array; the
   * fair question on our side is `Chunks`, not `Source` -- the tools
   * were already here and simply were not used.
   */
  @Benchmark
  def okayCollection_chunk_fold(): Long =
    Chunks.foldLeft(Chunks.fromIterator(list.iterator, size = N))(0L)(_ + _)

  @Benchmark
  def zioCollection_chunk_runSum(): Long =
    runZio(_root_.zio.stream.ZStream.fromIterable(list).runSum)

  // ── send by an EFFECTFUL STEP (no collection), read it whole ──────

  @Benchmark
  def okayStep_elem_lazyList(): Long =
    Source.range(0, N.toLong).toLazyList.foldLeft(0L)(_ + _)

  /** the general form, at the row zioStepWhole compares against */
  @Benchmark
  def okayStep_elem_unfold_lazyList(): Long =
    Source.unfold(0L)(i => if i < N then Some((i, i + 1)) else None).toLazyList.foldLeft(0L)(_ + _)

  @Benchmark
  def zioStep_elem_runSum(): Long =
    runZio(_root_.zio.stream.ZStream.unfold(0L)(i => if i < N then Some((i, i + 1)) else None).runSum)

  // ── read ONE AT A TIME under a callback, from a collection source ──

  @Benchmark
  def okayCollectionForeach_elem_lazyList(): Long =
    var sum = 0L
    Source.of(list).toLazyList.foreach(sum += _)
    sum

  @Benchmark
  def okayCollectionForeach_elem_runForeach(): Long =
    var sum = 0L
    Source.of(list).runForeach(x =>
      okay.effect[Async, Unit](Async.Run(() => sum += x))).runWith
    sum

  @Benchmark
  def zioCollectionForeach_chunk_runForeach(): Long =
    runZio(_root_.zio.Ref.make(0L).flatMap(r =>
      _root_.zio.stream.ZStream.fromIterable(list).runForeach(x => r.update(_ + x)) *> r.get))

  // ── read ONE AT A TIME, from a channel (Drain applies) ─────────────

  @Benchmark
  def okayChannelForeach_elem_lazyList(): Long =
    var sum = 0L
    Channel.buffer(1024)(list).drained.toLazyList.foreach(sum += _)
    sum

  /**
   * The like-for-like partner: no `toLazyList`, so no cell per
   * element and no memoisation -- exactly what `runForeach` was added
   * for, and what the zio lane below does.
   */
  @Benchmark
  def okayChannelForeach_elem_runForeach(): Long =
    var sum = 0L
    Channel.buffer(1024)(list).drained.runForeach(x =>
      okay.effect[Async, Unit](Async.Run(() => sum += x))).runWith
    sum

  /**
   * A LANE THAT DOES NOT MEASURE WHAT IT LOOKS LIKE, kept with the
   * explanation rather than deleted.
   *
   * It reads 318.7 -- SLOWER than the elementwise lane above -- and
   * the reason is that `.drained` already batches internally through
   * `receiveMany`. Putting `.chunked()` on top of it adds a layer
   * instead of removing one: a fresh chunk accumulated per group, on
   * a source that had already handed over arrays. Chunking is not a
   * spell; it pays only where it replaces a per-element coordination
   * step, and here there was none left to replace.
   */
  @Benchmark
  def okayChannelForeach_chunk_fold(): Long =
    var sum = 0L
    Channel.buffer(1024)(list).drained.chunked().runForeach(ch =>
      okay.effect[Async, Unit](Async.Run(() =>
        var i = 0
        while i < ch.length do { sum += ch(i); i += 1 }))).runWith
    sum

  @Benchmark
  def zioChannelForeach_chunk_runForeach(): Long =
    import _root_.zio.*
    runZio(for
      q <- Queue.bounded[Long](1024)
      _ <- ZIO.foreachDiscard(list)(q.offer).fork
      r <- Ref.make(0L)
      _ <- zio.stream.ZStream.fromQueue(q).take(N.toLong).runForeach(x => r.update(_ + x))
      s <- r.get
    yield s)
}
