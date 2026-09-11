package okay.zio.wroclaw

import okay.wroclaw.{Feed, Job, Native}

/**
 * zio-streams ON ITS OWN TERMS (docs/benchmarks.md §20).
 *
 * WHAT THE LIBRARY GIVES ITS USER. ZIO has no event-time window —
 * `groupedWithin` is PROCESSING time, a different operator answering a
 * different question — so the job is the fold a ZIO user writes:
 * `Native.Fold`, panes as keys in a map, nothing evicted, carried by
 * `ZStream`. The earlier version of this lane handed ZIO
 * `okay.Windows` and thereby held constant the one thing a reader is
 * choosing between.
 *
 * THE SOURCE is `ZStream.fromChunk(Chunk.fromArray(...)).rechunk(256)`
 * — the library's own chunked constructor at the granularity every
 * other lane uses. `Chunk.slice` is a view, so a slice costs nothing.
 *
 * EVERY ROW PAYS FOR THE RUNTIME, including the 1-core one: a
 * `ZStream` is a ZIO and there is no pure interpreter for it, which is
 * a real difference from fs2 (whose `Stream[Pure, *]` compiles without
 * one) and is visible in the rows.
 */
object ZioLane {

  import _root_.zio.*
  import _root_.zio.stream.ZStream

  /** stage 1 and the fold over one contiguous slice, as a ZStream */
  private def slice(feed: Feed, tram: Array[Boolean], from: Int, until: Int)
  : ZIO[Any, Nothing, Native.Fold] =
    ZIO.succeed(new Native.Fold(tram)).flatMap { f =>
      ZStream.fromChunk(Chunk.fromArray(feed.events).slice(from, until)).rechunk(256)
        .filter(f.known).map(f.enrich)
        .runFold(())((_, r) => f.add(r))
        .as(f)
    }

  private def unsafeRun[A](z: ZIO[Any, Nothing, A]): A =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  /** one core, one fibre */
  def run(feed: Feed): Job.Result =
    unsafeRun(slice(feed, Native.tramTable(feed), 0, feed.events.length).map(_.result))

  /**
   * P cores in ZIO's own vocabulary: `foreachPar` over the slices with
   * the parallelism fixed by `withParallelism`, which is what makes a
   * "4 cores" row mean four here. `foreachPar` returns the results in
   * the ORDER OF ITS INPUTS, and that order is load-bearing —
   * `Fold.absorb` stitches the bunching pair that straddles a slice
   * boundary, and that stitch is not commutative.
   */
  def parallel(feed: Feed, cores: Int): Job.Result =
    if cores <= 1 then run(feed) else
      val tram = Native.tramTable(feed)
      val n = feed.events.length
      val program = ZIO.foreachPar((0 until cores).toVector)(i =>
        slice(feed, tram, Native.bound(n, cores, i), Native.bound(n, cores, i + 1)))
        .withParallelism(cores)
      Native.combine(unsafeRun(program))
}
