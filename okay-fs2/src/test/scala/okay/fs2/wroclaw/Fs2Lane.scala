package okay.fs2.wroclaw

import okay.wroclaw.{Feed, Job, Native}

/**
 * fs2 ON ITS OWN TERMS (docs/benchmarks.md §20).
 *
 * WHAT THE LIBRARY GIVES ITS USER, said before the number. fs2 has no
 * event-time window: `groupWithin` is PROCESSING time and answers a
 * different question. So a fs2 user writing this job writes the fold
 * below — `Native.Fold`, panes as keys in a map, nothing evicted —
 * and carries it with fs2's own combinators. The earlier version of
 * this lane handed fs2 `okay.Windows`, which measured the plumbing
 * around identical work and quietly held constant the very thing a
 * reader is choosing between.
 *
 * THE SOURCE is `Stream.chunk(Chunk.array(...))` re-chunked to 256, to
 * match `Chunks.fromIterator(_, 256)` in the okay lane (§20's lane
 * rules: a competitor is priced from the source its author intended,
 * at a matched granularity).
 *
 * ONE CORE IS PURE: `Stream[Pure, *]` compiles with no cats-effect
 * runtime and no `unsafeRunSync`, which §0 prices at 7.6 us and which
 * this row therefore does not pay. MORE THAN ONE IS NOT, and cannot
 * be: parallelism in fs2 needs `Concurrent`, so the parallel rows run
 * on `IO` and pay for the runtime. That is not a handicap the
 * benchmark imposes, it is the library's own shape — and it is why
 * the 1-core row is the pure one rather than an IO row with the
 * parallelism set to one.
 */
object Fs2Lane {

  import _root_.fs2.{Chunk, Pure, Stream}

  /** stage 1 and the fold over one contiguous slice of the arrival
   * order, as a pure fs2 stream */
  private def slice(feed: Feed, tram: Array[Boolean], from: Int, until: Int): Native.Fold =
    val f = new Native.Fold(tram)
    val source: Stream[Pure, okay.wroclaw.Depart] =
      Stream.chunk(Chunk.array(feed.events, from, until - from))
    source.chunkLimit(256).flatMap(Stream.chunk)
      .filter(f.known).map(f.enrich)
      .compile.fold(())((_, r) => f.add(r))
    f

  /** one core: pure fs2, no runtime under it */
  def run(feed: Feed): Job.Result =
    slice(feed, Native.tramTable(feed), 0, feed.events.length).result

  /**
   * P cores, in fs2's own vocabulary: a stream of the slices, each
   * folded in `IO`, run `parEvalMap` — which keeps the OUTPUT ORDER of
   * its inputs, and the order is load-bearing here (`Fold.absorb`
   * stitches the bunching pair that straddles a slice boundary, and
   * that stitch is not commutative). `parEvalMapUnordered` would be
   * marginally cheaper and wrong.
   */
  def parallel(feed: Feed, cores: Int): Job.Result =
    if cores <= 1 then run(feed) else
      import cats.effect.IO
      import cats.effect.unsafe.implicits.global
      val tram = Native.tramTable(feed)
      val n = feed.events.length
      val folds = Stream.emits(0 until cores).covary[IO]
        .parEvalMap(cores)(i =>
          IO(slice(feed, tram, Native.bound(n, cores, i), Native.bound(n, cores, i + 1))))
        .compile.toVector.unsafeRunSync()
      Native.combine(folds)
}
