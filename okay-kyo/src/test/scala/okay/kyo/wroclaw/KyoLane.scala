package okay.kyo.wroclaw

import okay.wroclaw.{Depart, Feed, Job, Native}

/**
 * kyo ON ITS OWN TERMS (docs/benchmarks.md §20).
 *
 * WHAT THE LIBRARY GIVES ITS USER. kyo has no windowing operator at
 * all — not even a processing-time one — so the job is the fold a kyo
 * user writes: `Native.Fold`, panes as keys in a map, nothing evicted,
 * carried by `kyo.Stream`. The earlier version of this lane handed kyo
 * `okay.Windows`, which measured the plumbing and held constant the
 * operator itself.
 *
 * THE SOURCE is `Stream.init(ArraySeq.unsafeWrapArray(...), 256)` —
 * the library's own chunked constructor at the granularity every other
 * lane uses; `ArraySeq.slice` on a wrapped array copies, so the
 * parallel rows wrap once and slice the SEQ rather than the array.
 *
 * ONE CORE evaluates where it is built (`.eval`), with no scheduler
 * involved. MORE THAN ONE goes through `Async.parallel(p)`, kyo's own
 * bounded fan-out, blocked on by `KyoApp.Unsafe.runAndBlock` — the
 * same shape compare/src/jmh's `kyoAsync` benchmark uses, so the two
 * numbers are readable together.
 */
object KyoLane {

  import _root_.kyo.*

  /** stage 1 and the fold over one contiguous slice, as a kyo Stream */
  private def slice(tram: Array[Boolean],
                    events: scala.collection.immutable.ArraySeq[Depart],
                    from: Int, until: Int): Native.Fold =
    val f = new Native.Fold(tram)
    Stream.init(events.slice(from, until), 256)
      .filter((d: Depart) => f.known(d))
      .map((d: Depart) => f.enrich(d))
      .runFold(())((_: Unit, r: okay.wroclaw.Ride) => f.add(r))
      .eval
    f

  /** one core, evaluated where it is built */
  def run(feed: Feed): Job.Result =
    val events = scala.collection.immutable.ArraySeq.unsafeWrapArray(feed.events)
    slice(Native.tramTable(feed), events, 0, feed.events.length).result

  /**
   * P cores in kyo's own vocabulary: `Async.parallel(p)` over one
   * suspended fold per slice, blocked on once. `parallel` keeps the
   * results in the order of its inputs, and that order is
   * load-bearing — `Fold.absorb` stitches the bunching pair that
   * straddles a slice boundary, and that stitch is not commutative.
   */
  def parallel(feed: Feed, cores: Int): Job.Result =
    if cores <= 1 then run(feed) else
      import AllowUnsafe.embrace.danger
      val tram = Native.tramTable(feed)
      val events = scala.collection.immutable.ArraySeq.unsafeWrapArray(feed.events)
      val n = feed.events.length
      val slices: Seq[Native.Fold < (Abort[Nothing] & Async)] =
        (0 until cores).map(i =>
          IO(slice(tram, events, Native.bound(n, cores, i), Native.bound(n, cores, i + 1))))
      val folds = KyoApp.Unsafe.runAndBlock(Duration.Infinity)(
        Async.parallel(cores)(slices)).getOrThrow
      Native.combine(folds)
}
