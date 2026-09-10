package okay.flink.wroclaw

import okay.Windows
import scala.collection.mutable

/**
 * THE THREE IN-PROCESS STREAM LIBRARIES on the same job: fs2,
 * zio-streams and kyo.
 *
 * WHAT IS BEING COMPARED, said before the numbers. None of the three
 * has an event-time window — fs2 has `groupWithin`, ZIO
 * `groupedWithin`, kyo nothing at all, and all of those are
 * PROCESSING time, which is a different operator answering a
 * different question. So each lane gets `okay.Windows`, the same
 * operator okay's own lane and the JDK lane use, and what the three
 * numbers differ by is the library's plumbing around identical work:
 * how an element reaches a fold, and what `map` and `filter` cost on
 * the way.
 *
 * That is a narrower claim than "okay is faster than fs2", and it is
 * the only one this file supports. It is also the useful one: on a
 * stateful event-time job, the windowing is the same code whoever
 * carries the elements, so the plumbing is exactly what a reader is
 * choosing between.
 *
 * SOURCES, each the library's own chunked constructor, sliced to 256
 * to match `Chunks.fromIterator(_, 256)` in the okay lane (the lane
 * rules of this document: a competitor is priced from the source its
 * author intended, at a matched granularity):
 *
 *   - fs2: `Stream.chunk(Chunk.array(...))` re-chunked by
 *     `chunkLimit(256)`, and PURE — `Stream[Pure, *]` compiles with no
 *     cats-effect runtime and no `unsafeRunSync`, which §0 prices at
 *     7.6 us and which this lane therefore does not pay
 *   - ZIO: `ZStream.fromChunk(Chunk.fromArray(...)).rechunk(256)`,
 *     run once through `Unsafe.unsafe`
 *   - kyo: `Stream.init(ArraySeq.unsafeWrapArray(...), 256)`, `.eval`
 */
object LibLanes {

  /** the per-element work, identical in all three lanes */
  private final class Fold(feed: Feed) {
    private val tram = feed.routes.iterator.map(_.tram).toArray
    private val sink = new OkayLane.Sink(tram)
    private val routeWindows = Windows.tumbling[Int, Ride, Job.Acc, Job.Stats](
      Job.WindowMs, Job.Lateness)(_.route)(_.ts)(Job.stats)
    private val stopWindows = Windows.sliding[Int, Ride, Job.Acc, Job.Stats](
      Job.SlideWindowMs, Job.SlideMs, Job.Lateness)(_.stop)(_.ts)(Job.stats)
    private val lastSeen = mutable.LongMap.empty[Long]

    def known(d: Depart): Boolean = d.route >= 0 && d.route < tram.length

    def enrich(d: Depart): Ride =
      new Ride(d.ts, d.route, d.stop, d.vehicle, d.delay, tram(d.route))

    def add(r: Ride): Unit =
      routeWindows.add(r)(sink.route)
      stopWindows.add(r)(sink.stop)
      val key = (r.route.toLong << 20) | r.stop.toLong
      val prev = lastSeen.getOrElse(key, Long.MinValue)
      if prev != Long.MinValue then
        val gap = Math.abs(r.ts - prev)
        if gap < Job.BunchMs then sink.bunch(gap)
      lastSeen.update(key, r.ts)

    def result: Job.Result =
      routeWindows.close()(sink.route)
      stopWindows.close()(sink.stop)
      sink.result
  }

  /** fs2, pure: no cats-effect runtime in the lane at all */
  def fs2(feed: Feed): Job.Result = {
    val f = new Fold(feed)
    _root_.fs2.Stream.chunk(_root_.fs2.Chunk.array(feed.events))
      .chunkLimit(256).flatMap(_root_.fs2.Stream.chunk)
      .filter(f.known).map(f.enrich)
      .compile.fold(())((_, r) => f.add(r))
    f.result
  }

  /** zio-streams, run once through the default runtime */
  def zio(feed: Feed): Job.Result = {
    import _root_.zio.*
    val f = new Fold(feed)
    val program = _root_.zio.stream.ZStream
      .fromChunk(Chunk.fromArray(feed.events)).rechunk(256)
      .filter(f.known).map(f.enrich)
      .runFold(())((_, r) => f.add(r))
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(program).getOrThrowFiberFailure())
    f.result
  }

  /** kyo, evaluated where it is built */
  def kyo(feed: Feed): Job.Result = {
    import _root_.kyo.*
    val f = new Fold(feed)
    Stream.init(scala.collection.immutable.ArraySeq.unsafeWrapArray(feed.events), 256)
      .filter((d: Depart) => f.known(d))
      .map((d: Depart) => f.enrich(d))
      .runFold(())((_: Unit, r: Ride) => f.add(r))
      .eval
    f.result
  }
}
