package okay.cluster

import okay.{Aggregator, Pane, Windows}
import scala.collection.mutable

/**
 * ONE OUTPUT OF A MULTI-OUTPUT JOB (specs/dataflow.md, stage 3): a
 * keyed or windowed stage together with what its output is folded
 * into, and `and`, which pairs two of them.
 *
 * WHY THIS EXISTS. Stage 1 allows one keyed stage per flow, so
 * Wrocław's three keyed stages were three flows and the feed was read
 * three times where §20's hand-written lane reads it once. That is
 * also where docs/benchmarks.md §20 locates the real asymmetry
 * between okay and an engine: a fan-out in one JVM is calling three
 * methods with the same reference, and in Flink the same fan-out is
 * three shuffles.
 *
 * THE SHAPE IS DELIBERATELY `Aggregator.zip`'s. The core has computed
 * two statistics in one pass over one stream since P1 —
 * `count zip sum` — and this is that idea one level up, over stages
 * that carry keys, windows and watermarks instead of scalars. Nothing
 * new is invented; `and` is `zip` where the accumulators happen to be
 * pane maps.
 *
 * WHAT A FAN DOES NOT DO, said here rather than discovered later: it
 * finishes by MERGE. The exchange stays available to single-stage
 * flows through `Flows.run`. Stage 2 measured the crossover at about
 * 100 000 accumulators and a fan of Wrocław-sized stages is three
 * orders of magnitude under it, so a per-sink exchange inside a fan
 * is machinery nothing has asked for yet.
 */
abstract class Sink[A, R]:
  /** what one partition accumulates for this sink */
  type P

  /**
   * The event-time functions whose watermarks this sink needs seeded,
   * in order. `and` concatenates them, and the driver's single
   * pre-pass computes one prefix maximum per entry — which is what
   * keeps stage 1's theorem (a slice's watermark is the stream's)
   * true through a fan of stages that may window on different times.
   */
  def times: Vector[A => Long]

  /** a fresh partial for one partition; `seeds` has one entry per
   * entry of `times` */
  def start(seeds: Vector[Long]): P

  def step(p: P, a: A): Unit

  /** the end of a partition closes whatever the sink still holds open */
  def done(p: P): Unit

  /** the coordinator: the partials, IN PARTITION ORDER */
  def result(ps: Vector[P]): R

  /** late elements this sink dropped */
  def drops(ps: Vector[P]): Long

  /** two sinks over one pass */
  final def and[R2](that: Sink[A, R2]): Sink[A, (R, R2)] =
    val self = this
    val n = self.times.length
    new Sink[A, (R, R2)]:
      type P = (self.P, that.P)
      def times: Vector[A => Long] = self.times ++ that.times
      def start(seeds: Vector[Long]): P =
        (self.start(seeds.take(n)), that.start(seeds.drop(n)))
      def step(p: P, a: A): Unit = { self.step(p._1, a); that.step(p._2, a) }
      def done(p: P): Unit = { self.done(p._1); that.done(p._2) }
      def result(ps: Vector[P]): (R, R2) =
        (self.result(ps.map(_._1)), that.result(ps.map(_._2)))
      def drops(ps: Vector[P]): Long =
        self.drops(ps.map(_._1)) + that.drops(ps.map(_._2))

object Sink {

  /** no key and no window: fold every element straight into `into` */
  def fold[A, Acc, R](into: Aggregator[A, Acc, R]): Sink[A, R] =
    new Sink[A, R]:
      type P = Box[Acc]
      def times: Vector[A => Long] = Vector.empty
      def start(seeds: Vector[Long]): P = Box(into.init)
      def step(p: P, a: A): Unit = p.value = into.add(p.value, a)
      def done(p: P): Unit = ()
      def result(ps: Vector[P]): R = into.present(ps.map(_.value).reduceLeft(into.merge))
      def drops(ps: Vector[P]): Long = 0L

  /** one accumulator per key, no window */
  def keyed[A, K, Acc, O, IAcc, R](key: A => K, agg: Aggregator[A, Acc, O])
                                  (into: Aggregator[(K, O), IAcc, R]): Sink[A, R] =
    // the same rule as `Flows.run`, checked where the sink is BUILT
    // rather than where it is driven: a keyed stage's output is a hash
    // map's order, so an order-dependent terminal over it is wrong
    Flows.ordered(into)
    new Sink[A, R]:
      type P = mutable.HashMap[K, Acc]
      def times: Vector[A => Long] = Vector.empty
      def start(seeds: Vector[Long]): P = mutable.HashMap.empty[K, Acc]
      def step(p: P, a: A): Unit =
        val k = key(a)
        p.update(k, agg.add(p.getOrElse(k, agg.init), a))
      def done(p: P): Unit = ()
      def result(ps: Vector[P]): R =
        val all = mutable.HashMap.empty[K, Acc]
        for m <- ps do
          for (k, a) <- m do all.update(k, all.get(k).fold(a)(agg.merge(_, a)))
        var acc = into.init
        for (k, a) <- all do acc = into.add(acc, (k, agg.present(a)))
        into.present(acc)
      def drops(ps: Vector[P]): Long = 0L

  /** an event-time windowed aggregation, keyed */
  def windowed[A, K, Acc, O, IAcc, R](size: Long, slide: Long, lateness: Long,
                                      key: A => K, at: A => Long,
                                      agg: Aggregator[A, Acc, O], seeded: Boolean)
                                     (into: Aggregator[Pane[K, O], IAcc, R]): Sink[A, R] =
    Flows.ordered(into)
    // the same aggregator, presenting its ACCUMULATOR: what a partial
    // pane must carry so another partition's can be merged into it
    val partial = Aggregator[A, Acc, Acc](agg.init)(agg.add)(agg.merge)(identity)
    new Sink[A, R]:
      type P = Panes[K, Acc, A]
      def times: Vector[A => Long] = if seeded then Vector(at) else Vector.empty
      def start(seeds: Vector[Long]): P =
        val w = new Windows[K, A, Acc, Acc](size, slide, lateness, key, at, partial)
        if seeds.nonEmpty && seeds.head != Long.MinValue then w.seed(seeds.head)
        Panes(w, mutable.HashMap.empty[(Long, K), Acc], agg)
      def step(p: P, a: A): Unit = p.add(a)
      def done(p: P): Unit = p.close()
      def result(ps: Vector[P]): R =
        val all = mutable.HashMap.empty[(Long, K), Acc]
        for p <- ps do
          for (id, a) <- p.panes do all.update(id, all.get(id).fold(a)(agg.merge(_, a)))
        var acc = into.init
        for ((start, k), a) <- all do
          acc = into.add(acc, Pane(start, start + size, k, agg.present(a)))
        into.present(acc)
      def drops(ps: Vector[P]): Long =
        var d = 0L
        for p <- ps do d += p.late
        d

  def tumbling[A, K, Acc, O, IAcc, R](size: Long, lateness: Long,
                                      key: A => K, at: A => Long,
                                      agg: Aggregator[A, Acc, O], seeded: Boolean = true)
                                     (into: Aggregator[Pane[K, O], IAcc, R]): Sink[A, R] =
    windowed(size, size, lateness, key, at, agg, seeded)(into)

  def sliding[A, K, Acc, O, IAcc, R](size: Long, slide: Long, lateness: Long,
                                     key: A => K, at: A => Long,
                                     agg: Aggregator[A, Acc, O], seeded: Boolean = true)
                                    (into: Aggregator[Pane[K, O], IAcc, R]): Sink[A, R] =
    windowed(size, slide, lateness, key, at, agg, seeded)(into)

  /** a mutable cell: `Sink.fold`'s accumulator has to be per-partition
   * state the driver can hand back, and an immutable `Acc` cannot be */
  final class Box[Acc](var value: Acc)

  /** one partition's windowed state: the operator, the panes it has
   * closed so far as ACCUMULATORS, and the late elements it dropped */
  final class Panes[K, Acc, A](w: Windows[K, A, Acc, Acc],
                               val panes: mutable.HashMap[(Long, K), Acc],
                               agg: Aggregator[A, Acc, ?]):
    private val keep: Pane[K, Acc] => Unit = p =>
      val id = (p.start, p.key)
      panes.update(id, panes.get(id).fold(p.value)(agg.merge(_, p.value)))
    def add(a: A): Unit = w.add(a)(keep)
    def close(): Unit = w.close()(keep)
    def late: Long = w.dropped
}
