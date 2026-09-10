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
  /**
   * THE PARTITION'S WORKING STATE: a live `Windows`, mutable maps, a
   * terminal accumulator being folded into. It never leaves the
   * partition and never has to be a value.
   */
  type P

  /**
   * WHAT LEAVES THE PARTITION — and the only half that has to be a
   * value: immutable, mergeable, and (for a `Wire`) describable by a
   * Schema. Splitting the two is what makes a partial able to cross a
   * process boundary without dragging an operator with it.
   */
  type W

  /**
   * The event-time functions this sink's windows depend on, in order.
   * `and` concatenates them, and the driver's single pre-pass answers
   * one `Bounds` per entry per partition — the watermark seed, and
   * what that partition may finish by itself.
   */
  def times: Vector[A => Long]

  /** a fresh partial for one partition; `bounds` has one entry per
   * entry of `times` */
  def start(bounds: Vector[Bounds]): P

  def step(p: P, a: A): Unit

  /** the end of a partition: close whatever is still open, and hand
   * back what leaves */
  def finish(p: P): W

  /** the coordinator: the partials, IN PARTITION ORDER */
  def result(ws: Vector[W]): R

  /** late elements this sink dropped */
  def drops(ws: Vector[W]): Long

  /** how many accumulators reached the COORDINATOR — the work the
   * completeness rule exists to remove, reported so that a suite can
   * assert it rather than a benchmark merely notice it */
  def merged(ws: Vector[W]): Long

  /** two sinks over one pass */
  final def and[R2](that: Sink[A, R2]): Sink[A, (R, R2)] =
    val self = this
    val n = self.times.length
    new Sink[A, (R, R2)]:
      type P = (self.P, that.P)
      type W = (self.W, that.W)
      def times: Vector[A => Long] = self.times ++ that.times
      def start(bounds: Vector[Bounds]): P =
        (self.start(bounds.take(n)), that.start(bounds.drop(n)))
      def step(p: P, a: A): Unit = { self.step(p._1, a); that.step(p._2, a) }
      def finish(p: P): W = (self.finish(p._1), that.finish(p._2))
      def result(ws: Vector[W]): (R, R2) =
        (self.result(ws.map(_._1)), that.result(ws.map(_._2)))
      def drops(ws: Vector[W]): Long =
        self.drops(ws.map(_._1)) + that.drops(ws.map(_._2))
      def merged(ws: Vector[W]): Long =
        self.merged(ws.map(_._1)) + that.merged(ws.map(_._2))

object Sink {

  /** no key and no window: fold every element straight into `into`.
   *
   * The return type NAMES `W`. Without that refinement a caller sees
   * only the abstract member and cannot say what crosses a wire —
   * which is exactly what `Wire` has to say. */
  def fold[A, Acc, R](into: Aggregator[A, Acc, R]): Sink[A, R] { type W = Acc } =
    new Sink[A, R]:
      type P = Box[Acc]
      type W = Acc
      def times: Vector[A => Long] = Vector.empty
      def start(bounds: Vector[Bounds]): P = Box(into.init)
      def step(p: P, a: A): Unit = p.value = into.add(p.value, a)
      def finish(p: P): W = p.value
      def result(ws: Vector[W]): R = into.present(ws.reduceLeft(into.merge))
      def drops(ws: Vector[W]): Long = 0L
      def merged(ws: Vector[W]): Long = ws.length.toLong

  /** one accumulator per key, no window */
  def keyed[A, K, Acc, O, IAcc, R](key: A => K, agg: Aggregator[A, Acc, O])
                                  (into: Aggregator[(K, O), IAcc, R])
  : Sink[A, R] { type W = Vector[(K, Acc)] } =
    // the same rule as `Flows.run`, checked where the sink is BUILT
    // rather than where it is driven: a keyed stage's output is a hash
    // map's order, so an order-dependent terminal over it is wrong
    Flows.ordered(into)
    new Sink[A, R]:
      type P = mutable.HashMap[K, Acc]
      type W = Vector[(K, Acc)]
      def times: Vector[A => Long] = Vector.empty
      def start(bounds: Vector[Bounds]): P = mutable.HashMap.empty[K, Acc]
      def step(p: P, a: A): Unit =
        val k = key(a)
        p.update(k, agg.add(p.getOrElse(k, agg.init), a))
      def finish(p: P): W = p.toVector
      def result(ws: Vector[W]): R =
        val all = mutable.HashMap.empty[K, Acc]
        for m <- ws do
          for (k, a) <- m do all.update(k, all.get(k).fold(a)(agg.merge(_, a)))
        var acc = into.init
        for (k, a) <- all do acc = into.add(acc, (k, agg.present(a)))
        into.present(acc)
      def drops(ws: Vector[W]): Long = 0L
      def merged(ws: Vector[W]): Long =
        var t = 0L
        for m <- ws do t += m.length
        t

  /** an event-time windowed aggregation, keyed */
  def windowed[A, K, Acc, O, IAcc, R](size: Long, slide: Long, lateness: Long,
                                      key: A => K, at: A => Long,
                                      agg: Aggregator[A, Acc, O], seeded: Boolean)
                                     (into: Aggregator[Pane[K, O], IAcc, R])
  : Sink[A, R] { type W = Handed[K, Acc, IAcc] } =
    Flows.ordered(into)
    // the same aggregator, presenting its ACCUMULATOR: what a partial
    // pane must carry so another partition's can be merged into it
    val partial = Aggregator[A, Acc, Acc](agg.init)(agg.add)(agg.merge)(identity)
    new Sink[A, R]:
      type P = Panes[K, Acc, A, O, IAcc]
      type W = Handed[K, Acc, IAcc]
      def times: Vector[A => Long] = Vector(at)
      def start(bounds: Vector[Bounds]): P =
        val w = new Windows[K, A, Acc, Acc](size, slide, lateness, key, at, partial)
        val b = if bounds.isEmpty then Bounds(Long.MinValue, Long.MinValue) else bounds.head
        if seeded && b.lower != Long.MinValue then w.seed(b.lower)
        // WITHOUT the seeding the completeness rule must not fire
        // either: its lower bound is the same number, and a partition
        // whose watermark is not the stream's has no business
        // declaring anything finished.
        val done = if seeded then b else Bounds(Long.MaxValue, Long.MinValue)
        Panes(w, mutable.HashMap.empty[(Long, K), Acc], agg, size, done, into)
      def step(p: P, a: A): Unit = p.add(a)
      def finish(p: P): W = { p.close(); p.handed }
      def result(ws: Vector[W]): R =
        // the boundary panes — everything no partition could finish —
        // merged across the partitions in index order
        val all = mutable.HashMap.empty[(Long, K), Acc]
        for w <- ws do
          for (start, k, a) <- w.boundary do
            val id = (start, k)
            all.update(id, all.get(id).fold(a)(agg.merge(_, a)))
        var acc = into.init
        for ((start, k), a) <- all do
          acc = into.add(acc, Pane(start, start + size, k, agg.present(a)))
        // and the partitions' own accumulators, each already carrying
        // the panes it finished alone
        for w <- ws do acc = into.merge(acc, w.finished)
        into.present(acc)
      def drops(ws: Vector[W]): Long =
        var d = 0L
        for w <- ws do d += w.late
        d
      def merged(ws: Vector[W]): Long =
        var t = 0L
        for w <- ws do t += w.boundary.length
        t

  def tumbling[A, K, Acc, O, IAcc, R](size: Long, lateness: Long,
                                      key: A => K, at: A => Long,
                                      agg: Aggregator[A, Acc, O], seeded: Boolean = true)
                                     (into: Aggregator[Pane[K, O], IAcc, R])
  : Sink[A, R] { type W = Handed[K, Acc, IAcc] } =
    windowed(size, size, lateness, key, at, agg, seeded)(into)

  def sliding[A, K, Acc, O, IAcc, R](size: Long, slide: Long, lateness: Long,
                                     key: A => K, at: A => Long,
                                     agg: Aggregator[A, Acc, O], seeded: Boolean = true)
                                    (into: Aggregator[Pane[K, O], IAcc, R])
  : Sink[A, R] { type W = Handed[K, Acc, IAcc] } =
    windowed(size, slide, lateness, key, at, agg, seeded)(into)

  /** a mutable cell: `Sink.fold`'s accumulator has to be per-partition
   * state the driver can hand back, and an immutable `Acc` cannot be */
  final class Box[Acc](var value: Acc)

  /**
   * One partition's windowed state.
   *
   * A pane leaving the operator goes one of two ways, and which one
   * is the whole of `dataflow-complete-panes`. If it starts after
   * `bounds.lower` and ends at or before `bounds.upper` then no other
   * partition can contribute to it, so it is PRESENTED and folded
   * into this partition's own terminal accumulator here. Otherwise it
   * is kept as an accumulator for the coordinator to merge.
   *
   * On the Wrocław job that is the difference between the
   * coordinator merging every pane the job produced and merging the
   * few that span a partition boundary.
   */
  final class Panes[K, Acc, A, O, IAcc](w: Windows[K, A, Acc, Acc],
                                        val panes: mutable.HashMap[(Long, K), Acc],
                                        agg: Aggregator[A, Acc, O],
                                        size: Long, bounds: Bounds,
                                        into: Aggregator[Pane[K, O], IAcc, ?]):
    private var acc: IAcc = into.init
    private val keep: Pane[K, Acc] => Unit = p =>
      if p.start > bounds.lower && p.start + size <= bounds.upper then
        acc = into.add(acc, Pane(p.start, p.start + size, p.key, agg.present(p.value)))
      else
        val id = (p.start, p.key)
        panes.update(id, panes.get(id).fold(p.value)(agg.merge(_, p.value)))
    def add(a: A): Unit = w.add(a)(keep)
    def close(): Unit = w.close()(keep)
    /** what LEAVES this partition, as a value */
    def handed: Handed[K, Acc, IAcc] =
      Handed(panes.toVector.map { case ((start, k), a) => (start, k, a) }, acc, w.dropped)

  /**
   * What a windowed partition hands over — and the shape of it is the
   * completeness rule made visible. `finished` is everything this
   * partition could close by itself, ALREADY FOLDED into the
   * terminal: one value, however many panes went into it. `boundary`
   * is the handful that span a partition edge and still need
   * merging. On the Wrocław job the first is 1.7 million panes and
   * the second is a hundred thousand accumulators.
   */
  final case class Handed[K, Acc, IAcc](boundary: Vector[(Long, K, Acc)],
                                        finished: IAcc, late: Long)
}
