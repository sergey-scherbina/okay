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

  /**
   * What has left the partition SO FAR, without closing anything —
   * one epoch's worth (specs/dataflow.md, stage 6a).
   *
   * `finish` is this plus "and there is no more input". A windowed
   * operator has already emitted every pane its watermark closed, so
   * `peek` hands those over and keeps the open ones; `finish` sweeps
   * the rest out too.
   *
   * Handing over TWICE must not double-count, which is why every
   * implementation below empties what it hands over.
   */
  def peek(p: P): W

  /**
   * THE COORDINATOR'S RUNNING STATE.
   *
   * A batch run folds every partial once and presents. A stream has
   * no "once", so the coordinator keeps a state and folds epoch by
   * epoch, retiring what the watermark has closed. `result` below is
   * that fold with the whole stream in one epoch and a watermark of
   * infinity — so the batch answer stays the definition of correct.
   */
  type S

  def empty: S

  /**
   * Take one epoch's partials, IN PARTITION ORDER, and retire
   * everything the watermark has closed.
   *
   * `watermark` is the least event time that can still arrive: a pane
   * ending at or before it can receive nothing more and is folded
   * into the answer now. `Long.MaxValue` retires everything, which is
   * what the end of a stream means.
   */
  def absorb(s: S, ws: Vector[W], watermark: Long): S

  /** what has been retired so far */
  def emit(s: S): R

  /** the batch answer: one epoch, and nothing left to come */
  final def result(ws: Vector[W]): R = emit(absorb(empty, ws, Long.MaxValue))

  /** late elements this sink dropped */
  def drops(ws: Vector[W]): Long

  /**
   * HOW FAR OUT OF ORDER THIS SINK'S INPUT MAY ARRIVE — the declared
   * lateness of its windows, and zero when it has none.
   *
   * The coordinator needs a bound on the future to retire a pane, and
   * in a stream the OBSERVED backwardness is not one: it is only what
   * has been seen so far and it grows. The declared lateness is the
   * user's own contract — an element further out of order than this
   * is late and dropped — so it is the only number that is true of
   * what has not arrived yet.
   */
  def slack: Long

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
      type S = (self.S, that.S)
      def empty: S = (self.empty, that.empty)
      def absorb(s: S, ws: Vector[W], watermark: Long): S =
        (self.absorb(s._1, ws.map(_._1), watermark),
         that.absorb(s._2, ws.map(_._2), watermark))
      def emit(s: S): (R, R2) = (self.emit(s._1), that.emit(s._2))
      def slack: Long = math.max(self.slack, that.slack)
      def times: Vector[A => Long] = self.times ++ that.times
      def start(bounds: Vector[Bounds]): P =
        (self.start(bounds.take(n)), that.start(bounds.drop(n)))
      def step(p: P, a: A): Unit = { self.step(p._1, a); that.step(p._2, a) }
      def finish(p: P): W = (self.finish(p._1), that.finish(p._2))
      def peek(p: P): W = (self.peek(p._1), that.peek(p._2))
      def drops(ws: Vector[W]): Long =
        self.drops(ws.map(_._1)) + that.drops(ws.map(_._2))
      def merged(ws: Vector[W]): Long =
        self.merged(ws.map(_._1)) + that.merged(ws.map(_._2))

object Sink {

  /** no key and no window: fold every element straight into `into`.
   *
   * The return type NAMES `W` — and, since stage 8, `S` as well.
   * Without those refinements a caller sees only the abstract members
   * and cannot say what crosses a wire or what the coordinator holds,
   * which is exactly what `Wire` has to say. */
  def fold[A, Acc, R](into: Aggregator[A, Acc, R])
  : Sink[A, R] { type W = Acc; type S = Acc } =
    new Sink[A, R]:
      type P = Box[Acc]
      type W = Acc
      type S = Acc
      def times: Vector[A => Long] = Vector.empty
      def start(bounds: Vector[Bounds]): P = Box(into.init)
      def step(p: P, a: A): Unit = p.value = into.add(p.value, a)
      def finish(p: P): W = peek(p)
      def peek(p: P): W = { val out = p.value; p.value = into.init; out }
      def empty: S = into.init
      // no key and no window: nothing is ever open, so every epoch's
      // partials retire immediately and the watermark says nothing
      def absorb(s: S, ws: Vector[W], watermark: Long): S = ws.foldLeft(s)(into.merge)
      def emit(s: S): R = into.present(s)
      def slack: Long = 0L
      def drops(ws: Vector[W]): Long = 0L
      def merged(ws: Vector[W]): Long = ws.length.toLong

  /** one accumulator per key, no window */
  def keyed[A, K, Acc, O, IAcc, R](key: A => K, agg: Aggregator[A, Acc, O])
                                  (into: Aggregator[(K, O), IAcc, R])
  : Sink[A, R] { type W = Vector[(K, Acc)]; type S = mutable.HashMap[K, Acc] } =
    // the same rule as `Flows.run`, checked where the sink is BUILT
    // rather than where it is driven: a keyed stage's output is a hash
    // map's order, so an order-dependent terminal over it is wrong
    Flows.ordered(into)
    new Sink[A, R]:
      type P = mutable.HashMap[K, Acc]
      type W = Vector[(K, Acc)]
      type S = mutable.HashMap[K, Acc]
      def times: Vector[A => Long] = Vector.empty
      def start(bounds: Vector[Bounds]): P = mutable.HashMap.empty[K, Acc]
      def step(p: P, a: A): Unit =
        val k = key(a)
        p.update(k, agg.add(p.getOrElse(k, agg.init), a))
      def finish(p: P): W = peek(p)
      def peek(p: P): W = { val out = p.toVector; p.clear(); out }
      def empty: S = mutable.HashMap.empty[K, Acc]
      /**
       * A keyed stage with no window NEVER retires: a key can always
       * be seen again, so nothing is ever closed and the watermark
       * has nothing to say. Its state grows with the key space, which
       * is the same property Flink's keyed state has and the same
       * reason a stream over unbounded keys needs a TTL nobody has
       * asked for here yet.
       */
      def absorb(s: S, ws: Vector[W], watermark: Long): S =
        for m <- ws do
          for (k, a) <- m do s.update(k, s.get(k).fold(a)(agg.merge(_, a)))
        s
      def emit(s: S): R =
        var acc = into.init
        for (k, a) <- s do acc = into.add(acc, (k, agg.present(a)))
        into.present(acc)
      def slack: Long = 0L
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
  : Sink[A, R] { type W = Handed[K, Acc, IAcc]; type S = Open[K, Acc, IAcc] } =
    Flows.ordered(into)
    // the same aggregator, presenting its ACCUMULATOR: what a partial
    // pane must carry so another partition's can be merged into it
    val partial = Aggregator[A, Acc, Acc](agg.init)(agg.add)(agg.merge)(identity)
    new Sink[A, R]:
      type P = Panes[K, Acc, A, O, IAcc]
      type W = Handed[K, Acc, IAcc]
      type S = Open[K, Acc, IAcc]
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
      def peek(p: P): W = p.handed
      def empty: S = Open(mutable.HashMap.empty[(Long, K), Acc], into.init)
      /**
       * The boundary panes — everything no partition could finish
       * alone — merged across the partitions in index order, and then
       * RETIRED where the watermark has passed their end.
       *
       * A pane that is still open stays in the map and meets the next
       * epoch's partials; a pane the watermark has closed is
       * presented and folded into the answer, and is gone. That is
       * what keeps a stream's coordinator bounded rather than growing
       * with the run.
       */
      def absorb(s: S, ws: Vector[W], watermark: Long): S =
        for w <- ws do
          for (start, k, a) <- w.boundary do
            val id = (start, k)
            s.panes.update(id, s.panes.get(id).fold(a)(agg.merge(_, a)))
          // the partitions' own accumulators are already finished
          s.acc = into.merge(s.acc, w.finished)
        val closed = s.panes.keysIterator.filter((start, _) => start + size <= watermark).toVector
        for id <- closed do
          s.panes.remove(id).foreach { a =>
            s.acc = into.add(s.acc, Pane(id._1, id._1 + size, id._2, agg.present(a)))
          }
        s
      def emit(s: S): R = into.present(s.acc)
      def slack: Long = lateness
      def drops(ws: Vector[W]): Long =
        var d = 0L
        for w <- ws do d += w.late
        d
      def merged(ws: Vector[W]): Long =
        var t = 0L
        for w <- ws do t += w.boundary.length
        t

  /**
   * THE TERMINAL THAT WRITES A PANE OUT (specs/dataflow.md, stage 6c)
   * instead of folding it into a value the caller reads at the end,
   * and answers how many panes it wrote.
   *
   * It is an ordinary `Aggregator`, which is the point: a sink whose
   * output LEAVES needs no new seam in the engine, because "leaves"
   * is just what this particular fold does on the way past.
   *
   * WHAT THE ENGINE PROMISES, and it is not what this field usually
   * says. See `writing` below.
   */
  def writes[K, O](write: Pane[K, O] => Unit): Aggregator[Pane[K, O], Long, Long] =
    Aggregator[Pane[K, O], Long, Long](0L)((n, p) => { write(p); n + 1L })((a, b) => a + b)(identity)

  /**
   * A WINDOWED SINK WHOSE RETIRED PANES ARE WRITTEN OUT
   * (specs/dataflow.md, stage 6c).
   *
   * THE IDENTITY IS ALREADY THERE. A retired pane is `(start, key)`,
   * and that pair is unique by construction — a window is a half-open
   * interval and a key is a key. So a writer that is KEYED by it needs
   * no protocol at all: no transaction, no two-phase commit, no dedup
   * table, nothing to tune.
   *
   * WHAT IS PROMISED, in the two halves that are actually true:
   *
   *   1. every (window, key) this run retires is offered to `write`
   *      AT LEAST ONCE, in no particular order;
   *   2. every offer of one identity carries the SAME value.
   *
   * and therefore a keyed writer ends with each identity present
   * exactly once, holding the batch answer — an exactly-once OUTCOME,
   * which is the phrase specs/persist.md already settled on.
   *
   * IT IS NOT EXACTLY-ONCE EXECUTION, and the difference is not
   * pedantry — it is measured. `TestOnce` kills workers and counts:
   * the offers outnumber the panes, because a partition that dies is
   * recomputed and the panes it had already finished by itself are
   * written again. That repeat is not a defect to be removed; every
   * stage since 5 depends on a partition being recomputable, and (2)
   * is what makes it harmless. A writer that COUNTS rather than keys —
   * an append to a log, `+= 1` — gets at-least-once and nothing more,
   * which is why the answer here is the number of OFFERS and is
   * documented as such.
   *
   * The third qualifier is the run, and stage 8 moved it rather than
   * removing it. A coordinator with no journal that dies and starts
   * again re-offers EVERYTHING. One with a `Checkpoint` re-offers
   * only the epoch that was in flight — a pane is written while its
   * epoch is being absorbed and the epoch is committed after, so a
   * death in between loses the record of writes that happened. The
   * window is one epoch wide, it is named in specs/dataflow.md rather
   * than closed, and what makes it harmless is the same identity that
   * makes a recomputed partition harmless: same key, same value, one
   * row.
   */
  def writing[A, K, Acc, O](size: Long, slide: Long, lateness: Long,
                            key: A => K, at: A => Long,
                            agg: Aggregator[A, Acc, O], seeded: Boolean)
                           (write: Pane[K, O] => Unit)
  : Sink[A, Long] { type W = Handed[K, Acc, Long]; type S = Open[K, Acc, Long] } =
    windowed(size, slide, lateness, key, at, agg, seeded)(writes(write))

  def tumblingTo[A, K, Acc, O](size: Long, lateness: Long,
                               key: A => K, at: A => Long,
                               agg: Aggregator[A, Acc, O], seeded: Boolean = true)
                              (write: Pane[K, O] => Unit)
  : Sink[A, Long] { type W = Handed[K, Acc, Long]; type S = Open[K, Acc, Long] } =
    writing(size, size, lateness, key, at, agg, seeded)(write)

  def slidingTo[A, K, Acc, O](size: Long, slide: Long, lateness: Long,
                              key: A => K, at: A => Long,
                              agg: Aggregator[A, Acc, O], seeded: Boolean = true)
                             (write: Pane[K, O] => Unit)
  : Sink[A, Long] { type W = Handed[K, Acc, Long]; type S = Open[K, Acc, Long] } =
    writing(size, slide, lateness, key, at, agg, seeded)(write)

  def tumbling[A, K, Acc, O, IAcc, R](size: Long, lateness: Long,
                                      key: A => K, at: A => Long,
                                      agg: Aggregator[A, Acc, O], seeded: Boolean = true)
                                     (into: Aggregator[Pane[K, O], IAcc, R])
  : Sink[A, R] { type W = Handed[K, Acc, IAcc]; type S = Open[K, Acc, IAcc] } =
    windowed(size, size, lateness, key, at, agg, seeded)(into)

  def sliding[A, K, Acc, O, IAcc, R](size: Long, slide: Long, lateness: Long,
                                     key: A => K, at: A => Long,
                                     agg: Aggregator[A, Acc, O], seeded: Boolean = true)
                                    (into: Aggregator[Pane[K, O], IAcc, R])
  : Sink[A, R] { type W = Handed[K, Acc, IAcc]; type S = Open[K, Acc, IAcc] } =
    windowed(size, slide, lateness, key, at, agg, seeded)(into)

  /** a mutable cell: `Sink.fold`'s accumulator has to be per-partition
   * state the driver can hand back, and an immutable `Acc` cannot be */
  final class Box[Acc](var value: Acc)

  /** the COORDINATOR's running state for a windowed sink: the panes
   * still open across partitions, and everything already retired */
  final class Open[K, Acc, IAcc](val panes: mutable.HashMap[(Long, K), Acc], var acc: IAcc)

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
    /**
     * What LEAVES this partition, as a value — AND IT EMPTIES.
     *
     * A batch run asks once, so it never mattered. An epoch asks
     * every round, and a pane handed over twice would be merged
     * twice: the boundary map is cleared and the terminal
     * accumulator is reset to its zero, so each epoch hands over
     * exactly what it produced since the last one. The late count is
     * the operator's own running total, so it is DIFFERENCED rather
     * than reset — the operator has no way to forget it.
     */
    def handed: Handed[K, Acc, IAcc] =
      val out = Handed(panes.toVector.map { case ((start, k), a) => (start, k, a) },
        acc, w.dropped - handedLate)
      handedLate = w.dropped
      panes.clear()
      acc = into.init
      out

    private var handedLate: Long = 0L

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
