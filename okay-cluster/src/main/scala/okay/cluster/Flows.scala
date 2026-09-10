package okay.cluster

import okay.*
import okay.given
import scala.collection.mutable

/**
 * What a run answers: the value, and what the engine had to say about
 * it. Late elements are DROPPED by an event-time window and an engine
 * that does not report how many is hiding a wrong answer; `reducers`
 * is what the plan actually did, which for `Finish.Auto` is decided
 * during the run and is otherwise unknowable from the outside.
 */
final case class Run[O](value: O, dropped: Long, partitions: Int, reducers: Int,
                       merged: Long = 0L)

/**
 * What one partition may finish BY ITSELF, in one event-time column.
 *
 * A windowed pane is complete in this partition — no other partition
 * can contribute to it — when it starts after `lower` and ends at or
 * before `upper`. Such a pane is presented and folded into the
 * terminal here rather than shipped to the coordinator as an
 * accumulator, which is the difference between merging a handful of
 * boundary panes and merging every pane the job produced.
 *
 * `lower` is also the watermark seed: a partition starts where the
 * stream stood when it began.
 */
final case class Bounds(lower: Long, upper: Long)

/**
 * THE EXECUTOR (specs/dataflow.md, stage 1): a `Flow` over N
 * partitions, in one process, on one fibre per partition.
 *
 * The whole of stage 1 is one idea. A keyed stage does NOT have to
 * move its records to the key's owner; it has to move its
 * ACCUMULATORS, and an `Aggregator` is precisely the value that makes
 * that legal. So every partition folds every key it happens to see,
 * and the coordinator merges what comes back — which moves one
 * accumulator per (key, window) per partition where Spark's
 * `reduceByKey` and Flink's `keyBy` move the dataset. Stage 2 adds
 * the exchange for the case this cannot serve, which is the case
 * where the merged result does not fit one node.
 *
 * TWO THINGS THE ORDER DECIDES, and both are load-bearing:
 *
 *   - partials are merged BY PARTITION INDEX, never in the order the
 *     fibres happen to finish. For a plain `Aggregator` that costs
 *     nothing (its merge is commutative). For a `Sequential` — a
 *     keyed state machine's slice summary — it is the difference
 *     between the right answer and a plausible one.
 *   - a windowed partition is SEEDED with the greatest event time
 *     before it, so its watermark is the watermark the whole stream
 *     would have had at that point. Without it a slice closes panes
 *     later than the stream does and drops fewer late elements, and
 *     the parallel answer quietly stops being the serial one.
 */
object Flows {

  /** run the plan and fold its output elements with `into` */
  def fold[A, Acc, O](flow: Flow[A], into: Aggregator[A, Acc, O])
                     (using Scheduler): O ! Async =
    run(flow, into).map(_.value)

  /** the same, with what the engine has to report about the run */
  def run[A, Acc, O](flow: Flow[A], into: Aggregator[A, Acc, O])
                    (using Scheduler): Run[O] ! Async =
    val job = shape(flow).job(into)
    val n = job.parts
    if !job.ordered then ordered(into)
    val seeds: Vector[Long] ! Async =
      val pre = job.prepass
      if pre == null then pure[Async, Vector[Long]](Vector.fill(n)(Long.MinValue))
      else parallel(n)(i => pre.nn(i)).map(before)
    seeds.flatMap: sd =>
      parallel(n)(i => job.work(i, sd(i))).flatMap: ps =>
        val r = job.reducers(ps)
        parallel(r)(k => job.reduce(ps, k, r)).map: accs =>
          Run(into.present(accs.reduceLeft(into.merge)), job.drops(ps), n, r)

  /**
   * A keyed stage's output is a hash map's iteration order, not the
   * input's, whether one reducer produced it or eight. So an
   * order-dependent TERMINAL over it is wrong before any exchange is
   * involved, and saying so here is cheaper than a wrong number.
   *
   * The keyed aggregator itself is a different matter and is NOT
   * refused: a reducer owns a hash share of the keys and merges its
   * buckets partition by partition, in index order, so a `Sequential`
   * survives the exchange intact. That asymmetry is the whole reason
   * the distinction is a type.
   */
  private[cluster] def ordered(into: Aggregator[?, ?, ?]): Unit = into match
    case _: Sequential[?, ?, ?] =>
      throw IllegalArgumentException(
        "the terminal aggregator is a Sequential, but this plan's output comes out of a " +
          "keyed stage, whose order is a hash map's and not the input's. Use a commutative " +
          "terminal, or put the order-dependent aggregation IN the keyed stage, where the " +
          "engine merges by partition index (specs/dataflow.md, stage 2).")
    case _ => ()

  /**
   * SEVERAL SINKS, ONE PASS (specs/dataflow.md, stage 3).
   *
   * `Flows.run` drives a plan with one keyed stage in it. A job with
   * three of them was therefore three plans and three readings of the
   * source — which is the one place §20's hand-written lane was still
   * doing something the engine could not. Here the keyed stages live
   * in the SINKS instead, `Sink.and` pairs them, and one pass over
   * each partition feeds all of them: in this process a fan-out is
   * calling three methods with the same reference, where in Flink it
   * is three shuffles.
   *
   * The watermark seeding survives the fan. Each sink declares the
   * event-time functions its windows need seeded, `and` concatenates
   * them, and ONE pre-pass computes every column's prefix maximum at
   * once — so a fan whose stages window on different times is still
   * exactly the single-threaded answer, drops included.
   *
   * A fan finishes by merge; `Run.reducers` is 1. See `Sink` for why
   * that is a decision rather than an omission.
   */
  def fan[A, R](flow: Flow[A], sink: Sink[A, R])(using Scheduler): Run[R] ! Async =
    val sh = shape(flow)
    val head = sh.source
    if head == null then
      throw IllegalArgumentException(
        "a fan reads ONE source, and this plan already has a keyed stage in it: put the " +
          "keyed stages in the sinks (Sink.keyed / Sink.tumbling / Sink.sliding) rather " +
          "than in the flow (specs/dataflow.md, stage 3)")
    val src = head.nn
    val n = sh.parts
    val times = sink.times
    val bounds: Vector[Vector[Bounds]] ! Async =
      if times.isEmpty then pure[Async, Vector[Vector[Bounds]]](Vector.fill(n)(Vector.empty))
      else parallel(n)(i => extent(src(i), times)).map(edges)
    bounds.flatMap: bs =>
      parallel(n) { i =>
        val p = sink.start(bs(i))
        Chunks.foldLeft(src(i))(())((_, a) => sink.step(p, a))
        sink.done(p)
        p
      }.map: ps =>
        Run(sink.result(ps), sink.drops(ps), n, 1, sink.merged(ps))

  /** one partition's shape in one event-time column: its greatest and
   * least value, and its own backwardness — how far a value fell
   * below the greatest seen BEFORE it, within this partition */
  private[cluster] final case class Extent(max: Long, min: Long, back: Long)

  /** every column's extent, in ONE pass over the partition */
  private def extent[A](c: Chunks[A], times: Vector[A => Long]): Vector[Extent] =
    val k = times.length
    val hi = Array.fill(k)(Long.MinValue)
    val lo = Array.fill(k)(Long.MaxValue)
    val bk = Array.fill(k)(0L)
    Chunks.foldLeft(c)(())((_, a) =>
      var j = 0
      while j < k do
        val t = times(j)(a)
        if t > hi(j) then hi(j) = t else if hi(j) - t > bk(j) then bk(j) = hi(j) - t
        if t < lo(j) then lo(j) = t
        j += 1)
    (0 until k).toVector.map(j => Extent(hi(j), lo(j), bk(j)))

  /**
   * THE COMPLETENESS BOUNDS (specs/dataflow.md, stage 3's Results).
   *
   * A partition's windowed operator can emit a pane itself — rather
   * than handing an accumulator to the coordinator — when no other
   * partition can touch it. `OkayLane.parallel` writes that rule by
   * hand; this computes it for every partition and every event-time
   * column from the one pre-pass the seeding already needed:
   *
   *   - no EARLIER partition touched a window starting after
   *     `hi(i-1)`, because every earlier event's time is at most that;
   *   - no LATER partition touches a window ending at or before
   *     `hi(i) - back`, because every later event's time is at least
   *     that.
   *
   * `back` is the greatest BACKWARDNESS in the stream. Within a
   * partition it is measured directly; ACROSS partitions an element
   * can fall below the greatest seen in an earlier one, and that is
   * covered by `hi(i-1) - min(i)` — which over-estimates, since it
   * pairs the partition's least value with a maximum that may come
   * long before it. Over-estimating is the safe direction: a larger
   * `back` declares FEWER panes complete, never more.
   *
   * The lower bound is also the watermark seed, so nothing new is
   * computed for it: a partition starts where the stream stood.
   */
  private[cluster] def edges(es: Vector[Vector[Extent]]): Vector[Vector[Bounds]] =
    if es.isEmpty || es.head.isEmpty then es.map(_ => Vector.empty)
    else
      val cols = es.head.indices.toVector.map { j =>
        val maxes = es.map(_(j).max)
        val lower = before(maxes)                       // hi(i-1), and the seed
        val incl = lower.indices.map(i => math.max(lower(i), maxes(i)))
        var back = 0L
        for i <- es.indices do
          val e = es(i)(j)
          if e.back > back then back = e.back
          // an element below the greatest seen in an EARLIER partition
          if lower(i) != Long.MinValue && e.min != Long.MaxValue && lower(i) - e.min > back then
            back = lower(i) - e.min
        lower.indices.toVector.map { i =>
          // an empty partition has nothing complete, and MinValue - back would wrap
          val upper = if incl(i) == Long.MinValue then Long.MinValue else incl(i) - back
          Bounds(lower(i), upper)
        }
      }
      cols.transpose

  /**
   * Every element the plan produces, in partition order.
   *
   * The accumulator ALLOCATES in `init`, and that is not style:
   * `Aggregator.apply(z)(…)` takes its zero by value, so `init`
   * answers one and the same object every time it is asked — which
   * is invisible for an immutable accumulator and catastrophic for a
   * mutable one here, because a stateless plan calls `init` once per
   * PARTITION and hands every fibre the same buffer.
   */
  def collect[A](flow: Flow[A])(using Scheduler): Vector[A] ! Async =
    fold(flow, new Aggregator[A, mutable.ArrayBuffer[A], Vector[A]]:
      def init: mutable.ArrayBuffer[A] = mutable.ArrayBuffer.empty[A]
      def add(b: mutable.ArrayBuffer[A], a: A): mutable.ArrayBuffer[A] = b += a
      def merge(a: mutable.ArrayBuffer[A], b: mutable.ArrayBuffer[A]): mutable.ArrayBuffer[A] = a ++= b
      def present(b: mutable.ArrayBuffer[A]): Vector[A] = b.toVector)

  /** the greatest event time in everything BEFORE each partition: the
   * prefix maximum, which is what a partition's watermark must start
   * from to be the stream's watermark */
  private[cluster] def before(maxes: Vector[Long]): Vector[Long] =
    maxes.scanLeft(Long.MinValue)(math.max).init

  // ---------------------------------------------------------------
  // the compiled plan
  // ---------------------------------------------------------------

  /**
   * A flow, compiled to the two things an executor needs: what one
   * partition computes alone, and what the coordinator does with the
   * pieces. A `Pipe` is a plan that never needs the second — every
   * partition can fold straight into the answer.
   *
   * `andThen` is why there is no type test anywhere below: a chunk
   * transformer pushes THROUGH a shape rather than being matched on
   * it, so a `map` above a window and a `map` below one are the same
   * line of code.
   */
  private sealed abstract class Shape[A]:
    def parts: Int
    /** the per-partition chunks while the plan is still one pass, and
     * null once a keyed stage has been compiled into it — which is
     * how a second keyed stage is refused by name rather than
     * answered wrongly */
    def source: (Int => Chunks[A]) | Null
    def andThen[B](f: Chunks[A] => Chunks[B]): Shape[B]
    def job[Acc](into: Aggregator[A, Acc, ?]): Job[Acc]

  /**
   * What the driver runs: `work` on a fibre per partition, then
   * `reduce` on a fibre per reducer, with `P` — the partial — known
   * only here.
   *
   * `reducers` is asked AFTER the partials are in, which is what lets
   * `Finish.Auto` decide from their size rather than from a guess
   * made before the run.
   */
  private abstract class Job[Acc]:
    type P
    def parts: Int
    def prepass: (Int => Long) | Null
    /** does this plan hand its elements to the terminal in the
     * input's order? true for a stateless plan, false out of a keyed
     * stage — see `ordered` */
    def ordered: Boolean
    def work(i: Int, seed: Long): P
    def reducers(ps: Vector[P]): Int
    def reduce(ps: Vector[P], r: Int, of: Int): Acc
    def drops(ps: Vector[P]): Long

  private final class Pipe[A](val parts: Int, val at: Int => Chunks[A]) extends Shape[A]:
    def source: (Int => Chunks[A]) | Null = at
    def andThen[B](f: Chunks[A] => Chunks[B]): Shape[B] =
      new Pipe(parts, i => f(at(i)))
    def job[Acc](into: Aggregator[A, Acc, ?]): Job[Acc] =
      val self = this
      new Job[Acc]:
        type P = Acc
        def parts: Int = self.parts
        def prepass: (Int => Long) | Null = null
        def ordered: Boolean = true
        def work(i: Int, seed: Long): Acc =
          Chunks.foldLeft(self.at(i))(into.init)((acc, a) => into.add(acc, a))
        def reducers(ps: Vector[Acc]): Int = 1
        def reduce(ps: Vector[Acc], r: Int, of: Int): Acc = ps.reduceLeft(into.merge)
        def drops(ps: Vector[Acc]): Long = 0L

  /**
   * A stage wider than one partition. The map side writes `buckets`
   * hash buckets; a reducer takes a contiguous RANGE of them, which
   * is how the number of reducers can be smaller than the number of
   * buckets and still be a partition of the key space.
   */
  private abstract class Wide[A] extends Shape[A]:
    type P
    def prepass: (Int => Long) | Null
    def buckets: Int
    def work(i: Int, seed: Long): P
    def reducers(ps: Vector[P]): Int
    /** the elements of buckets [lo, hi), merged across the partitions
     * IN INDEX ORDER */
    def out(ps: Vector[P], lo: Int, hi: Int): Chunks[A]
    def drops(ps: Vector[P]): Long

    final def source: (Int => Chunks[A]) | Null = null

    final def andThen[B](f: Chunks[A] => Chunks[B]): Shape[B] =
      val self = this
      new Wide[B]:
        type P = self.P
        def parts: Int = self.parts
        def prepass: (Int => Long) | Null = self.prepass
        def buckets: Int = self.buckets
        def work(i: Int, seed: Long): P = self.work(i, seed)
        def reducers(ps: Vector[P]): Int = self.reducers(ps)
        def out(ps: Vector[P], lo: Int, hi: Int): Chunks[B] = f(self.out(ps, lo, hi))
        def drops(ps: Vector[P]): Long = self.drops(ps)

    final def job[Acc](into: Aggregator[A, Acc, ?]): Job[Acc] =
      val self = this
      new Job[Acc]:
        type P = self.P
        def parts: Int = self.parts
        def prepass: (Int => Long) | Null = self.prepass
        def ordered: Boolean = false
        def work(i: Int, seed: Long): P = self.work(i, seed)
        def reducers(ps: Vector[P]): Int = self.reducers(ps)
        def reduce(ps: Vector[P], r: Int, of: Int): Acc =
          val b = self.buckets
          val lo = (b.toLong * r / of).toInt
          val hi = (b.toLong * (r + 1) / of).toInt
          Chunks.foldLeft(self.out(ps, lo, hi))(into.init)((acc, a) => into.add(acc, a))
        def drops(ps: Vector[P]): Long = self.drops(ps)

  private def shape[A](flow: Flow[A]): Shape[A] = flow match
    case Flow.Src(ps) =>
      require(ps.nonEmpty, "a source has at least one partition")
      new Pipe(ps.length, i => ps(i)())
    case Flow.Local(in, _, f) => shape(in).andThen(f)
    case Flow.Keyed(in, key, agg, finish) => keyed(shape(in), key, agg, finish)
    case Flow.Windowed(in, size, slide, lateness, key, at, agg, seeded, finish) =>
      windowed(shape(in), size, slide, lateness, key, at, agg, seeded, finish)

  private def one[X, A](in: Shape[X], what: String): Int => Chunks[X] =
    val at = in.source
    if at == null then
      throw IllegalArgumentException(
        s"$what follows another keyed stage: two of them need an exchange between " +
          "them, which is stage 2 of specs/dataflow.md")
    at.nn

  /** how many hash buckets the map side writes, and how many reducers
   * may read them — `Auto` buckets as widely as the source is
   * partitioned, so the choice it makes later has somewhere to go */
  private def bucketsFor(finish: Finish, parts: Int): Int = finish match
    case Finish.Merge => 1
    case Finish.Shuffle(r) =>
      require(r > 0, "a shuffle has at least one reducer")
      r
    case Finish.Auto => math.max(1, parts)

  /** which bucket a key belongs to. `h` is an INLINE parameter, so
   * with one bucket the hash is never computed at all and
   * `Finish.Merge` pays nothing for the exchange it is not using */
  private inline def bucketOf(inline h: Int, b: Int): Int =
    if b == 1 then 0 else Math.floorMod(h, b)

  /**
   * THE AUTO RULE, and what it is really choosing.
   *
   * In one process an exchange saves no memory: every partial is
   * already here. What it buys is that the final merge runs on
   * several threads instead of one, and what it costs is the
   * bucketing on the map side plus a fibre per reducer. So the
   * question `Auto` answers is arithmetic — is there enough merging
   * to pay for the hand-off — and the answer is a count of
   * accumulators, MEASURED (MeasureExchange, and the table in
   * specs/dataflow.md's Results) rather than guessed.
   *
   * In a cluster the same switch answers a second, larger question —
   * whether the merged result fits the node doing the merging — and
   * that one is stage 4's to ask. This bound is not it, and must not
   * be quoted as if it were.
   *
   * MEASURED (MeasureExchange, 1M rows over 8 partitions, count per
   * key, minimum of 7 alternating rounds): the merge road wins below
   * 80 000 accumulators (0.67x) and loses above 160 000 (1.33x),
   * rising to 4.8x against it by a million. 100 000 sits in that
   * bracket. Two things move it and neither is guessed at: a FATTER
   * accumulator makes merging dearer and moves the bound DOWN, and
   * fewer partitions leave less for the reducers to win, moving it
   * up. This is a default for a plan that did not choose; a plan that
   * knows its shape should say `Merge` or `Shuffle` and not consult
   * a number measured on someone else's job.
   */
  private[cluster] val autoBound: Long = 100_000L

  private def chosen(finish: Finish, buckets: Int, entries: Long): Int = finish match
    case Finish.Merge => 1
    case Finish.Shuffle(r) => r
    case Finish.Auto => if entries < autoBound then 1 else buckets

  private def keyed[X, K, Acc, O](in: Shape[X], key: X => K,
                                  agg: Aggregator[X, Acc, O],
                                  finish: Finish): Shape[(K, O)] =
    val at = one(in, "a keyed aggregation")
    val n = in.parts
    val b = bucketsFor(finish, n)
    new Wide[(K, O)]:
      type P = Array[mutable.HashMap[K, Acc]]
      def parts: Int = n
      def prepass: (Int => Long) | Null = null
      def buckets: Int = b
      def work(i: Int, seed: Long): P =
        val ms = Array.fill(b)(mutable.HashMap.empty[K, Acc])
        Chunks.foldLeft(at(i))(())((_, x) =>
          val k = key(x)
          val m = ms(bucketOf(k.##, b))
          m.update(k, agg.add(m.getOrElse(k, agg.init), x)))
        ms
      def reducers(ps: Vector[P]): Int =
        chosen(finish, b, entries(ps))
      def out(ps: Vector[P], lo: Int, hi: Int): Chunks[(K, O)] =
        val all = mutable.HashMap.empty[K, Acc]
        var j = lo
        while j < hi do
          for m <- ps do
            for (k, a) <- m(j) do all.update(k, all.get(k).fold(a)(agg.merge(_, a)))
          j += 1
        Chunks.fromIterator(all.iterator.map((k, a) => (k, agg.present(a))))
      def drops(ps: Vector[P]): Long = 0L
      private def entries(ps: Vector[P]): Long =
        var t = 0L
        for ms <- ps do for m <- ms do t += m.size
        t

  /** a partition's share of a windowed stage: every pane it touched,
   * as an ACCUMULATOR (not a presented value — a presented mean
   * cannot be merged with another partition's), and the late
   * elements it dropped */
  private final case class Panes[K, Acc](panes: Array[mutable.HashMap[(Long, K), Acc]], late: Long)

  private def windowed[X, K, Acc, O](in: Shape[X], size: Long, slide: Long, lateness: Long,
                                     key: X => K, at: X => Long,
                                     agg: Aggregator[X, Acc, O],
                                     seeded: Boolean, finish: Finish): Shape[Pane[K, O]] =
    val src = one(in, "a windowed aggregation")
    val n = in.parts
    val b = bucketsFor(finish, n)
    // the same aggregator, presenting its ACCUMULATOR: what a partial
    // pane must carry so another partition's can be merged into it
    val partial = Aggregator[X, Acc, Acc](agg.init)(agg.add)(agg.merge)(identity)
    new Wide[Pane[K, O]]:
      type P = Panes[K, Acc]
      def parts: Int = n
      def buckets: Int = b
      def prepass: (Int => Long) | Null =
        if !seeded then null
        else (i: Int) => Chunks.foldLeft(src(i))(Long.MinValue)((m, x) => math.max(m, at(x)))
      def work(i: Int, seed: Long): P =
        val w = new Windows[K, X, Acc, Acc](size, slide, lateness, key, at, partial)
        if seed != Long.MinValue then w.seed(seed)
        val ms = Array.fill(b)(mutable.HashMap.empty[(Long, K), Acc])
        // bucketed by the (window, key) PAIR, not by the key: the
        // aggregation is per pane, so the pair spreads a hot key's
        // windows over the reducers instead of piling them on one
        val keep: Pane[K, Acc] => Unit = p =>
          val id = (p.start, p.key)
          val m = ms(bucketOf(id.##, b))
          m.update(id, m.get(id).fold(p.value)(agg.merge(_, p.value)))
        Chunks.foldLeft(src(i))(())((_, x) => w.add(x)(keep))
        w.close()(keep)
        Panes(ms, w.dropped)
      def reducers(ps: Vector[P]): Int =
        var t = 0L
        for p <- ps do for m <- p.panes do t += m.size
        chosen(finish, b, t)
      def out(ps: Vector[P], lo: Int, hi: Int): Chunks[Pane[K, O]] =
        val all = mutable.HashMap.empty[(Long, K), Acc]
        var j = lo
        while j < hi do
          for p <- ps do
            for (id, a) <- p.panes(j) do all.update(id, all.get(id).fold(a)(agg.merge(_, a)))
          j += 1
        Chunks.fromIterator(all.iterator.map { case ((start, k), a) =>
          Pane(start, start + size, k, agg.present(a))
        })
      def drops(ps: Vector[P]): Long =
        var d = 0L
        for p <- ps do d += p.late
        d

  // ---------------------------------------------------------------
  // the fibres
  // ---------------------------------------------------------------

  private def parallel[P](n: Int)(w: Int => P)(using Scheduler): Vector[P] ! Async =
    val fibres = (0 until n).toVector.map(i => Async.spawn(async(w(i))))
    gather(fibres, 0, Vector.empty)

  /** join BY INDEX, not by readiness: the partials must reach the
   * coordinator in the input's order (see the class comment) */
  private def gather[P](fs: Vector[Fiber[P]], i: Int, acc: Vector[P]): Vector[P] ! Async =
    if i >= fs.length then pure[Async, Vector[P]](acc)
    else fs(i).joinAsync.flatMap(p => gather(fs, i + 1, acc :+ p))
}
