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
                       merged: Long = 0L, retried: Long = 0L)

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
    val bounds: Vector[Bounds] ! Async =
      val pre = job.prepass
      // `Bounds(MinValue, MinValue)` is the no-pre-pass shape and it
      // says both things at once: nothing to seed the watermark with,
      // and nothing may be finished locally — the same sentinel
      // `Sink.windowed` uses (specs/dataflow.md, stage 6a).
      if pre == null then pure[Async, Vector[Bounds]](Vector.fill(n)(Bounds(Long.MinValue, Long.MinValue)))
      else parallel(n)(i => pre.nn(i)).map(es => edges(es.map(e => Vector(e))).map(_.head))
    bounds.flatMap: bs =>
      parallel(n)(i => job.work(i, bs(i))).flatMap: ps =>
        val r = job.reducers(ps)
        parallel(r)(k => job.reduce(ps, k, r)).map: accs =>
          Run(into.present(accs.reduceLeft(into.merge)), job.drops(ps), n, r, job.merged(ps))

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
    fanWith(flow, sink)(identity)

  /** the fan, with what happens to a partial between the partition
   * and the coordinator made explicit — `identity` in one process, a
   * codec round trip in `fanWired`, a socket in stage 4b */
  private def fanWith[A, R](flow: Flow[A], sink: Sink[A, R])
                           (handOver: sink.W => sink.W)(using Scheduler): Run[R] ! Async =
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
        handOver(sink.finish(p))
      }.map: ws =>
        val out = Run(sink.result(ws), sink.drops(ws), n, 1, sink.merged(ws))
        // A BATCH RUN IS ONE EPOCH, and it is over: a staging writer
        // has to hear that or it would never move anything in
        // (specs/dataflow.md, stage 9)
        sink.committed(1)
        out

  /**
   * THE SAME RUN, WITH EVERY PARTIAL FORCED THROUGH ITS CODEC
   * (specs/dataflow.md, stage 4).
   *
   * This is what a distributed run does between a worker and the
   * coordinator, performed here in one process: `finish` produces a
   * `W`, the `W` is encoded, the bytes are decoded, and only then is
   * anything merged. A wire that changes the answer — a Schema that
   * loses a field, an accumulator that does not survive a round trip
   * — is caught by an ordinary test in milliseconds rather than
   * across four processes.
   *
   * It is not a simulation of the network. Nothing here is delayed,
   * dropped or reordered; that is stage 5's business. What it pins is
   * the one thing sockets cannot fix: whether the partial is a value.
   */
  def fanWired[A, R](flow: Flow[A], sink: Wire[A, R])(using Scheduler): Run[R] ! Async =
    val codec = okay.codec.Codecs.cbor(sink.wire)
    fanWith(flow, sink) { w =>
      codec.decode(codec.encode(w)) match
        case Right(back) => back
        case Left(why) => throw IllegalStateException(s"a partial did not survive its codec: $why")
    }

  /**
   * One partition's shape in one event-time column: its greatest and
   * least value, and its own backwardness — how far a value fell
   * below the greatest seen BEFORE it, within this partition.
   *
   * PUBLIC because it crosses a wire. It is what a worker answers to
   * the pre-pass request, and the coordinator turns every
   * partition's extents into the `Bounds` each of them then runs
   * under (`edges`). Three longs per event-time column is the whole
   * of what the coordinator needs to know before any partition may
   * start — see stage 4b.
   */
  final case class Extent(max: Long, min: Long, back: Long)

  /** one partition of a plan, as the chunks a worker will read.
   * Refuses the same way `fan` does when the plan already has a keyed
   * stage in it, since a partition of that is not a thing */
  private[cluster] def partition[A](flow: Flow[A], i: Int): Chunks[A] =
    val sh = shape(flow)
    val head = sh.source
    if head == null then
      throw IllegalArgumentException(
        "a distributed job reads ONE source: put the keyed stages in the sinks " +
          "(specs/dataflow.md, stage 3)")
    head.nn(i)

  /** every column's extent, in ONE pass over the partition */
  private[cluster] def extent[A](c: Chunks[A], times: Vector[A => Long]): Vector[Extent] =
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
        val last = lower.length - 1
        lower.indices.toVector.map { i =>
          // THE LAST PARTITION HAS NO UPPER BOUND, and that is not an
          // optimisation bolted on: the two bounds guard two
          // different neighbours. `lower` says no EARLIER partition
          // can contribute (the pane starts after everything they
          // saw); `upper` says no LATER one can. For the last
          // partition there is no later one, so its only limit is the
          // first — which is what makes a run at ONE partition merge
          // nothing at all, as it should, instead of holding back the
          // final `back` of the stream (dataflow-run-complete-panes).
          val upper =
            if i == last then Long.MaxValue
            // an empty partition has nothing complete, and MinValue - back would wrap
            else if incl(i) == Long.MinValue then Long.MinValue
            else incl(i) - back
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
    /**
     * WHAT ONE PARTITION'S EVENT TIMES LOOK LIKE, when the plan has
     * any — the same `Extent` the fan's pre-pass computes, and for
     * the same two reasons: the prefix maximum is a partition's
     * watermark SEED, and the maximum with the backwardness is what
     * decides which panes a partition may finish ALONE
     * (dataflow-run-complete-panes). Until that lane it answered only
     * the maximum, and the single-stage road could seed but not
     * finish.
     */
    def prepass: (Int => Extent) | Null
    /** does this plan hand its elements to the terminal in the
     * input's order? true for a stateless plan, false out of a keyed
     * stage — see `ordered` */
    def ordered: Boolean
    def work(i: Int, bounds: Bounds): P
    def reducers(ps: Vector[P]): Int
    def reduce(ps: Vector[P], r: Int, of: Int): Acc
    def drops(ps: Vector[P]): Long
    /** how many accumulators reach the coordinator's merge — the work
     * the completeness rule exists to remove, reported so a suite can
     * assert it rather than a benchmark merely notice it */
    def merged(ps: Vector[P]): Long

  private final class Pipe[A](val parts: Int, val at: Int => Chunks[A]) extends Shape[A]:
    def source: (Int => Chunks[A]) | Null = at
    def andThen[B](f: Chunks[A] => Chunks[B]): Shape[B] =
      new Pipe(parts, i => f(at(i)))
    def job[Acc](into: Aggregator[A, Acc, ?]): Job[Acc] =
      val self = this
      new Job[Acc]:
        type P = Acc
        def parts: Int = self.parts
        def prepass: (Int => Extent) | Null = null
        def ordered: Boolean = true
        def work(i: Int, bounds: Bounds): Acc =
          Chunks.foldLeft(self.at(i))(into.init)((acc, a) => into.add(acc, a))
        def reducers(ps: Vector[Acc]): Int = 1
        def reduce(ps: Vector[Acc], r: Int, of: Int): Acc = ps.reduceLeft(into.merge)
        def drops(ps: Vector[Acc]): Long = 0L
        // one accumulator per partition, and nothing keyed
        def merged(ps: Vector[Acc]): Long = ps.length.toLong

  /**
   * A stage wider than one partition. The map side writes `buckets`
   * hash buckets; a reducer takes a contiguous RANGE of them, which
   * is how the number of reducers can be smaller than the number of
   * buckets and still be a partition of the key space.
   */
  private abstract class Wide[A] extends Shape[A]:
    type P
    def prepass: (Int => Extent) | Null
    def buckets: Int
    def work(i: Int, bounds: Bounds): P
    def reducers(ps: Vector[P]): Int
    /** the elements of buckets [lo, hi), merged across the partitions
     * IN INDEX ORDER */
    def out(ps: Vector[P], lo: Int, hi: Int): Chunks[A]
    def drops(ps: Vector[P]): Long
    def merged(ps: Vector[P]): Long

    final def source: (Int => Chunks[A]) | Null = null

    final def andThen[B](f: Chunks[A] => Chunks[B]): Shape[B] =
      val self = this
      new Wide[B]:
        type P = self.P
        def parts: Int = self.parts
        def prepass: (Int => Extent) | Null = self.prepass
        def buckets: Int = self.buckets
        def work(i: Int, bounds: Bounds): P = self.work(i, bounds)
        def reducers(ps: Vector[P]): Int = self.reducers(ps)
        def out(ps: Vector[P], lo: Int, hi: Int): Chunks[B] = f(self.out(ps, lo, hi))
        def drops(ps: Vector[P]): Long = self.drops(ps)
        def merged(ps: Vector[P]): Long = self.merged(ps)

    final def job[Acc](into: Aggregator[A, Acc, ?]): Job[Acc] =
      val self = this
      new Job[Acc]:
        type P = self.P
        def parts: Int = self.parts
        def prepass: (Int => Extent) | Null = self.prepass
        def ordered: Boolean = false
        def work(i: Int, bounds: Bounds): P = self.work(i, bounds)
        def reducers(ps: Vector[P]): Int = self.reducers(ps)
        def reduce(ps: Vector[P], r: Int, of: Int): Acc =
          val b = self.buckets
          val lo = (b.toLong * r / of).toInt
          val hi = (b.toLong * (r + 1) / of).toInt
          Chunks.foldLeft(self.out(ps, lo, hi))(into.init)((acc, a) => into.add(acc, a))
        def drops(ps: Vector[P]): Long = self.drops(ps)
        def merged(ps: Vector[P]): Long = self.merged(ps)

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
      def prepass: (Int => Extent) | Null = null
      def buckets: Int = b
      def work(i: Int, bounds: Bounds): P =
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
      def merged(ps: Vector[P]): Long = entries(ps)
      private def entries(ps: Vector[P]): Long =
        var t = 0L
        for ms <- ps do for m <- ms do t += m.size
        t

  /** a partition's share of a windowed stage: every pane it touched,
   * as an ACCUMULATOR (not a presented value — a presented mean
   * cannot be merged with another partition's), and the late
   * elements it dropped */
  /**
   * What a windowed partition hands the coordinator, and the shape is
   * the completeness rule made visible (dataflow-run-complete-panes).
   *
   * `panes` are the BOUNDARY ones — the handful another partition can
   * still contribute to — bucketed by (window, key) so a reducer owns
   * a share of them. `done` are the ones this partition finished
   * alone: already PRESENTED, so nothing merges them and nothing
   * hashes them again; a reducer concatenates its range.
   *
   * The alternative was to thread the terminal aggregator down here
   * and fold a finished pane into it on the spot. That would have
   * saved holding them, at the price of the node knowing what it is
   * folded into — and `job(into)` is built after the node exists, so
   * it would have meant restructuring the plan to save memory the
   * measurement says is not the problem.
   */
  private final case class Panes[K, Acc, O](panes: Array[mutable.HashMap[(Long, K), Acc]],
                                            done: Array[mutable.ArrayBuffer[Pane[K, O]]],
                                            late: Long)

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
      type P = Panes[K, Acc, O]
      def parts: Int = n
      def buckets: Int = b
      // the whole extent, not just the maximum: the prefix maximum
      // seeds the watermark and the maximum with the backwardness is
      // what lets a partition finish a pane alone
      def prepass: (Int => Extent) | Null =
        if !seeded then null
        else (i: Int) => extent(src(i), Vector(at)).head
      def work(i: Int, bounds: Bounds): P =
        val w = new Windows[K, X, Acc, Acc](size, slide, lateness, key, at, partial)
        if bounds.lower != Long.MinValue then w.seed(bounds.lower)
        val ms = Array.fill(b)(mutable.HashMap.empty[(Long, K), Acc])
        val done = Array.fill(b)(mutable.ArrayBuffer.empty[Pane[K, O]])
        // THE COMPLETENESS RULE, on the single-stage road at last. A
        // pane that starts after everything the earlier partitions
        // saw and ends before anything later can still go back to is
        // this partition's alone: it is presented here and never
        // merged. Otherwise it is a boundary pane and goes into the
        // bucketed map — bucketed by the (window, key) PAIR, not the
        // key, so the pair spreads a hot key's windows over the
        // reducers instead of piling them on one.
        val keep: Pane[K, Acc] => Unit = p =>
          val id = (p.start, p.key)
          if p.start > bounds.lower && p.start + size <= bounds.upper then
            done(bucketOf(id.##, b)) += Pane(p.start, p.start + size, p.key, agg.present(p.value))
          else
            val m = ms(bucketOf(id.##, b))
            m.update(id, m.get(id).fold(p.value)(agg.merge(_, p.value)))
        Chunks.foldLeft(src(i))(())((_, x) => w.add(x)(keep))
        w.close()(keep)
        Panes(ms, done, w.dropped)
      def reducers(ps: Vector[P]): Int =
        var t = 0L
        for p <- ps do for m <- p.panes do t += m.size
        chosen(finish, b, t)
      def out(ps: Vector[P], lo: Int, hi: Int): Chunks[Pane[K, O]] =
        val all = mutable.HashMap.empty[(Long, K), Acc]
        val finished = Vector.newBuilder[Iterator[Pane[K, O]]]
        var j = lo
        while j < hi do
          for p <- ps do
            for (id, a) <- p.panes(j) do all.update(id, all.get(id).fold(a)(agg.merge(_, a)))
            finished += p.done(j).iterator
          j += 1
        val merged = all.iterator.map { case ((start, k), a) =>
          Pane(start, start + size, k, agg.present(a))
        }
        // the panes nobody had to merge come out beside the ones that
        // were merged; a windowed plan's output is a hash map's order
        // either way (`ordered` is false), so nothing depends on which
        Chunks.fromIterator(merged ++ finished.result().iterator.flatten)
      def drops(ps: Vector[P]): Long =
        var d = 0L
        for p <- ps do d += p.late
        d
      // the BOUNDARY panes only: what a partition finished alone is
      // already presented and never reaches a merge
      def merged(ps: Vector[P]): Long =
        var t = 0L
        for p <- ps do for m <- p.panes do t += m.size
        t

  // ---------------------------------------------------------------
  // the fibres
  // ---------------------------------------------------------------

  /** the same fan-out the local driver uses, for the coordinator:
   * one fibre per partition, joined BY INDEX */
  private[cluster] def spread[P](n: Int)(w: Int => P)(using Scheduler): Vector[P] ! Async =
    parallel(n)(w)

  private def parallel[P](n: Int)(w: Int => P)(using Scheduler): Vector[P] ! Async =
    val fibres = (0 until n).toVector.map(i => Async.spawn(async(w(i))))
    gather(fibres, 0, Vector.empty)

  /** join BY INDEX, not by readiness: the partials must reach the
   * coordinator in the input's order (see the class comment) */
  private def gather[P](fs: Vector[Fiber[P]], i: Int, acc: Vector[P]): Vector[P] ! Async =
    if i >= fs.length then pure[Async, Vector[P]](acc)
    else fs(i).joinAsync.flatMap(p => gather(fs, i + 1, acc :+ p))
}
