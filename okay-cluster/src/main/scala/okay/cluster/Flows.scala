package okay.cluster

import okay.*
import okay.given
import scala.collection.mutable

/** what a run answers: the value, and what the engine had to say
 * about it — late elements are DROPPED by an event-time window, and
 * an engine that does not report how many is hiding a wrong answer */
final case class Run[O](value: O, dropped: Long, partitions: Int)

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
    val seeds: Vector[Long] ! Async =
      val pre = job.prepass
      if pre == null then pure[Async, Vector[Long]](Vector.fill(n)(Long.MinValue))
      else parallel(n)(i => pre.nn(i)).map(before)
    seeds.flatMap: sd =>
      parallel(n)(i => job.work(i, sd(i))).map: ps =>
        Run(into.present(job.combine(ps)), job.drops(ps), n)

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

  /** what the driver runs: `work` on a fibre per partition, `combine`
   * at the coordinator, and `P` — the partial — known only here */
  private abstract class Job[Acc]:
    type P
    def parts: Int
    def prepass: (Int => Long) | Null
    def work(i: Int, seed: Long): P
    def combine(ps: Vector[P]): Acc
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
        def work(i: Int, seed: Long): Acc =
          Chunks.foldLeft(self.at(i))(into.init)((acc, a) => into.add(acc, a))
        def combine(ps: Vector[Acc]): Acc = ps.reduceLeft(into.merge)
        def drops(ps: Vector[Acc]): Long = 0L

  private abstract class Wide[A] extends Shape[A]:
    type P
    def prepass: (Int => Long) | Null
    def work(i: Int, seed: Long): P
    def out(ps: Vector[P]): Chunks[A]
    def drops(ps: Vector[P]): Long

    final def source: (Int => Chunks[A]) | Null = null

    final def andThen[B](f: Chunks[A] => Chunks[B]): Shape[B] =
      val self = this
      new Wide[B]:
        type P = self.P
        def parts: Int = self.parts
        def prepass: (Int => Long) | Null = self.prepass
        def work(i: Int, seed: Long): P = self.work(i, seed)
        def out(ps: Vector[P]): Chunks[B] = f(self.out(ps))
        def drops(ps: Vector[P]): Long = self.drops(ps)

    final def job[Acc](into: Aggregator[A, Acc, ?]): Job[Acc] =
      val self = this
      new Job[Acc]:
        type P = self.P
        def parts: Int = self.parts
        def prepass: (Int => Long) | Null = self.prepass
        def work(i: Int, seed: Long): P = self.work(i, seed)
        def combine(ps: Vector[P]): Acc =
          Chunks.foldLeft(self.out(ps))(into.init)((acc, a) => into.add(acc, a))
        def drops(ps: Vector[P]): Long = self.drops(ps)

  private def shape[A](flow: Flow[A]): Shape[A] = flow match
    case Flow.Src(ps) =>
      require(ps.nonEmpty, "a source has at least one partition")
      new Pipe(ps.length, i => ps(i)())
    case Flow.Local(in, _, f) => shape(in).andThen(f)
    case Flow.Keyed(in, key, agg) => keyed(shape(in), key, agg)
    case Flow.Windowed(in, size, slide, lateness, key, at, agg, seeded) =>
      windowed(shape(in), size, slide, lateness, key, at, agg, seeded)

  private def one[X, A](in: Shape[X], what: String): Int => Chunks[X] =
    val at = in.source
    if at == null then
      throw IllegalArgumentException(
        s"$what follows another keyed stage: two of them need an exchange between " +
          "them, which is stage 2 of specs/dataflow.md")
    at.nn

  private def keyed[X, K, Acc, O](in: Shape[X], key: X => K,
                                  agg: Aggregator[X, Acc, O]): Shape[(K, O)] =
    val at = one(in, "a keyed aggregation")
    val n = in.parts
    new Wide[(K, O)]:
      type P = mutable.HashMap[K, Acc]
      def parts: Int = n
      def prepass: (Int => Long) | Null = null
      def work(i: Int, seed: Long): P =
        val m = mutable.HashMap.empty[K, Acc]
        Chunks.foldLeft(at(i))(())((_, x) =>
          val k = key(x)
          m.update(k, agg.add(m.getOrElse(k, agg.init), x)))
        m
      def out(ps: Vector[P]): Chunks[(K, O)] =
        val all = mutable.HashMap.empty[K, Acc]
        for m <- ps do
          for (k, a) <- m do all.update(k, all.get(k).fold(a)(agg.merge(_, a)))
        Chunks.fromIterator(all.iterator.map((k, a) => (k, agg.present(a))))
      def drops(ps: Vector[P]): Long = 0L

  /** a partition's share of a windowed stage: every pane it touched,
   * as an ACCUMULATOR (not a presented value — a presented mean
   * cannot be merged with another partition's), and the late
   * elements it dropped */
  private final case class Panes[K, Acc](panes: mutable.HashMap[(Long, K), Acc], late: Long)

  private def windowed[X, K, Acc, O](in: Shape[X], size: Long, slide: Long, lateness: Long,
                                     key: X => K, at: X => Long,
                                     agg: Aggregator[X, Acc, O],
                                     seeded: Boolean): Shape[Pane[K, O]] =
    val src = one(in, "a windowed aggregation")
    val n = in.parts
    // the same aggregator, presenting its ACCUMULATOR: what a partial
    // pane must carry so another partition's can be merged into it
    val partial = Aggregator[X, Acc, Acc](agg.init)(agg.add)(agg.merge)(identity)
    new Wide[Pane[K, O]]:
      type P = Panes[K, Acc]
      def parts: Int = n
      def prepass: (Int => Long) | Null =
        if !seeded then null
        else (i: Int) => Chunks.foldLeft(src(i))(Long.MinValue)((m, x) => math.max(m, at(x)))
      def work(i: Int, seed: Long): P =
        val w = new Windows[K, X, Acc, Acc](size, slide, lateness, key, at, partial)
        if seed != Long.MinValue then w.seed(seed)
        val m = mutable.HashMap.empty[(Long, K), Acc]
        val keep: Pane[K, Acc] => Unit = p =>
          val id = (p.start, p.key)
          m.update(id, m.get(id).fold(p.value)(agg.merge(_, p.value)))
        Chunks.foldLeft(src(i))(())((_, x) => w.add(x)(keep))
        w.close()(keep)
        Panes(m, w.dropped)
      def out(ps: Vector[P]): Chunks[Pane[K, O]] =
        val all = mutable.HashMap.empty[(Long, K), Acc]
        for p <- ps do
          for (id, a) <- p.panes do all.update(id, all.get(id).fold(a)(agg.merge(_, a)))
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
