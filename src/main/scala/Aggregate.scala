package okay

/**
 * A named, typed, reusable unit of aggregation (specs/aggregators.md):
 * a start, a step, a MERGE of partial results, and a final
 * presentation. The merge is what a plain Fold lacks — it makes an
 * Aggregator chunk-parallel and distribution-ready: (init, add,
 * merge) is exactly the (zero, seqOp, combOp) triple Spark and Flink
 * aggregation APIs accept. zip composes two aggregators into one that
 * computes both in a SINGLE pass over the data — mean is sum zip
 * count, presented.
 */
trait Aggregator[-In, Acc, +Out] extends Serializable:
  def init: Acc

  /** the sequential step (seqOp) */
  def add(acc: Acc, in: In): Acc

  /** combine two partial results (combOp) — associative */
  def merge(a: Acc, b: Acc): Acc

  /** the final projection */
  def present(acc: Acc): Out

  /** the push-consumer view (for Stream.fold and friends) */
  final def fold[In2 <: In]: Fold[In2, Acc] = Fold(init)(add(_, _))

  /** run over anything iterable */
  final def run(xs: IterableOnce[In]): Out =
    present(xs.iterator.foldLeft(init)(add))

  /** both statistics, one pass */
  final def zip[In2 <: In, Acc2, Out2](that: Aggregator[In2, Acc2, Out2])
  : Aggregator[In2, (Acc, Acc2), (Out, Out2)] =
    val self = this
    new Aggregator[In2, (Acc, Acc2), (Out, Out2)]:
      def init = (self.init, that.init)
      def add(acc: (Acc, Acc2), in: In2) = (self.add(acc._1, in), that.add(acc._2, in))
      def merge(a: (Acc, Acc2), b: (Acc, Acc2)) = (self.merge(a._1, b._1), that.merge(a._2, b._2))
      def present(acc: (Acc, Acc2)) = (self.present(acc._1), that.present(acc._2))

  /** transform the answer */
  final def map[Out2](f: Out => Out2): Aggregator[In, Acc, Out2] =
    val self = this
    new Aggregator[In, Acc, Out2]:
      def init = self.init
      def add(acc: Acc, in: In) = self.add(acc, in)
      def merge(a: Acc, b: Acc) = self.merge(a, b)
      def present(acc: Acc) = f(self.present(acc))

  /** transform the input */
  final def contramap[In2](f: In2 => In): Aggregator[In2, Acc, Out] =
    val self = this
    new Aggregator[In2, Acc, Out]:
      def init = self.init
      def add(acc: Acc, in: In2) = self.add(acc, f(in))
      def merge(a: Acc, b: Acc) = self.merge(a, b)
      def present(acc: Acc) = self.present(acc)

object Aggregator {

  /** make one from the four pieces */
  def apply[In, Acc, Out](z: Acc)(step: (Acc, In) => Acc)(comb: (Acc, Acc) => Acc)
                         (out: Acc => Out): Aggregator[In, Acc, Out] =
    new Aggregator[In, Acc, Out]:
      def init = z
      def add(acc: Acc, in: In) = step(acc, in)
      def merge(a: Acc, b: Acc) = comb(a, b)
      def present(acc: Acc) = out(acc)

  /** every Monoid aggregates on its diagonal */
  def fromMonoid[A](using M: Monoid[A]): Aggregator[A, A, A] =
    apply(M.empty)(M.combine)(M.combine)(identity)

  /** how many elements */
  def count[A]: Aggregator[A, Long, Long] =
    apply(0L)((n, _: A) => n + 1)(_ + _)(identity)

  /** the sum */
  def sum[N](using N: Numeric[N]): Aggregator[N, N, N] =
    apply(N.zero)(N.plus)(N.plus)(identity)

  /** the arithmetic mean: sum zip count, one pass */
  def mean[N](using N: Numeric[N]): Aggregator[N, (N, Long), Double] =
    sum[N].zip(count[N]).map((s, c) => if c == 0 then Double.NaN else N.toDouble(s) / c)

  /**
   * Population variance in one pass: Welford's step, merged by
   * Chan/Golub/LeVeque — the merge form is what makes it
   * chunk-parallel and distribution-safe.
   */
  def variance[N](using N: Numeric[N]): Aggregator[N, (Long, Double, Double), Double] =
    apply[N, (Long, Double, Double), Double]((0L, 0.0, 0.0)) { case ((n, mean, m2), x) =>
      val xd = N.toDouble(x)
      val n1 = n + 1
      val d = xd - mean
      val mean1 = mean + d / n1
      (n1, mean1, m2 + d * (xd - mean1))
    } { case ((n1, mean1, m21), (n2, mean2, m22)) =>
      if n1 == 0 then (n2, mean2, m22)
      else if n2 == 0 then (n1, mean1, m21)
      else
        val n = n1 + n2
        val d = mean2 - mean1
        (n, mean1 + d * n2 / n, m21 + m22 + d * d * n1 * n2 / n)
    } { case (n, _, m2) => if n == 0 then Double.NaN else m2 / n }

  /** the standard deviation (population) */
  def stddev[N: Numeric]: Aggregator[N, (Long, Double, Double), Double] =
    variance[N].map(math.sqrt)

  /** the least element, if any */
  def min[A](using O: Ordering[A]): Aggregator[A, Option[A], Option[A]] =
    apply(Option.empty[A])((s, a: A) => Some(s.fold(a)(O.min(_, a))))(
      (a, b) => (a, b) match
        case (Some(x), Some(y)) => Some(O.min(x, y))
        case _ => a.orElse(b))(identity)

  /** the greatest element, if any */
  def max[A](using O: Ordering[A]): Aggregator[A, Option[A], Option[A]] =
    min[A](using O.reverse)

  /** the first element, if any (merge keeps the left side's) */
  def first[A]: Aggregator[A, Option[A], Option[A]] =
    apply(Option.empty[A])((s, a: A) => s.orElse(Some(a)))((a, b) => a.orElse(b))(identity)

  /** the last element, if any (merge keeps the right side's) */
  def last[A]: Aggregator[A, Option[A], Option[A]] =
    apply(Option.empty[A])((_, a: A) => Some(a))((a, b) => b.orElse(a))(identity)

  /** the k greatest elements, descending */
  def topK[A](k: Int)(using O: Ordering[A]): Aggregator[A, List[A], List[A]] =
    def keep(xs: List[A]) = xs.sorted(using O.reverse).take(k)
    apply(List.empty[A])((s, a: A) => keep(a :: s))((a, b) => keep(a ++ b))(identity)

  /** the distinct elements, exactly (bounded data; sketches for the rest) */
  def distinct[A]: Aggregator[A, Set[A], Long] =
    apply(Set.empty[A])(_ + (_: A))(_ ++ _)(_.size.toLong)

  /** one aggregator per key, in one pass */
  def groupBy[K, In, Acc, Out](key: In => K)(agg: Aggregator[In, Acc, Out])
  : Aggregator[In, Map[K, Acc], Map[K, Out]] =
    apply(Map.empty[K, Acc]) { (m, in: In) =>
      val k = key(in)
      m.updated(k, agg.add(m.getOrElse(k, agg.init), in))
    } { (a, b) =>
      b.foldLeft(a)((m, kv) => m.updated(kv._1, m.get(kv._1).fold(kv._2)(agg.merge(_, kv._2))))
    }(_.view.mapValues(agg.present).toMap)
}

/**
 * The sliding window, on a Group: each emitted value is the combine
 * of the last (up to) n elements — aging data is SUBTRACTED by the
 * inverse, never recomputed. A Monoid-only element type (a running
 * max, a String) is rejected at compile time: there is no un-seeing
 * without an inverse.
 */
def sliding[S[_], F[+_], A](s: S[A])(n: Int)
                           (using G: Group[A], St: Stream[S, F], H: Handler[F]): LazyList[A] =
  def go(q: Vector[A], acc: A, rest: LazyList[A]): LazyList[A] = rest match
    case a #:: t =>
      val grown = G.combine(acc, a)
      if q.length >= n then
        val aged = G.combine(grown, G.inverse(q.head))
        aged #:: go(q.tail :+ a, aged, t)
      else grown #:: go(q :+ a, grown, t)
    case _ => LazyList.empty

  go(Vector.empty, G.empty, s.toLazyList)
