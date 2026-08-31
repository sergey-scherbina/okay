package okay

import scala.compiletime.summonFrom

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

  /**
   * The push-consumer view (for Stream.fold and friends).
   *
   * NOT final: this is the seam through which the accumulator's
   * specialization travels. Built here it is a generic `Fold`, whose
   * `add(s: S, a: A): S` erases to `(Object, Object)Object`, so an
   * aggregator that knows its accumulator is a `long` overrides this
   * and hands over a `Fold.OfLong` instead. Measured before that
   * override existed: `Aggregator.count` folding 10k Longs took
   * 37.8us where `Fold.count` took 6.9 — 5.5x, for the same
   * arithmetic, purely because the specialization stopped here.
   */
  def fold[In2 <: In]: Fold[In2, Acc] = Fold(init)(add(_, _))

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

  // ------------------------------------------ unboxed accumulators
  //
  // The same three-part story as `Fold`: erasure is fixed at the
  // DECLARATION, so `add(acc: Acc, in: In): Acc` is
  // `(Object, Object)Object` in the generic parent and stays that way
  // in every subtype; a differently-named method declared where the
  // type is already primitive is the only thing that erases unboxed.
  // These exist so the specialization reaches the callers that cannot
  // inline anything — Spark, a java Collector, the cluster — which is
  // where an Aggregator is actually spent.

  /** an aggregator whose accumulator is a `long` */
  trait OfLong[-In, +Out] extends Aggregator[In, Long, Out]:
    def initLong: Long
    def addLong(acc: Long, in: In): Long
    def mergeLong(a: Long, b: Long): Long
    final def init: Long = initLong
    final def add(acc: Long, in: In): Long = addLong(acc, in)
    final def merge(a: Long, b: Long): Long = mergeLong(a, b)
    final override def fold[In2 <: In]: Fold.OfLong[In2] =
      val self = this
      new Fold.OfLong[In2]:
        def initLong: Long = self.initLong
        def addLong(s: Long, a: In2): Long = self.addLong(s, a)

  /** an aggregator whose accumulator is a `double` */
  trait OfDouble[-In, +Out] extends Aggregator[In, Double, Out]:
    def initDouble: Double
    def addDouble(acc: Double, in: In): Double
    def mergeDouble(a: Double, b: Double): Double
    final def init: Double = initDouble
    final def add(acc: Double, in: In): Double = addDouble(acc, in)
    final def merge(a: Double, b: Double): Double = mergeDouble(a, b)
    final override def fold[In2 <: In]: Fold.OfDouble[In2] =
      val self = this
      new Fold.OfDouble[In2]:
        def initDouble: Double = self.initDouble
        def addDouble(s: Double, a: In2): Double = self.addDouble(s, a)

  /** an aggregator whose accumulator is an `int` */
  trait OfInt[-In, +Out] extends Aggregator[In, Int, Out]:
    def initInt: Int
    def addInt(acc: Int, in: In): Int
    def mergeInt(a: Int, b: Int): Int
    final def init: Int = initInt
    final def add(acc: Int, in: In): Int = addInt(acc, in)
    final def merge(a: Int, b: Int): Int = mergeInt(a, b)
    final override def fold[In2 <: In]: Fold.OfInt[In2] =
      val self = this
      new Fold.OfInt[In2]:
        def initInt: Int = self.initInt
        def addInt(s: Int, a: In2): Int = self.addInt(s, a)

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

  /** how many elements — the counter never leaves a register */
  def count[A]: OfLong[A, Long] = new:
    def initLong: Long = 0L
    def addLong(n: Long, a: A): Long = n + 1L
    def mergeLong(a: Long, b: Long): Long = a + b
    def present(acc: Long): Long = acc

  /** the unboxed sums */
  val sumLong: OfLong[Long, Long] = new:
    def initLong: Long = 0L
    def addLong(s: Long, a: Long): Long = s + a
    def mergeLong(a: Long, b: Long): Long = a + b
    def present(acc: Long): Long = acc

  val sumInt: OfInt[Int, Int] = new:
    def initInt: Int = 0
    def addInt(s: Int, a: Int): Int = s + a
    def mergeInt(a: Int, b: Int): Int = a + b
    def present(acc: Int): Int = acc

  val sumDouble: OfDouble[Double, Double] = new:
    def initDouble: Double = 0.0
    def addDouble(s: Double, a: Double): Double = s + a
    def mergeDouble(a: Double, b: Double): Double = a + b
    def present(acc: Double): Double = acc

  /**
   * The sum, unboxed where the type is known — the same selection
   * `Fold.sum` makes, for the same reason. `Numeric` cannot specialize
   * anything (`plus(x: T, y: T): T` erases like every other generic
   * method); it can only SAY which type this is, and the `=:=` that
   * says it also transports the aggregator, with no cast.
   */
  inline def sum[N](using N: Numeric[N]): Aggregator[N, N, N] =
    summonFrom {
      case ev: (N =:= Long) =>
        ev.flip.substituteCo[[X] =>> Aggregator[X, X, X]](sumLong)
      case ev: (N =:= Int) =>
        ev.flip.substituteCo[[X] =>> Aggregator[X, X, X]](sumInt)
      case ev: (N =:= Double) =>
        ev.flip.substituteCo[[X] =>> Aggregator[X, X, X]](sumDouble)
      case _ => apply(N.zero)(N.plus)(N.plus)(identity)
    }

  /**
   * The running mean's accumulator: two primitive fields in ONE
   * object, where `sum zip count` carried a `(N, Long)`.
   *
   * A `Tuple2` is three allocations per element — the tuple, and a box
   * for each field, since a tuple's fields are `Object`. This is one,
   * with the fields laid out as a `double` and a `long`, and in a
   * local fold the JIT can often remove even that. Measured: the
   * tuple form took 87.0us per 10k against 18.9 for a hand loop
   * carrying two locals.
   *
   * It sums in `Double` rather than in `N`. For `mean[Double]` that is
   * exactly what the old form did; for an integral `N` it trades exact
   * summation past 2^53 for immunity to the overflow the old
   * `sum[Int]` accumulator had.
   */
  final case class Mean(sum: Double, count: Long)

  /** the arithmetic mean, one pass, one flat accumulator */
  def mean[N](using N: Numeric[N]): Aggregator[N, Mean, Double] =
    new Aggregator[N, Mean, Double]:
      def init: Mean = Mean(0.0, 0L)
      def add(acc: Mean, in: N): Mean = Mean(acc.sum + N.toDouble(in), acc.count + 1L)
      def merge(a: Mean, b: Mean): Mean = Mean(a.sum + b.sum, a.count + b.count)
      def present(acc: Mean): Double =
        if acc.count == 0 then Double.NaN else acc.sum / acc.count

  /**
   * Population variance in one pass: Welford's step, merged by
   * Chan/Golub/LeVeque — the merge form is what makes it
   * chunk-parallel and distribution-safe.
   */
  /**
   * Welford's three running values, flat: a `long` and two `double`s
   * in one object, where a `(Long, Double, Double)` was four
   * allocations per element — the tuple and a box for each field.
   * Same arithmetic, same merge, same answers.
   */
  final case class Variance(count: Long, mean: Double, m2: Double)

  def variance[N](using N: Numeric[N]): Aggregator[N, Variance, Double] =
    new Aggregator[N, Variance, Double]:
      def init: Variance = Variance(0L, 0.0, 0.0)

      def add(acc: Variance, x: N): Variance =
        val xd = N.toDouble(x)
        val n1 = acc.count + 1
        val d = xd - acc.mean
        val mean1 = acc.mean + d / n1
        Variance(n1, mean1, acc.m2 + d * (xd - mean1))

      def merge(a: Variance, b: Variance): Variance =
        if a.count == 0 then b
        else if b.count == 0 then a
        else
          val n = a.count + b.count
          val d = b.mean - a.mean
          Variance(n, a.mean + d * b.count / n,
            a.m2 + b.m2 + d * d * a.count * b.count / n)

      def present(acc: Variance): Double =
        if acc.count == 0 then Double.NaN else acc.m2 / acc.count

  /** the standard deviation (population) */
  def stddev[N: Numeric]: Aggregator[N, Variance, Double] =
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
