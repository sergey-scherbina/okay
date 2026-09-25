package okay2
import scala.annotation.tailrec


/**
 * A named, typed, reusable unit of aggregation — the Scala 3 core's
 * Aggregate.scala: a start, a step, a MERGE of partial results, and a
 * final presentation. The merge is what a plain Fold lacks — it makes an
 * Aggregator chunk-parallel and distribution-ready: (init, add, merge)
 * is exactly the (zero, seqOp, combOp) triple Spark and Flink
 * aggregation APIs accept. `zip` composes two aggregators into one that
 * computes both in a SINGLE pass over the data.
 */
trait Aggregator[-In, Acc, +Out] extends Serializable {
  def init: Acc

  /** the sequential step (seqOp) */
  def add(acc: Acc, in: In): Acc

  /** combine two partial results (combOp) — associative */
  def merge(a: Acc, b: Acc): Acc

  /** the final projection */
  def present(acc: Acc): Out

  /** the push-consumer view (for Stream.fold and friends). NOT final:
   * an aggregator that knows its accumulator is a primitive overrides
   * it with itself, so the specialization reaches the fold loop */
  def fold[In2 <: In]: Fold[In2, Acc] = Fold[In2, Acc](init)((s, a) => add(s, a))

  /** run over anything iterable */
  final def run(xs: IterableOnce[In]): Out = present(xs.iterator.foldLeft(init)(add))

  /** both statistics, one pass */
  final def zip[In2 <: In, Acc2, Out2](that: Aggregator[In2, Acc2, Out2]): Aggregator[In2, (Acc, Acc2), (Out, Out2)] = {
    val self = this
    new Aggregator[In2, (Acc, Acc2), (Out, Out2)] {
      def init: (Acc, Acc2) = (self.init, that.init)
      def add(acc: (Acc, Acc2), in: In2): (Acc, Acc2) = (self.add(acc._1, in), that.add(acc._2, in))
      def merge(a: (Acc, Acc2), b: (Acc, Acc2)): (Acc, Acc2) = (self.merge(a._1, b._1), that.merge(a._2, b._2))
      def present(acc: (Acc, Acc2)): (Out, Out2) = (self.present(acc._1), that.present(acc._2))
    }
  }

  /** transform the answer */
  final def map[Out2](f: Out => Out2): Aggregator[In, Acc, Out2] = {
    val self = this
    new Aggregator[In, Acc, Out2] {
      def init: Acc = self.init
      def add(acc: Acc, in: In): Acc = self.add(acc, in)
      def merge(a: Acc, b: Acc): Acc = self.merge(a, b)
      def present(acc: Acc): Out2 = f(self.present(acc))
    }
  }

  /** transform the input */
  final def contramap[In2](f: In2 => In): Aggregator[In2, Acc, Out] = {
    val self = this
    new Aggregator[In2, Acc, Out] {
      def init: Acc = self.init
      def add(acc: Acc, in: In2): Acc = self.add(acc, f(in))
      def merge(a: Acc, b: Acc): Acc = self.merge(a, b)
      def present(acc: Acc): Out = self.present(acc)
    }
  }
}

/**
 * An aggregation whose `merge` is associative but NOT commutative:
 * `merge(a, b)` reads "the slice summarised by a, and THEN the slice
 * summarised by b", so a merge tree over it must respect the input's
 * order — what a distributed finish must refuse to reorder.
 */
trait Sequential[-In, Acc, +Out] extends Aggregator[In, Acc, Out]

object Aggregator {

  // ------------------------------------------ unboxed accumulators
  //
  // The core's three: a differently-named method declared where the
  // type is already primitive is the only thing that erases unboxed,
  // and an aggregator of that shape IS its own `Fold.OfX`, so the
  // accumulator stays a primitive through `Stream.fold`'s dispatch.

  /** an aggregator whose accumulator is a `long` */
  trait OfLong[-In, +Out] extends Aggregator[In, Long, Out] with Fold.OfLong[In] { self =>
    def mergeLong(a: Long, b: Long): Long
    final def merge(a: Long, b: Long): Long = mergeLong(a, b)
    /** it IS its own fold */
    final override def fold[In2 <: In]: Fold.OfLong[In2] = this

    /** `zip`, with the specialization kept: one `Longs2` per `add` where
     * the generic form allocates a tuple and two boxes */
    final def zipLong[In2 <: In, Out2](that: OfLong[In2, Out2]): Aggregator[In2, Longs2, (Out, Out2)] =
      new Aggregator[In2, Longs2, (Out, Out2)] {
        def init: Longs2 = Longs2(self.initLong, that.initLong)
        def add(acc: Longs2, in: In2): Longs2 = Longs2(self.addLong(acc.a, in), that.addLong(acc.b, in))
        def merge(x: Longs2, y: Longs2): Longs2 = Longs2(self.mergeLong(x.a, y.a), that.mergeLong(x.b, y.b))
        def present(acc: Longs2): (Out, Out2) = (self.present(acc.a), that.present(acc.b))
      }
  }

  /** two long-accumulated statistics in one flat object */
  final case class Longs2(a: Long, b: Long)

  /** an aggregator whose accumulator is a `double` */
  trait OfDouble[-In, +Out] extends Aggregator[In, Double, Out] with Fold.OfDouble[In] {
    def mergeDouble(a: Double, b: Double): Double
    final def merge(a: Double, b: Double): Double = mergeDouble(a, b)
    final override def fold[In2 <: In]: Fold.OfDouble[In2] = this
  }

  /** an aggregator whose accumulator is an `int` */
  trait OfInt[-In, +Out] extends Aggregator[In, Int, Out] with Fold.OfInt[In] {
    def mergeInt(a: Int, b: Int): Int
    final def merge(a: Int, b: Int): Int = mergeInt(a, b)
    final override def fold[In2 <: In]: Fold.OfInt[In2] = this
  }

  /** make one from the four pieces. `z` is taken BY VALUE: right for
   * an immutable accumulator, wrong for a mutable one wherever `init`
   * is asked more than once — write those as an explicit Aggregator */
  def apply[In, Acc, Out](z: Acc)(step: (Acc, In) => Acc)(combine: (Acc, Acc) => Acc)(finish: Acc => Out): Aggregator[In, Acc, Out] =
    new Aggregator[In, Acc, Out] {
      def init: Acc = z
      def add(acc: Acc, in: In): Acc = step(acc, in)
      def merge(a: Acc, b: Acc): Acc = combine(a, b)
      def present(acc: Acc): Out = finish(acc)
    }

  /** every Monoid aggregates on its diagonal */
  def fromMonoid[A](implicit M: Monoid[A]): Aggregator[A, A, A] =
    apply[A, A, A](M.empty)(M.combine)(M.combine)(identity)

  /** how many elements — the counter never leaves a register */
  def count[A]: OfLong[A, Long] = new OfLong[A, Long] {
    def initLong: Long = 0L
    def addLong(n: Long, a: A): Long = n + 1L
    def mergeLong(a: Long, b: Long): Long = a + b
    def present(acc: Long): Long = acc
  }

  /** the unboxed sums */
  val sumLong: OfLong[Long, Long] = new OfLong[Long, Long] {
    def initLong: Long = 0L
    def addLong(s: Long, a: Long): Long = s + a
    def mergeLong(a: Long, b: Long): Long = a + b
    def present(acc: Long): Long = acc
  }

  val sumInt: OfInt[Int, Int] = new OfInt[Int, Int] {
    def initInt: Int = 0
    def addInt(s: Int, a: Int): Int = s + a
    def mergeInt(a: Int, b: Int): Int = a + b
    def present(acc: Int): Int = acc
  }

  val sumDouble: OfDouble[Double, Double] = new OfDouble[Double, Double] {
    def initDouble: Double = 0.0
    def addDouble(s: Double, a: Double): Double = s + a
    def mergeDouble(a: Double, b: Double): Double = a + b
    def present(acc: Double): Double = acc
  }

  /**
   * The specialization `sum[N]` answers with, VISIBLE in its type — the
   * Scala 3 core's match type `SumOf[N]`, spelled as Scala 2 spells a
   * type-level function: an implicit carrying its answer as a member.
   * `sum[Long]` is an `OfLong`, so `.zipLong` is reachable from the
   * idiomatic spelling; any other `Numeric` falls to the generic sum.
   * No cast: each instance is typed at what it is.
   */
  trait SumOf[N] {
    type Out <: Aggregator[N, _, N]
    def aggregator: Out
  }

  object SumOf extends LowSumOf {
    type Aux[N, O] = SumOf[N] { type Out = O }
    private def of[N, O <: Aggregator[N, _, N]](a: O): Aux[N, O] = new SumOf[N] {
      type Out = O
      def aggregator: O = a
    }
    implicit val long: Aux[Long, OfLong[Long, Long]] = of[Long, OfLong[Long, Long]](sumLong)
    implicit val int: Aux[Int, OfInt[Int, Int]] = of[Int, OfInt[Int, Int]](sumInt)
    implicit val double: Aux[Double, OfDouble[Double, Double]] = of[Double, OfDouble[Double, Double]](sumDouble)
  }

  trait LowSumOf {
    implicit def numeric[N](implicit N: Numeric[N]): SumOf.Aux[N, Aggregator[N, N, N]] = new SumOf[N] {
      type Out = Aggregator[N, N, N]
      def aggregator: Aggregator[N, N, N] = apply[N, N, N](N.zero)(N.plus)(N.plus)(identity)
    }
  }

  /** the sum, unboxed where the type is known */
  def sum[N](implicit S: SumOf[N]): S.Out = S.aggregator

  /** the running mean's accumulator: a `double` and a `long` in ONE
   * object, where `sum zip count` carried a tuple and two boxes */
  final case class Mean(sum: Double, count: Long)

  /** the arithmetic mean, one pass, one flat accumulator */
  def mean[N](implicit N: Numeric[N]): Aggregator[N, Mean, Double] =
    new Aggregator[N, Mean, Double] {
      def init: Mean = Mean(0.0, 0L)
      def add(acc: Mean, in: N): Mean = Mean(acc.sum + N.toDouble(in), acc.count + 1L)
      def merge(a: Mean, b: Mean): Mean = Mean(a.sum + b.sum, a.count + b.count)
      def present(acc: Mean): Double = if (acc.count == 0) Double.NaN else acc.sum / acc.count
    }

  /** Welford's three running values, flat */
  final case class Variance(count: Long, mean: Double, m2: Double)

  /** population variance in one pass: Welford's step, merged by
   * Chan/Golub/LeVeque — the merge form is what makes it
   * chunk-parallel and distribution-safe */
  def variance[N](implicit N: Numeric[N]): Aggregator[N, Variance, Double] =
    new Aggregator[N, Variance, Double] {
      def init: Variance = Variance(0L, 0.0, 0.0)

      def add(acc: Variance, x: N): Variance = {
        val xd = N.toDouble(x)
        val n1 = acc.count + 1
        val d = xd - acc.mean
        val mean1 = acc.mean + d / n1
        Variance(n1, mean1, acc.m2 + d * (xd - mean1))
      }

      def merge(a: Variance, b: Variance): Variance =
        if (a.count == 0) b
        else if (b.count == 0) a
        else {
          val n = a.count + b.count
          val d = b.mean - a.mean
          Variance(n, a.mean + d * b.count / n, a.m2 + b.m2 + d * d * a.count * b.count / n)
        }

      def present(acc: Variance): Double = if (acc.count == 0) Double.NaN else acc.m2 / acc.count
    }

  /** the standard deviation (population) */
  def stddev[N: Numeric]: Aggregator[N, Variance, Double] = variance[N].map(math.sqrt)

  /**
   * Count, sum, min and max of one measure in ONE flat accumulator of
   * four `long`s, where `count zip sum zip min zip max` allocates six
   * objects per element. `min`/`max` are the sentinels
   * `Long.MaxValue`/`Long.MinValue` on an empty summary; `count == 0`
   * is the test for "nothing was seen".
   */
  final case class Summary(count: Long, sum: Long, min: Long, max: Long) {
    /** the arithmetic mean, or NaN when nothing was summarised */
    def mean: Double = if (count == 0L) Double.NaN else sum.toDouble / count.toDouble
  }

  object Summary {
    val empty: Summary = Summary(0L, 0L, Long.MaxValue, Long.MinValue)
  }

  /** the four statistics of a long-valued measure, one pass */
  def summary[A](measure: A => Long): Aggregator[A, Summary, Summary] =
    new Aggregator[A, Summary, Summary] {
      def init: Summary = Summary.empty
      def add(acc: Summary, in: A): Summary = {
        val x = measure(in)
        Summary(acc.count + 1L, acc.sum + x,
          if (x < acc.min) x else acc.min,
          if (x > acc.max) x else acc.max)
      }
      def merge(a: Summary, b: Summary): Summary =
        Summary(a.count + b.count, a.sum + b.sum,
          if (a.min < b.min) a.min else b.min,
          if (a.max > b.max) a.max else b.max)
      def present(acc: Summary): Summary = acc
    }

  /** the least element, if any */
  def min[A](implicit O: Ordering[A]): Aggregator[A, Option[A], Option[A]] =
    apply[A, Option[A], Option[A]](Option.empty[A])((s, a) => Some(s.fold(a)(O.min(_, a))))(
      (a, b) => (a, b) match {
        case (Some(x), Some(y)) => Some(O.min(x, y))
        case _ => a.orElse(b)
      })(identity)

  /** the greatest element, if any */
  def max[A](implicit O: Ordering[A]): Aggregator[A, Option[A], Option[A]] = min[A](O.reverse)

  /** the first element, if any (merge keeps the left side's) */
  def first[A]: Aggregator[A, Option[A], Option[A]] =
    apply[A, Option[A], Option[A]](Option.empty[A])((s, a) => s.orElse(Some(a)))((a, b) => a.orElse(b))(identity)

  /** the last element, if any (merge keeps the right side's) */
  def last[A]: Aggregator[A, Option[A], Option[A]] =
    apply[A, Option[A], Option[A]](Option.empty[A])((_, a) => Some(a))((a, b) => b.orElse(a))(identity)

  /**
   * The k greatest elements, descending. The accumulator is kept sorted
   * and capped at k, so an element that does not beat the k-th is
   * refused with one comparison and no allocation. Ties: an element
   * EQUAL to the k-th is refused, so among equals the first seen
   * survives.
   */
  def topK[A](k: Int)(implicit O: Ordering[A]): Aggregator[A, List[A], List[A]] = {
    // a loop (specs/stack-safety.md): the kept prefix is carried
    // reversed and put back in front once the insertion point is found
    @tailrec def insert(a: A, xs: List[A], remaining: Int, keptRev: List[A]): List[A] =
      if (remaining <= 0) keptRev.reverse
      else xs match {
        case Nil => keptRev reverse_::: (a :: Nil)
        case h :: t =>
          if (O.gt(h, a)) insert(a, t, remaining - 1, h :: keptRev)
          else keptRev reverse_::: (a :: xs.take(remaining - 1))
      }
    def keep(xs: List[A]) = xs.sorted(O.reverse).take(k)
    apply[A, List[A], List[A]](List.empty[A]) { (s, a) =>
      val kth = s.drop(k - 1)
      if (kth.isEmpty || O.gt(a, kth.head)) insert(a, s, k, Nil) else s
    }((a, b) => keep(a ++ b))(identity)
  }

  /** the distinct elements, exactly (bounded data; sketches for the rest) */
  def distinct[A]: Aggregator[A, Set[A], Long] =
    apply[A, Set[A], Long](Set.empty[A])(_ + _)(_ ++ _)(_.size.toLong)

  /** one aggregator per key, in one pass */
  def groupBy[K, In, Acc, Out](key: In => K)(agg: Aggregator[In, Acc, Out]): Aggregator[In, Map[K, Acc], Map[K, Out]] =
    apply[In, Map[K, Acc], Map[K, Out]](Map.empty[K, Acc]) { (m, in) =>
      val k = key(in)
      m.updated(k, agg.add(m.getOrElse(k, agg.init), in))
    } { (a, b) =>
      b.foldLeft(a)((m, kv) => m.updated(kv._1, m.get(kv._1).fold(kv._2)(agg.merge(_, kv._2))))
    }(_.view.mapValues(agg.present).toMap)
}
