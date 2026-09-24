package okay2

/**
 * The algebra of a left fold: a start and a step. Weaker than a
 * Monoid — no combine of two S is required, so a count, a sum, a
 * last-value, a hash, an IO sink all fit. One algebra, every
 * consumer: a Stream runs it over its elements, Writer folds its told
 * values with it, Chunks folds chunk by chunk.
 */
trait Fold[-A, S] {
  /** the empty output */
  def init: S

  /** accept one element */
  def add(s: S, a: A): S
}

object Fold {
  /** the default: collect in order (a Vector underneath) */
  implicit def collect[A]: Fold[A, Seq[A]] = new Fold[A, Seq[A]] {
    def init: Seq[A] = Vector.empty
    def add(s: Seq[A], a: A): Seq[A] = s :+ a
  }

  /** make a Fold from a start and a step */
  def apply[A, S](z: S)(f: (S, A) => S): Fold[A, S] = new Fold[A, S] {
    def init: S = z
    def add(s: S, a: A): S = f(s, a)
  }

  // ------------------------------------------- unboxed accumulators
  //
  // The Scala 3 core's four primitive shapes, kept because the
  // DISPATCH they enable (`Chunks.fold`, `Streams.fold`) is the
  // same here. What is NOT kept is the inline constructor that
  // beta-reduced the step into `addLong`: without `inline`, `Fold.long`
  // stores its step as a `Function2` and `addLong` calls through it —
  // the accumulator still stays a `long` across the loop, the step's
  // element is boxed as it always was, and the step's RESULT boxes on
  // its way out of the `Function2`. A fold written as a direct
  // `new OfLong { ... }` pays neither; the builders are the
  // convenient road, not the fast one (specs/okay2.md, stage 3).

  /** a fold into a `long` that never leaves the register */
  trait OfLong[-A] extends Fold[A, Long] {
    def initLong: Long
    def addLong(s: Long, a: A): Long
    final def init: Long = initLong
    final def add(s: Long, a: A): Long = addLong(s, a)
  }

  /** a fold into an `int` */
  trait OfInt[-A] extends Fold[A, Int] {
    def initInt: Int
    def addInt(s: Int, a: A): Int
    final def init: Int = initInt
    final def add(s: Int, a: A): Int = addInt(s, a)
  }

  /** a fold into a `double` */
  trait OfDouble[-A] extends Fold[A, Double] {
    def initDouble: Double
    def addDouble(s: Double, a: A): Double
    final def init: Double = initDouble
    final def add(s: Double, a: A): Double = addDouble(s, a)
  }

  /** a fold into a `boolean` */
  trait OfBoolean[-A] extends Fold[A, Boolean] {
    def initBoolean: Boolean
    def addBoolean(s: Boolean, a: A): Boolean
    final def init: Boolean = initBoolean
    final def add(s: Boolean, a: A): Boolean = addBoolean(s, a)
  }

  def long[A](z: Long)(f: (Long, A) => Long): OfLong[A] = new OfLong[A] {
    def initLong: Long = z
    def addLong(s: Long, a: A): Long = f(s, a)
  }

  def int[A](z: Int)(f: (Int, A) => Int): OfInt[A] = new OfInt[A] {
    def initInt: Int = z
    def addInt(s: Int, a: A): Int = f(s, a)
  }

  def double[A](z: Double)(f: (Double, A) => Double): OfDouble[A] = new OfDouble[A] {
    def initDouble: Double = z
    def addDouble(s: Double, a: A): Double = f(s, a)
  }

  def boolean[A](z: Boolean)(f: (Boolean, A) => Boolean): OfBoolean[A] = new OfBoolean[A] {
    def initBoolean: Boolean = z
    def addBoolean(s: Boolean, a: A): Boolean = f(s, a)
  }

  /** how many elements — the counter stays a `long` */
  def count[A]: OfLong[A] = new OfLong[A] {
    def initLong: Long = 0L
    def addLong(s: Long, a: A): Long = s + 1L
  }

  /** is there an element satisfying p (the whole input is read; `FoldUntil.exists` stops) */
  def exists[A](p: A => Boolean): OfBoolean[A] = new OfBoolean[A] {
    def initBoolean: Boolean = false
    def addBoolean(s: Boolean, a: A): Boolean = s || p(a)
  }

  /** do all elements satisfy p */
  def forall[A](p: A => Boolean): OfBoolean[A] = new OfBoolean[A] {
    def initBoolean: Boolean = true
    def addBoolean(s: Boolean, a: A): Boolean = s && p(a)
  }

  /** the sum of `long`s, unboxed */
  val sumLong: OfLong[Long] = new OfLong[Long] {
    def initLong: Long = 0L
    def addLong(s: Long, a: Long): Long = s + a
  }

  /** the sum of `int`s, unboxed */
  val sumInt: OfInt[Int] = new OfInt[Int] {
    def initInt: Int = 0
    def addInt(s: Int, a: Int): Int = s + a
  }

  /** the sum of `double`s, unboxed */
  val sumDouble: OfDouble[Double] = new OfDouble[Double] {
    def initDouble: Double = 0.0
    def addDouble(s: Double, a: Double): Double = s + a
  }

  val maxLong: OfLong[Long] = new OfLong[Long] {
    def initLong: Long = Long.MinValue
    def addLong(s: Long, a: Long): Long = math.max(s, a)
  }

  val minLong: OfLong[Long] = new OfLong[Long] {
    def initLong: Long = Long.MaxValue
    def addLong(s: Long, a: Long): Long = math.min(s, a)
  }

  val maxDouble: OfDouble[Double] = new OfDouble[Double] {
    def initDouble: Double = Double.NegativeInfinity
    def addDouble(s: Double, a: Double): Double = if (a > s) a else s
  }

  val minDouble: OfDouble[Double] = new OfDouble[Double] {
    def initDouble: Double = Double.PositiveInfinity
    def addDouble(s: Double, a: Double): Double = if (a < s) a else s
  }

  /** every Monoid folds on its own diagonal — implicit, as in the core:
   * okay2 passes a Fold explicitly everywhere its output type is not
   * already fixed, so this cannot compete with `collect` */
  implicit def fromMonoid[W](implicit M: Monoid[W]): Fold[W, W] = Fold(M.empty)(M.combine)

  /** the sum in any Numeric: the generic fold — the three unboxed
   * sums above are the ones to name where the type is known (the
   * Scala 3 core picks them at compile time with `summonFrom`; here
   * the choice is the caller's) */
  def sum[N](implicit N: Numeric[N]): Fold[N, N] = new Fold[N, N] {
    def init: N = N.zero
    def add(s: N, a: N): N = N.plus(s, a)
  }

  /** the first element, if any */
  def first[A]: Fold[A, Option[A]] = Fold(Option.empty[A])((s, a) => s.orElse(Some(a)))

  /** the last element, if any */
  def last[A]: Fold[A, Option[A]] = Fold(Option.empty[A])((_, a) => Some(a))
}

/**
 * A fold that STOPS: `done` says the state has seen enough, and a
 * consumer asks for an element only while it has not — so a stream
 * that computes on demand computes nothing past the stop, and `end`
 * makes the result from wherever the walk stopped.
 */
trait FoldUntil[-A, S, R] {
  def init: S
  def add(s: S, a: A): S
  def done(s: S): Boolean
  def end(s: S): R
}

object FoldUntil {
  /** make one from its four parts */
  def apply[A, S, R](z: S)(f: (S, A) => S)(stop: S => Boolean)(finish: S => R): FoldUntil[A, S, R] = new FoldUntil[A, S, R] {
    def init: S = z
    def add(s: S, a: A): S = f(s, a)
    def done(s: S): Boolean = stop(s)
    def end(s: S): R = finish(s)
  }

  /** a step that answers `Left(next)` to continue or `Right(result)`
   * to stop, and `finish` for the state left when the input ran out */
  def until[A, S, R](z: S)(f: (S, A) => Either[S, R])(finish: S => R): FoldUntil[A, Either[S, R], R] =
    new FoldUntil[A, Either[S, R], R] {
      def init: Either[S, R] = Left(z)
      def add(s: Either[S, R], a: A): Either[S, R] = s match {
        case Left(s) => f(s, a)
        case r => r
      }
      def done(s: Either[S, R]): Boolean = s.isRight
      def end(s: Either[S, R]): R = s match {
        case Left(s) => finish(s)
        case Right(r) => r
      }
    }

  /** the first element satisfying the predicate, and no pull after it */
  def find[A](p: A => Boolean): FoldUntil[A, Option[A], Option[A]] = new FoldUntil[A, Option[A], Option[A]] {
    def init: Option[A] = None
    def add(s: Option[A], a: A): Option[A] = if (p(a)) Some(a) else s
    def done(s: Option[A]): Boolean = s.isDefined
    def end(s: Option[A]): Option[A] = s
  }

  /** the first element, if any */
  def headOption[A]: FoldUntil[A, Option[A], Option[A]] = find(_ => true)

  /** the first n elements */
  def take[A](n: Int): FoldUntil[A, Vector[A], Vector[A]] = new FoldUntil[A, Vector[A], Vector[A]] {
    def init: Vector[A] = Vector.empty
    def add(s: Vector[A], a: A): Vector[A] = s :+ a
    def done(s: Vector[A]): Boolean = s.length >= n
    def end(s: Vector[A]): Vector[A] = s
  }

  /** a stopping fold whose state is a `long` */
  trait OfLong[-A, R] extends FoldUntil[A, Long, R] {
    def initLong: Long
    def addLong(s: Long, a: A): Long
    def doneLong(s: Long): Boolean
    def endLong(s: Long): R
    final def init: Long = initLong
    final def add(s: Long, a: A): Long = addLong(s, a)
    final def done(s: Long): Boolean = doneLong(s)
    final def end(s: Long): R = endLong(s)
  }

  trait OfInt[-A, R] extends FoldUntil[A, Int, R] {
    def initInt: Int
    def addInt(s: Int, a: A): Int
    def doneInt(s: Int): Boolean
    def endInt(s: Int): R
    final def init: Int = initInt
    final def add(s: Int, a: A): Int = addInt(s, a)
    final def done(s: Int): Boolean = doneInt(s)
    final def end(s: Int): R = endInt(s)
  }

  trait OfDouble[-A, R] extends FoldUntil[A, Double, R] {
    def initDouble: Double
    def addDouble(s: Double, a: A): Double
    def doneDouble(s: Double): Boolean
    def endDouble(s: Double): R
    final def init: Double = initDouble
    final def add(s: Double, a: A): Double = addDouble(s, a)
    final def done(s: Double): Boolean = doneDouble(s)
    final def end(s: Double): R = endDouble(s)
  }

  trait OfBoolean[-A, R] extends FoldUntil[A, Boolean, R] {
    def initBoolean: Boolean
    def addBoolean(s: Boolean, a: A): Boolean
    def doneBoolean(s: Boolean): Boolean
    def endBoolean(s: Boolean): R
    final def init: Boolean = initBoolean
    final def add(s: Boolean, a: A): Boolean = addBoolean(s, a)
    final def done(s: Boolean): Boolean = doneBoolean(s)
    final def end(s: Boolean): R = endBoolean(s)
  }

  def long[A, R](z: Long)(f: (Long, A) => Long)(stop: Long => Boolean)(finish: Long => R): OfLong[A, R] = new OfLong[A, R] {
    def initLong: Long = z
    def addLong(s: Long, a: A): Long = f(s, a)
    def doneLong(s: Long): Boolean = stop(s)
    def endLong(s: Long): R = finish(s)
  }

  def int[A, R](z: Int)(f: (Int, A) => Int)(stop: Int => Boolean)(finish: Int => R): OfInt[A, R] = new OfInt[A, R] {
    def initInt: Int = z
    def addInt(s: Int, a: A): Int = f(s, a)
    def doneInt(s: Int): Boolean = stop(s)
    def endInt(s: Int): R = finish(s)
  }

  def double[A, R](z: Double)(f: (Double, A) => Double)(stop: Double => Boolean)(finish: Double => R): OfDouble[A, R] = new OfDouble[A, R] {
    def initDouble: Double = z
    def addDouble(s: Double, a: A): Double = f(s, a)
    def doneDouble(s: Double): Boolean = stop(s)
    def endDouble(s: Double): R = finish(s)
  }

  def boolean[A, R](z: Boolean)(f: (Boolean, A) => Boolean)(stop: Boolean => Boolean)(finish: Boolean => R): OfBoolean[A, R] = new OfBoolean[A, R] {
    def initBoolean: Boolean = z
    def addBoolean(s: Boolean, a: A): Boolean = f(s, a)
    def doneBoolean(s: Boolean): Boolean = stop(s)
    def endBoolean(s: Boolean): R = finish(s)
  }

  /** does any element satisfy the predicate — stopping at the first that does */
  def exists[A](p: A => Boolean): OfBoolean[A, Boolean] = new OfBoolean[A, Boolean] {
    def initBoolean: Boolean = false
    def addBoolean(s: Boolean, a: A): Boolean = s || p(a)
    def doneBoolean(s: Boolean): Boolean = s
    def endBoolean(s: Boolean): Boolean = s
  }

  /** do all of them — stopping at the first that does not */
  def forall[A](p: A => Boolean): OfBoolean[A, Boolean] = new OfBoolean[A, Boolean] {
    def initBoolean: Boolean = true
    def addBoolean(s: Boolean, a: A): Boolean = s && p(a)
    def doneBoolean(s: Boolean): Boolean = !s
    def endBoolean(s: Boolean): Boolean = s
  }
}

/**
 * The push side of consumption: a Foldable runs a Fold over all its
 * elements and yields only the output. The pull side is Stream
 * (codata, uncons).
 */
trait Foldable[F[_]] {
  def fold[A, S](fa: F[A])(f: Fold[A, S]): S

  /** the fold that stops: elements are read only while `done` is
   * false, and none after */
  def foldUntil[A, S, R](fa: F[A])(fo: FoldUntil[A, S, R]): R
}

object Foldable {
  def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F

  /** every IterableOnce container runs a Fold by foldLeft, and a
   * FoldUntil by its iterator — which is left positioned after the
   * satisfying element, so an `Iterator` can be folded on in pieces */
  implicit def iterableOnce[F[X] <: IterableOnce[X]]: Foldable[F] = new Foldable[F] {
    def fold[A, S](fa: F[A])(f: Fold[A, S]): S = fa.iterator.foldLeft(f.init)(f.add)

    def foldUntil[A, S, R](fa: F[A])(fo: FoldUntil[A, S, R]): R = {
      val it = fa.iterator
      // the element type is erased, so these tests see only the shape —
      // the same unavoidable `@unchecked` `Stream.foldUntil` carries
      fo match {
        case l: FoldUntil.OfLong[A @unchecked, R @unchecked] =>
          var s = l.initLong
          while (!l.doneLong(s) && it.hasNext) s = l.addLong(s, it.next())
          l.endLong(s)
        case i: FoldUntil.OfInt[A @unchecked, R @unchecked] =>
          var s = i.initInt
          while (!i.doneInt(s) && it.hasNext) s = i.addInt(s, it.next())
          i.endInt(s)
        case d: FoldUntil.OfDouble[A @unchecked, R @unchecked] =>
          var s = d.initDouble
          while (!d.doneDouble(s) && it.hasNext) s = d.addDouble(s, it.next())
          d.endDouble(s)
        case b: FoldUntil.OfBoolean[A @unchecked, R @unchecked] =>
          var s = b.initBoolean
          while (!b.doneBoolean(s) && it.hasNext) s = b.addBoolean(s, it.next())
          b.endBoolean(s)
        case _ =>
          var s = fo.init
          while (!fo.done(s) && it.hasNext) s = fo.add(s, it.next())
          fo.end(s)
      }
    }
  }
}
