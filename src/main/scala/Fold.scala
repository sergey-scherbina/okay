package okay

import scala.compiletime.summonFrom

/**
 * The algebra of a left fold: a start and a step. Weaker than a
 * Monoid — no combine of two S is required, so a count, a sum, a
 * last-value, a hash, an IO sink all fit; every Monoid[W] would give
 * the diagonal Fold[W, W] (init = empty, add = combine). One algebra,
 * every consumer: Foldable containers run it over their elements,
 * Writer folds its told values with it.
 */
trait Fold[A, S]:
  /** the empty output */
  def init: S

  /** accept one element */
  def add(s: S, a: A): S

object Fold:
  /** the default option: collect in order (a Vector underneath) */
  given [A]: Fold[A, Seq[A]] = new:
    def init: Seq[A] = Vector.empty
    def add(s: Seq[A], a: A): Seq[A] = s :+ a

  /** make a Fold from a start and a step */
  def apply[A, S](z: S)(f: (S, A) => S): Fold[A, S] = new:
    def init: S = z
    def add(s: S, a: A): S = f(s, a)

  // ------------------------------------------- unboxed accumulators
  //
  // `Chunks.foldLeft` fixes the boxing where the step is written at
  // the call site. These fix it where it is NOT — where the fold
  // arrives as data, from an `Aggregator`, a java `Collector`, a
  // choice made at runtime — which is the case nothing can inline.
  //
  // Measured, per 10k elements: with the accumulator generic and the
  // element read directly, 45.2us; with the element boxed and the
  // accumulator a raw long, 3.8 against a 2.5 floor. The accumulator
  // is essentially the whole cost, so it is the only one specialized
  // here and the element stays generic — which is what makes these
  // useful, since `Chunks.fold` cannot know `A` either way.
  //
  // The extra method name is not decoration. Erasure is fixed at the
  // DECLARATION: `add(s: S, a: A): S` erases to `(Object, Object)Object`
  // in the generic parent and stays that way in every subtype, so
  // re-declaring it with `S = Long` would be the same symbol and the
  // same boxing. `addLong` is declared where the type is already
  // primitive, so it erases to `(long, Object)long`. It is the same
  // reason the JDK has `LongBinaryOperator` next to
  // `BinaryOperator<Long>`.
  //
  // Four primitives, not eight: Long (counts, sums, hashes), Int
  // (indices, small counters), Double (means, variances, norms) and
  // Boolean (exists, forall) are what accumulators are actually made
  // of. Float loses to Double by convention, and Short/Byte/Char
  // widen to Int before they arithmetic.

  /** a fold into a `long` that never leaves the register */
  trait OfLong[A] extends Fold[A, Long]:
    def initLong: Long
    def addLong(s: Long, a: A): Long
    final def init: Long = initLong
    final def add(s: Long, a: A): Long = addLong(s, a)

  /** a fold into an `int` */
  trait OfInt[A] extends Fold[A, Int]:
    def initInt: Int
    def addInt(s: Int, a: A): Int
    final def init: Int = initInt
    final def add(s: Int, a: A): Int = addInt(s, a)

  /** a fold into a `double` */
  trait OfDouble[A] extends Fold[A, Double]:
    def initDouble: Double
    def addDouble(s: Double, a: A): Double
    final def init: Double = initDouble
    final def add(s: Double, a: A): Double = addDouble(s, a)

  /** a fold into a `boolean` */
  trait OfBoolean[A] extends Fold[A, Boolean]:
    def initBoolean: Boolean
    def addBoolean(s: Boolean, a: A): Boolean
    final def init: Boolean = initBoolean
    final def add(s: Boolean, a: A): Boolean = addBoolean(s, a)

  /**
   * Build one — and `inline`, which is not decoration either.
   *
   * A plain constructor would store the step as a `(Long, A) => Long`,
   * and `Function2.apply` erases to `(Object, Object)Object` like
   * everything else generic — so the boxing the subtrait just removed
   * would come straight back in the field it closed over, and
   * `addLong` would unbox only to re-box. Measured before this was
   * inline: 27.5us against 9.1 for a fold written as a direct `new
   * OfLong`, which is most of the win given away at the door.
   *
   * `inline f` beta-reduces the lambda into `addLong`'s body at the
   * construction site, so there is no function object left to call.
   * Measured after: 7.8us, against 34.1 for the generic fold.
   *
   * The compiler warns that the anonymous class is duplicated at each
   * inline site, and it is — that duplication IS the mechanism, since
   * a shared class would have to reach the step through a field and
   * the field would be a `Function2` again. Suppressed deliberately,
   * on all four.
   */
  @annotation.nowarn("id=E197")
  inline def long[A](z: Long)(inline f: (Long, A) => Long): OfLong[A] = new:
    def initLong: Long = z
    def addLong(s: Long, a: A): Long = f(s, a)

  @annotation.nowarn("id=E197")
  inline def int[A](z: Int)(inline f: (Int, A) => Int): OfInt[A] = new:
    def initInt: Int = z
    def addInt(s: Int, a: A): Int = f(s, a)

  @annotation.nowarn("id=E197")
  inline def double[A](z: Double)(inline f: (Double, A) => Double): OfDouble[A] = new:
    def initDouble: Double = z
    def addDouble(s: Double, a: A): Double = f(s, a)

  @annotation.nowarn("id=E197")
  inline def boolean[A](z: Boolean)(inline f: (Boolean, A) => Boolean): OfBoolean[A] = new:
    def initBoolean: Boolean = z
    def addBoolean(s: Boolean, a: A): Boolean = f(s, a)

  /** how many elements — the counter stays a `long` */
  def count[A]: OfLong[A] = new:
    def initLong: Long = 0L
    def addLong(n: Long, a: A): Long = n + 1L

  /** does any element satisfy the predicate (it keeps scanning: a
   * `Fold` has no way to stop, which is what `Chunks.exists` is for) */
  def exists[A](p: A => Boolean): OfBoolean[A] = new:
    def initBoolean: Boolean = false
    def addBoolean(s: Boolean, a: A): Boolean = s || p(a)

  /** do all of them */
  def forall[A](p: A => Boolean): OfBoolean[A] = new:
    def initBoolean: Boolean = true
    def addBoolean(s: Boolean, a: A): Boolean = s && p(a)

  /** the unboxed sums, and the unboxed extremes */
  val sumLong: OfLong[Long] = new:
    def initLong: Long = 0L
    def addLong(s: Long, a: Long): Long = s + a

  val sumInt: OfInt[Int] = new:
    def initInt: Int = 0
    def addInt(s: Int, a: Int): Int = s + a

  val sumDouble: OfDouble[Double] = new:
    def initDouble: Double = 0.0
    def addDouble(s: Double, a: Double): Double = s + a

  val maxLong: OfLong[Long] = new:
    def initLong: Long = Long.MinValue
    def addLong(s: Long, a: Long): Long = if a > s then a else s

  val minLong: OfLong[Long] = new:
    def initLong: Long = Long.MaxValue
    def addLong(s: Long, a: Long): Long = if a < s then a else s

  val maxDouble: OfDouble[Double] = new:
    def initDouble: Double = Double.NegativeInfinity
    def addDouble(s: Double, a: Double): Double = if a > s then a else s

  val minDouble: OfDouble[Double] = new:
    def initDouble: Double = Double.PositiveInfinity
    def addDouble(s: Double, a: Double): Double = if a < s then a else s

  /**
   * The sum, unboxed WHERE THE TYPE IS KNOWN.
   *
   * `Numeric` cannot do this itself — `plus(x: T, y: T): T` erases to
   * `(Object, Object)Object` exactly like `add`, so it is the same
   * problem rather than a solution to it. What it can do is SELECT:
   * `summonFrom` asks whether `N` is one of the primitives at the call
   * site and hands back the specialized fold when it is.
   *
   * And the conversion back to `Fold[N, N]` is not a cast. The `=:=`
   * that identified `N` also transports the fold: `substituteCo` at
   * `[X] =>> Fold[X, X]` turns a `Fold[Long, Long]` into a
   * `Fold[N, N]` because the two types are, provably, the same one.
   */
  inline def sum[N](using N: Numeric[N]): Fold[N, N] =
    summonFrom {
      case ev: (N =:= Long) => ev.flip.substituteCo[[X] =>> Fold[X, X]](sumLong)
      case ev: (N =:= Int) => ev.flip.substituteCo[[X] =>> Fold[X, X]](sumInt)
      case ev: (N =:= Double) => ev.flip.substituteCo[[X] =>> Fold[X, X]](sumDouble)
      case _ => Fold(N.zero)(N.plus)
    }

  /** the first element, if any */
  def first[A]: Fold[A, Option[A]] = Fold(Option.empty[A])((s, a) => s.orElse(Some(a)))

  /** the last element, if any */
  def last[A]: Fold[A, Option[A]] = Fold(Option.empty[A])((_, a) => Some(a))

/** combine with a neutral element */
trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A
  extension (x: A)
    inline def |+|(y: A): A = combine(x, y)

/**
 * A Monoid that can also UN-combine: the inverse turns a sliding
 * window from recompute-from-scratch into subtract-what-aged-out.
 * Sum and count are groups; min/max (and the sketches) are not — no
 * way to un-see a value from a running max, and the type system says
 * so (a window over a Monoid-only type is a compile error).
 */
trait Group[A] extends Monoid[A]:
  def inverse(a: A): A
  extension (x: A)
    inline def |-|(y: A): A = combine(x, inverse(y))

/** numbers add, and addition inverts (the conventional default) */
given [N](using N: Numeric[N]): Group[N] = new:
  def empty: N = N.zero
  def combine(x: N, y: N): N = N.plus(x, y)
  def inverse(a: N): N = N.negate(a)

given Monoid[String] with
  def empty: String = ""
  def combine(x: String, y: String): String = x + y

given [A]: Monoid[List[A]] = new:
  def empty: List[A] = Nil
  def combine(x: List[A], y: List[A]): List[A] = x ++ y

/** every Alternative is a family of monoids (e.g. LazyList) */
given [F[_], A](using P: Alternative[F]): Monoid[F[A]] = new:
  def empty: F[A] = P.empty
  def combine(x: F[A], y: F[A]): F[A] = P.append(x)(y)

/** every Monoid folds on its own diagonal */
given [W](using M: Monoid[W]): Fold[W, W] = Fold(M.empty)(M.combine)

/**
 * The push side of consumption: a Foldable runs a Fold over all its
 * elements and yields only the output. The pull side is Stream
 * (codata, uncons) — a stream consumes through its LazyList bridge.
 */
trait Foldable[F[_]]:
  def fold[A, S](fa: F[A])(using f: Fold[A, S]): S

/** every IterableOnce container runs a Fold by foldLeft */
given [F[X] <: IterableOnce[X]]: Foldable[F] = new:
  def fold[A, S](fa: F[A])(using f: Fold[A, S]): S =
    fa.iterator.foldLeft(f.init)(f.add)

extension [F[_], A](fa: F[A])(using F: Foldable[F])
  /** run any Fold over the elements */
  def foldTo[S](using Fold[A, S]): S = F.fold(fa)
