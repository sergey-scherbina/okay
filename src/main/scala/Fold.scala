package okay

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

  /** how many elements */
  def count[A]: Fold[A, Long] = Fold(0L)((n, _) => n + 1)

  /** the sum */
  def sum[N](using N: Numeric[N]): Fold[N, N] = Fold(N.zero)(N.plus)

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
