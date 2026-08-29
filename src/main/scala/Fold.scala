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
