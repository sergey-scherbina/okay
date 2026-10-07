package okay.cats

/**
 * SEMIGROUP, MONOID, GROUP ACROSS cats-kernel AND okay
 * (specs/cats-kernel-bridge.md). okay's three (Fold.scala) and
 * cats-kernel's are separate classes; okay has no `Eq`/`Order`.
 *
 * Two tools, because the obvious one alone does not work:
 *
 *   - [[Combine]], for the `Validated` instances of this module: a
 *     combiner from EITHER library, okay's first. Ambiguity-free by
 *     construction (one owner, a priority chain), so it sits in the
 *     default import and neither `Validated` asks which side you hold.
 *   - [[FromCatsKernel]] / [[ToCatsKernel]], the general bridges, one
 *     import each like FromCats/ToCats. They are NOT folded into those
 *     two: measured, `FromCats.group` beside `okay.given` made
 *     `okay.freer.Group[Int]` ambiguous with okay's own numeric `Group`, two
 *     generic lexical givens with no priority between them. Imported only
 *     where a type has ONE side's instance, they never meet the other's.
 */
trait Combine[E]:
  def combine(x: E, y: E): E

object Combine extends CombineFromCats:
  /** okay's semigroup first */
  given fromOkay[E](using S: okay.freer.Semigroup[E]): Combine[E] = (x, y) => S.combine(x, y)

trait CombineFromCats:
  /** cats-kernel's when okay has none */
  given fromCats[E](using S: _root_.cats.kernel.Semigroup[E]): Combine[E] = (x, y) => S.combine(x, y)

/** okay's `Group`/`Monoid`/`Semigroup` from cats-kernel's
 * (`import okay.cats.FromCatsKernel.given`), in that priority. Not
 * together with [[ToCatsKernel]]. */
object FromCatsKernel extends FromCatsKernelMonoid:
  given group[A](using G: _root_.cats.kernel.Group[A]): okay.freer.Group[A] with
    def empty: A = G.empty
    def combine(x: A, y: A): A = G.combine(x, y)
    def inverse(a: A): A = G.inverse(a)

trait FromCatsKernelMonoid extends FromCatsKernelSemigroup:
  given monoid[A](using M: _root_.cats.kernel.Monoid[A]): okay.freer.Monoid[A] with
    def empty: A = M.empty
    def combine(x: A, y: A): A = M.combine(x, y)

trait FromCatsKernelSemigroup:
  given semigroup[A](using S: _root_.cats.kernel.Semigroup[A]): okay.freer.Semigroup[A] with
    def combine(x: A, y: A): A = S.combine(x, y)

/** cats-kernel's `Group`/`Monoid`/`Semigroup` from okay's
 * (`import okay.cats.ToCatsKernel.given`), in that priority. Not
 * together with [[FromCatsKernel]]. */
object ToCatsKernel extends ToCatsKernelMonoid:
  given group[A](using G: okay.freer.Group[A]): _root_.cats.kernel.Group[A] with
    def empty: A = G.empty
    def combine(x: A, y: A): A = G.combine(x, y)
    def inverse(a: A): A = G.inverse(a)

trait ToCatsKernelMonoid extends ToCatsKernelSemigroup:
  given monoid[A](using M: okay.freer.Monoid[A]): _root_.cats.kernel.Monoid[A] with
    def empty: A = M.empty
    def combine(x: A, y: A): A = M.combine(x, y)

trait ToCatsKernelSemigroup:
  given semigroup[A](using S: okay.freer.Semigroup[A]): _root_.cats.kernel.Semigroup[A] with
    def combine(x: A, y: A): A = S.combine(x, y)
