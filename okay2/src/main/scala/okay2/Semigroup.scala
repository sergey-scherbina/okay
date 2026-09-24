package okay2

/**
 * How two values of A combine into one — the Scala 3 core's `Semigroup`
 * (Fold.scala there). `Validated` asks for one to accumulate its errors,
 * and the CALLER chooses what accumulation means: concatenation for a
 * form, a count for a sampler, a map of field to problems for an API.
 */
trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  def apply[A](implicit S: Semigroup[A]): Semigroup[A] = S

  /** every Monoid is one */
  implicit def fromMonoid[A](implicit M: Monoid[A]): Semigroup[A] = M
}

/** a Semigroup with a neutral element */
trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit M: Monoid[A]): Monoid[A] = M

  /** make one from its two parts */
  def of[A](zero: A)(f: (A, A) => A): Monoid[A] = new Monoid[A] {
    def empty: A = zero
    def combine(x: A, y: A): A = f(x, y)
  }

  implicit def vector[A]: Monoid[Vector[A]] = of(Vector.empty[A])(_ ++ _)
  implicit def list[A]: Monoid[List[A]] = of(List.empty[A])(_ ++ _)
  implicit val string: Monoid[String] = of("")(_ + _)

  /** every Numeric is a Group under addition */
  implicit def numeric[N](implicit N: Numeric[N]): Group[N] = new Group[N] {
    def empty: N = N.zero
    def combine(x: N, y: N): N = N.plus(x, y)
    def inverse(a: N): N = N.negate(a)
  }
}

/** a Monoid with inverses */
trait Group[A] extends Monoid[A] {
  def inverse(a: A): A
}
