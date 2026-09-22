package okay

/**
 * How a forwarded operation reports its failure to the scope that
 * forwarded it.  Concrete failure semantics belong to the effect that
 * can fail; `okay-async` supplies the Async implementation.
 */
trait Failing[F[+_]]:
  def guard[X](e: F[X], onFailure: () => Unit): F[X]

object Failing:
  /** Pure has no failure channel to decorate. */
  given pure: Failing[Pure] with
    def guard[X](e: Nothing, onFailure: () => Unit): Nothing = e
