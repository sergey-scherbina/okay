package okay

/**
 * A ready effect's handler as a VALUE (level 1, specs/shift-effect.md): `p.handle(State(5))` removes `E` from
 * the row and answers `O[A]`, for any program whose answer is an `I`. One way to take an effect off, for
 * every effect: `p.handle(State(5)).handle(Throws.either).run`. `Needs` is what the handler needs of the rest
 * of the row (most need nothing: `Handling.Plain`).
 */
trait Handling[E[+_], I, O[_], Needs[_[+_]]]:
  def run[A, F[+_]](p: A ! E + F)(using A <:< I, Distinct[E + F], Needs[F]): O[A] ! F

object Handling:
  /** the evidence of nothing: always there */
  final class Nothing[F[+_]] private[Handling] ()
  object Nothing:
    given any[F[+_]]: Nothing[F] = new Nothing[F]()

  /** a handler that needs nothing of the rest of the row */
  type Plain[E[+_], I, O[_]] = Handling[E, I, O, Handling.Nothing]
