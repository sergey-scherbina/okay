package okay

import okay.Row.at

/**
 * A VALUE THAT MAY NOT BE THERE, as an effect of its own
 * (specs/core-gaps.md, 2026-09-26).
 *
 *     def age(name: String): Int ! Maybe = ages.get(name).maybe
 *
 * `Some(x).maybe` answers `x`; `None.maybe` stops the program, and
 * `Maybe.run` answers `None` for it — the `Option` moves out of every
 * signature along the way and into the handler at the end.
 *
 * WHY NOT `Abort`. `Abort` is `Throws % Unit`: a failure with nothing
 * to say. Operations are identified by class, so a row holds ONE
 * `Throws` — and a program that can both find nothing and fail with a
 * reason (`Abort + Throws % E`) was refused by `Distinct`. `Maybe` is
 * its own class, so `Maybe + Throws % E` is a good row, and "was it
 * not there, or did it break?" has two answers, from two handlers.
 *
 * WHY NOT `Option` ITSELF as the signature. It could be, the way a
 * `List` is nondeterminism (Choice.scala) — and Choice.scala gives the
 * reason not to: a row wants a signature that means ONE thing. The
 * box costs one allocation per operation, `Choose`'s price.
 *
 * The class IS the whole identity: `Maybe` has no parameter but its
 * erased answer type, so splitting a row on it is a TOTAL test.
 */
final case class Maybe[+A](value: Option[A]) derives Effect

extension [A](o: Option[A])
  /** the value, or stop: `None` ends the program at `Maybe.run` */
  inline def maybe: A ! Maybe = effect(Maybe(o))

object Maybe:
  /** stop: nothing is there */
  inline def none[A]: A ! Maybe = effect(Maybe(None))

  /** handle Maybe into Option, forwarding the effects F. `Some` is
   * answered in place (tail-resumptive, nothing captured); `None`
   * drops the rest of the program. */
  def run[A, F[+_]](p: A ! Maybe + F): Option[A] ! F =
    Effects[Free].handle[Maybe, F](p)(a => pure[F, Option[A]](Some(a))):
      [X] => m => m.value match
        case Some(x) => Cont.Pure(x)
        case None => shift(_ => pure[F, Option[A]](None))

  extension [A, F[+_]](p: A ! Maybe + F)
    /** where p found nothing, try q — a handler installed over p alone,
     * so the row comes out unchanged (Throws' `orElse`, for absence) */
    def orElse(q: => A ! Maybe + F): A ! Maybe + F =
      run[A, F](p).at[Maybe + F].flatMap:
        case Some(a) => pure(a)
        case None => q

    /** the value, or the default where p found nothing */
    def getOrElse(a: => A): A ! F = run[A, F](p).map(_.getOrElse(a))
