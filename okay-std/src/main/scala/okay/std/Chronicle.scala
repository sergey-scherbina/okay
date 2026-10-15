package okay.std

import okay.{!, %, Effect, Handler, Pure, Within, effect, pure}
import okay.core.Split

/**
 * ERRORS THAT ACCUMULATE, IN A PROGRAM THAT GOES ON
 * (specs/core-gaps.md stage 3).
 *
 * `Throws` stops at the first error. `Validated` collects every error,
 * but only across checks that do not depend on each other. `Chronicle`
 * sits between them: a program RECORDS an error (`dictate`) and goes
 * on, possibly with a fallback, and stops (`halt`, or `confess` = record
 * then stop) where it cannot. The handler answers a `Verdict`: `Clean`
 * where nothing was recorded, `Warned` where errors were recorded and
 * the program still finished, `Failed` where it halted.
 *
 *     def host(s: String): String ! Chronicle % String =
 *       if s.contains("_") then Chronicle.dictate(s"'\$s' has an underscore").map(_ => s) else pure(s)
 *
 * The name and the operations are from Haskell's `these` package
 * (`MonadChronicle`: dictate, confess, condemn). The same idea is cats'
 * `Ior`, arrow-kt's `Raise.accumulate`, and zio-prelude's `ZValidation`
 * with warnings.
 *
 * WHY NOT `Writer % E + Throws % E`: it takes the row's one `Throws`
 * slot (so a program raising `Throws % IOError` could not also
 * accumulate), its warnings vanish when the handlers run in the wrong
 * order, and it has no `all`. The spec says so at length.
 *
 * PARAMETERISED; on the kernel's rows an operation goes to its handler
 * by its PATH, so two Chronicles in a row are two places (okay-freer's
 * `Distinct` was the union's). It is distinct from Throws, Writer and Maybe.
 */
enum Chronicle[E, +A] derives Effect:
  /** record an error and go on */
  case Dictate(e: E) extends Chronicle[E, Unit]
  /** stop, with what has been recorded so far */
  case Halt() extends Chronicle[E, Nothing], Final

object Chronicle:
  /** record an error and go on */
  inline def dictate[E](e: E): Unit ! Chronicle % E = effect(Dictate(e))

  /** stop, with what has been recorded so far (`these`' condemn shape) */
  inline def halt[E, A]: A ! Chronicle % E = effect[Chronicle % E, A](Halt())

  /** record an error and stop */
  inline def confess[E, A](e: E): A ! Chronicle % E = dictate(e).flatMap(_ => halt[E, A])

  /** what a chronicled program came to */
  enum Verdict[+E, +A]:
    /** finished, nothing recorded */
    case Clean(a: A)
    /** finished, with errors recorded on the way */
    case Warned(a: A, errors: Vector[E])
    /** halted; the errors recorded until then (empty only for a bare `halt`) */
    case Failed(errors: Vector[E]) extends Verdict[E, Nothing]

  import Verdict.*

  /** the handler as a value: `p.handle(Chronicle.verdict)`, every recorded error, in order, and whether the
   * program finished. A clause given `k` (the kernel's `Deep`): a dictate resumes the rest once and puts its error in
   * front of the rest's verdict; a halt drops the rest, `Failed` with nothing after it */
  def verdict[E]: Handler[Chronicle % E, [A] =>> Verdict[E, A]] =
    Handler.deep[Chronicle % E, [A] =>> Verdict[E, A]]([A] => (a: A) => Clean(a))(
      [A, X] => (op: Chronicle[E, X], k: X => Verdict[E, A] ! Pure) => op match
        case Dictate(err) => k(()).map(recorded(err, _))
        case Halt() => pure[Verdict[E, A], Pure](Failed(Vector.empty)))

  /** `err`, recorded before what the rest came to */
  private def recorded[E, A](err: E, v: Verdict[E, A]): Verdict[E, A] = v match
    case Clean(a) => Warned(a, Vector(err))
    case Warned(a, errs) => Warned(a, err +: errs)
    case Failed(errs) => Failed(err +: errs)

  /** every recorded error, in order, and whether the program finished — wherever Chronicle is among the program's
   * effects: the rest of them forwarded */
  def run[E, A, Es[_]](p: A ! Es)(using s: Split[Es, Chronicle % E]): Verdict[E, A] ! s.Rest = p.handle(verdict[E])

  /**
   * ACCUMULATE ACROSS ELEMENTS (arrow-kt's `mapOrAccumulate`): each
   * element runs under its own `run`, so one element's halt does not
   * stop the next. Every element's errors are then recorded in element
   * order, and if any element halted, so does this, after all of them
   * ran. Otherwise it answers every element's value.
   *
   * The errors are recorded when the last element is done, not when each
   * happened. The `Verdict` sees the same list in the same order. A
   * forwarded effect sees them after every element's own work.
   */
  def all[E, X, B, Es[_]](xs: Iterable[X])(f: X => B ! Es)
                         (using s: Split[Es, Chronicle % E], w: Within[s.Rest, Es], c: Within[Chronicle % E, Es])
                         : Vector[B] ! Es =
    val perElement: (Vector[B], Vector[E], Boolean) ! s.Rest =
      foldM[X, (Vector[B], Vector[E], Boolean), s.Rest](xs)((Vector.empty[B], Vector.empty[E], false)): (acc, x) =>
        val (bs, es, halted) = acc
        run[E, B, Es](f(x))(using s).map:
          case Clean(b) => (bs :+ b, es, halted)
          case Warned(b, more) => (bs :+ b, es ++ more, halted)
          case Failed(more) => (bs, es ++ more, true)
    perElement.at[Es].flatMap: (bs, es, halted) =>
      val told = foldM[E, Unit, Es](es)(())((_, e) => dictate(e).at[Es])
      told.flatMap(_ => if halted then halt[E, Vector[B]].at[Es] else pure[Vector[B], Es](bs))
