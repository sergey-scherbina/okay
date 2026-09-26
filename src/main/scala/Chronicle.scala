package okay

import scala.annotation.tailrec
import okay.!.*
import okay.Row.at

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
 *       if s.contains("_") then Chronicle.dictate(s"'$s' has an underscore").map(_ => s) else pure(s)
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
 * PARAMETERISED, so the derived test is by class only and a row holds
 * one Chronicle. It is class-distinct from Throws, Writer and Maybe.
 */
enum Chronicle[E, +A] derives Effect:
  /** record an error and go on */
  case Dictate(e: E) extends Chronicle[E, Unit]
  /** stop, with what has been recorded so far */
  case Halt() extends Chronicle[E, Nothing]

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

  /** the handler: every recorded error, in order, and whether the
   * program finished; forwards the effects F */
  def run[E, A, F[+_]](p: A ! Chronicle % E + F)(using Distinct[Chronicle % E + F]): Verdict[E, A] ! F = {
    def verdict[B](errs: List[E], a: B): Verdict[E, B] =
      if errs.isEmpty then Clean(a) else Warned(a, errs.reverse.toVector)

    def _loop(errs: List[E])(x: A ! Chronicle % E + F): Verdict[E, A] ! F = loop(errs)(x)

    // the seed of `Writer.loopWith`'s shape: a bespoke loop, because the
    // record has to be threaded through it
    @tailrec def loop(errs: List[E])(x: A ! Chronicle % E + F): Verdict[E, A] ! F = (x.resume: @unchecked) match
      case Return(a) => Return(verdict(errs, a))
      case Inject(e) => split[Chronicle % E, F](e) {
          case Dictate(err) => Return(verdict(err :: errs, ())): Verdict[E, A] ! F
          case Halt() => Return(Failed(errs.reverse.toVector)): Verdict[E, A] ! F
        } { e => Inject(e).map(verdict(errs, _)) }
      case Bind(Inject(e), k) => split[Chronicle % E, F](e) { c =>
          // the constructor refines the CONTINUATION's domain here; the
          // checker cannot see that under an existential answer type the
          // two cases are all there is — `Writer.loopWith`'s claim
          (c: @unchecked) match
            case Dictate(err) => loop(err :: errs)(k(()))
            case Halt() => Return(Failed(errs.reverse.toVector)): Verdict[E, A] ! F
        } { e => Inject(e).flatMap(x => _loop(errs)(k(x))) }

    loop(Nil)(p)
  }

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
  def all[E, X, B, F[+_]](xs: Iterable[X])(f: X => B ! Chronicle % E + F)
                         (using Distinct[Chronicle % E + F]): Vector[B] ! Chronicle % E + F =
    val each: (Vector[B], Vector[E], Boolean) ! F =
      xs.foldLeft(pure[F, (Vector[B], Vector[E], Boolean)]((Vector.empty, Vector.empty, false))): (acc, x) =>
        acc.flatMap: (bs, es, halted) =>
          run[E, B, F](f(x)).map:
            case Clean(b) => (bs :+ b, es, halted)
            case Warned(b, more) => (bs :+ b, es ++ more, halted)
            case Failed(more) => (bs, es ++ more, true)
    each.at[Chronicle % E + F].flatMap: (bs, es, halted) =>
      val told = es.foldLeft(pure[Chronicle % E + F, Unit](()))((acc, e) => acc.flatMap(_ => dictate(e).at[Chronicle % E + F]))
      told.flatMap(_ => if halted then halt[E, Vector[B]].at[Chronicle % E + F] else pure(bs))
