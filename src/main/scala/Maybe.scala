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
final case class Maybe[+A](value: Option[A]) extends Final derives Effect:
  /** nothing there: no handler resumes it */
  override def isFinal: Boolean = value.isEmpty

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

  /**
   * SKIP INSTEAD OF STOP. `run` answers a whole program's absence;
   * these answer it per ELEMENT or per BRANCH, and that is what makes
   * `Maybe` more than `Abort`: an absence is decided by the handler's
   * SCOPE, and a narrow scope drops one element and goes on.
   *
   * `collect` runs `f` once per element, each under its own `run`, and
   * keeps the elements that were there — Haskell's `mapMaybe`, Scala's
   * `collect`, with an effectful `f`. Effects of `f` other than Maybe
   * are forwarded in element order, including those of an element that
   * then turned out empty.
   */
  def collect[X, B, F[+_]](xs: Iterable[X])(f: X => B ! Maybe + F): Vector[B] ! F =
    xs.foldLeft(pure[F, Vector[B]](Vector.empty)): (acc, x) =>
      acc.flatMap(v => run[B, F](f(x)).map(o => if o.isEmpty then v else v :+ o.get))

  /**
   * An absence as a PRUNED BRANCH: every `Maybe` becomes a `Choose` —
   * `Some(x)` one alternative, `None` none — so under `runChoice` a
   * branch that found nothing dies and the others go on, and the
   * answers are exactly the branches where everything was there.
   *
   * A walk of its own rather than `Effects.interpret`: the row being
   * pruned usually carries `Choose` ALREADY (that is why it is pruned),
   * and `interpret`'s `Distinct[Maybe + Choose + H]` refuses a second
   * `Choose`. Here `Maybe` is tested first and everything else is
   * forwarded as it is, so `H` may hold `Choose` — the new alternatives
   * join the ones already there, which is the point.
   */
  def prune[A, H[+_]](p: A ! Maybe + H): A ! Choose + H = (p.resume: @unchecked) match
    case Free.Return(a) => Free.Return(a)
    case Free.Inject(e) => split[Maybe, H](e)
      (m => Free.Inject(Choose(m.value.toList)): A ! Choose + H)
      (h => Free.Inject(h): A ! Choose + H)
    case Free.Bind(Free.Inject(e), k) => split[Maybe, H](e)
      (m => Free.Inject(Choose(m.value.toList)).flatMap(x => prune(k(x))): A ! Choose + H)
      (h => Free.Inject(h).flatMap(x => prune(k(x))): A ! Choose + H)

  extension [A, F[+_]](p: A ! Maybe + F)
    /** where p found nothing, try q — a handler installed over p alone,
     * so the row comes out unchanged (Throws' `orElse`, for absence) */
    def orElse(q: => A ! Maybe + F): A ! Maybe + F =
      run[A, F](p).at[Maybe + F].flatMap:
        case Some(a) => pure(a)
        case None => q

    /** the value, or the default where p found nothing */
    def getOrElse(a: => A): A ! F = run[A, F](p).map(_.getOrElse(a))
