package okay.scala2

import okay.TypeableK
import Rows.{Top, coerce}

/**
 * Nondeterminism for Scala 2.13 (specs/scala2-facade.md, stage 13):
 * okay's `Choose` and `Logic`, as a capability of `Eff`.
 *
 * A program that performs `Choose.choose(1, 2, 3)` has several answers.
 * A handler decides what they mean: `runChoice` collects every one,
 * `Logic.observe(n)` the first n (lazily, so an infinite search is
 * fine), `Logic.cut` commits to the first, and `Logic.ifte` is the soft
 * cut. `interleave` and `fairBind` are
 * the FAIR forms, where an infinite branch cannot starve the other one.
 *
 * THE RESIDUAL TEST (residual-row-typeable). okay's fair combinators
 * declare a `TypeableK[F]` for the rest of the row, and on the Scala 2
 * side that rest is only the phantom `R`, stored at `Top`. The instance
 * here is the COMPLEMENT of the side that is known: an operation of the
 * rest is anything that is not a `Choose`. That is the core's own
 * "test one side, take the other by exclusion", packaged as an
 * instance. It was also found (2026-09-23) that `Logic.interleave`,
 * `fairBind` and `observe` never CONSULT that instance; their only
 * split is `msplit`'s `split[Choose, F]`, which tests the `Choose` side.
 * So the complement cannot misroute anything today. If the core ever
 * starts to call it, it is still the right answer for a two-part row.
 */
sealed trait Choose

object Choose {

  /** one of `as`: every one is an answer (okay's `choose`) */
  def choose[A](as: A*): Eff[Choose, A] = Eff.of(coerce(okay.choose(as*)))

  /** no answer at all */
  def fail[A]: Eff[Choose, A] = choose[A]()

  /** continue only if `ok` */
  def guard(ok: Boolean): Eff[Choose, Unit] = if (ok) Eff.pure(()) else fail[Unit]

  /** every answer, in order (okay's `runChoice`), the rest of the row
   * forwarded */
  def runChoice[A, R](e: Eff[Choose & R, A]): Eff[R, Seq[A]] =
    Eff.of(coerce(okay.runChoice[A, Top](coerce(e.program))))
}

/**
 * Backtracking search over `Choose` — okay's and okay2's `Logic`, under
 * their names. The combinators that KEEP `Choose` in the row take the
 * whole row as `R <: Choose` rather than `Choose & R`: written the
 * second way, scalac 2.13 solves the rest to `Any` for a program that is
 * only `Choose`, and `-Xlint` reports it; an upper bound states the same
 * requirement with nothing left to infer. A call without type arguments
 * reads the same against okay2.
 */
object Logic {

  private[scala2] given residual: TypeableK[Top] = new TypeableK[Top] {
    private val choose = summon[TypeableK[okay.Choose]]
    def test(x: Any): Boolean = !choose.test(x)
  }

  /** the first `n` answers; the search stops there, so it may be infinite */
  def observe[A, R](n: Int)(e: Eff[Choose & R, A]): Eff[R, Seq[A]] =
    Eff.of(coerce(okay.Logic.observe[A, Top](n)(coerce(e.program))))

  /** the first answer and the rest of the search as a program, or None */
  def msplit[A, R](m: Eff[Choose & R, A]): Eff[R, Option[(A, Eff[Choose & R, A])]] =
    Eff.of(coerce(okay.Logic.msplit[A, Top](coerce(m.program))
      .map(_.map { case (a, rest) => (a, Eff.of[Choose & R, A](coerce(rest))) })))

  /** commit to the first answer, dropping the rest of the search */
  def cut[R <: Choose, A](e: Eff[R, A]): Eff[R, A] =
    Eff.of(coerce(okay.Logic.cut[A, Top](coerce(e.program))))

  /** the soft cut: `th` for every answer of `cond`, and `el` only if
   * `cond` has none */
  def ifte[R <: Choose, A, B](cond: Eff[R, A])(th: A => Eff[R, B])(el: => Eff[R, B]): Eff[R, B] =
    Eff.of(coerce(okay.Logic.ifte[A, B, Top](coerce(cond.program))(a => coerce(th(a).program))(coerce(el.program))))

  /** negation as failure: one answer exactly when `m` has none */
  def gnot[R <: Choose, A](m: Eff[R, A]): Eff[R, Unit] =
    Eff.of(coerce(okay.Logic.gnot[A, Top](coerce(m.program))))

  /** the FAIR or: answers of `a` and `b` take turns */
  def interleave[R <: Choose, A](a: Eff[R, A], b: => Eff[R, A]): Eff[R, A] =
    Eff.of(coerce(okay.Logic.interleave[A, Top](coerce(a.program), coerce(b.program))))

  /** the FAIR bind: every answer of `m` gets a turn before one branch of
   * `f` can monopolise the search */
  def fairBind[R <: Choose, A, B](m: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
    Eff.of(coerce(okay.Logic.fairBind[A, B, Top](coerce(m.program))(a => coerce(f(a).program))))
}

/**
 * Search over samples, for any program: the shape okay-agent's `Search`
 * gives a model's completions ("sample until the answer is valid"),
 * over `Choose` underneath, so `bestOf` stops at the first good sample.
 */
object Search {

  private def samples[R, A](n: Int)(gen: Eff[R, A])(ok: A => Boolean): Eff[Choose & R, A] =
    Choose.choose((1 to n)*).flatMap(_ => gen).flatMap(a => Choose.guard(ok(a)).map(_ => a))

  /** run `gen` up to `n` times, and answer the first result that passes `ok` */
  def bestOf[R, A](n: Int)(gen: Eff[R, A])(ok: A => Boolean): Eff[R, Option[A]] =
    Choose.runChoice(Logic.cut(samples(n)(gen)(ok))).map(_.headOption)

  /** run `gen` `n` times, and answer every result that passes `ok` */
  def all[R, A](n: Int)(gen: Eff[R, A])(ok: A => Boolean): Eff[R, Seq[A]] =
    Choose.runChoice(samples(n)(gen)(ok))

  /** the most frequent answer (self-consistency) */
  def majority[A](answers: Seq[A]): Option[A] =
    answers.groupBy(identity).maxByOption(_._2.size).map(_._1)
}
