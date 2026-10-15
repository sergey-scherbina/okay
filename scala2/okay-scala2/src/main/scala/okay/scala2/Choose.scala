package okay.scala2

import okay.{+, !}
import Rows.{Top, erase, moved}

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
 * Underneath, a combinator moves `Choose`'s operations to it beside the
 * rest of the row (`Rows.moved`) and is the core's own; one that keeps
 * `Choose` puts it back at the row's one place (`Rows.erase`).
 */
sealed trait Choose

object Choose {

  /** the row a combinator works at: `Choose` beside the rest */
  private[scala2] type C = okay.std.Choose + Top

  /** one of `as`: every one is an answer (okay's `choose`) */
  def choose[A](as: A*): Eff[Choose, A] = Eff.of(erase(okay.std.choose(as*)))

  /** no answer at all */
  def fail[A]: Eff[Choose, A] = choose[A]()

  /** continue only if `ok` */
  def guard(ok: Boolean): Eff[Choose, Unit] = if (ok) Eff.pure(()) else fail[Unit]

  /** every answer, in order (okay's `runChoice`), the rest of the row
   * forwarded */
  def runChoice[A, R](e: Eff[Choose & R, A]): Eff[R, Seq[A]] =
    Eff.of(okay.std.runChoice[A, C](moved[okay.std.Choose, A](e.program)))
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

  import Choose.C

  /** `Choose`'s operations beside the rest */
  private def lift[A](e: Eff[?, A]): A ! C = moved[okay.std.Choose, A](e.program)

  /** the first `n` answers; the search stops there, so it may be infinite */
  def observe[A, R](n: Int)(e: Eff[Choose & R, A]): Eff[R, Seq[A]] =
    Eff.of(okay.std.Logic.observe[A, C](n)(lift(e)))

  /** the first answer and the rest of the search as a program, or None */
  def msplit[A, R](m: Eff[Choose & R, A]): Eff[R, Option[(A, Eff[Choose & R, A])]] =
    Eff.of(okay.std.Logic.msplit[A, C](lift(m))
      .map(_.map { case (a, rest) => (a, Eff.of[Choose & R, A](erase(rest))) }))

  /** commit to the first answer, dropping the rest of the search */
  def cut[R <: Choose, A](e: Eff[R, A]): Eff[R, A] =
    Eff.of(erase(okay.std.Logic.cut[A, C](lift(e))))

  /** the soft cut: `th` for every answer of `cond`, and `el` only if
   * `cond` has none */
  def ifte[R <: Choose, A, B](cond: Eff[R, A])(th: A => Eff[R, B])(el: => Eff[R, B]): Eff[R, B] =
    Eff.of(erase(okay.std.Logic.ifte[A, B, C](lift(cond))(a => lift(th(a)))(lift(el))))

  /** negation as failure: one answer exactly when `m` has none */
  def gnot[R <: Choose, A](m: Eff[R, A]): Eff[R, Unit] =
    Eff.of(erase(okay.std.Logic.gnot[A, C](lift(m))))

  /** the FAIR or: answers of `a` and `b` take turns */
  def interleave[R <: Choose, A](a: Eff[R, A], b: => Eff[R, A]): Eff[R, A] =
    Eff.of(erase(okay.std.Logic.interleave[A, C](lift(a), lift(b))))

  /** the FAIR bind: every answer of `m` gets a turn before one branch of
   * `f` can monopolise the search */
  def fairBind[R <: Choose, A, B](m: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
    Eff.of(erase(okay.std.Logic.fairBind[A, B, C](lift(m))(a => lift(f(a)))))
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
