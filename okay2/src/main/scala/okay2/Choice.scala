package okay2

import Free.{Return, Inject, Bind}
import Split.split

/**
 * The nondeterminism effect: choose one of several values, and let
 * the handler explore every branch. The handler is MULTI-SHOT — it
 * invokes the captured continuation once per alternative, which is
 * delimited continuations doing what neither a relay (exactly-once by
 * parametricity) nor an exception-style handler can. The class IS the
 * whole identity: `Choose` has no parameter but its erased answer, so
 * splitting a row on it is a TOTAL test.
 *
 * Not here: the Scala 3 core's `runSeq`, where a collection IS the
 * signature (`A ! List`) — a row here is a type of kind `*` with an
 * `Op` member, and `List` is not one; and `MonadPlus`/`guard` with the
 * `withFilter` that lets a `for` prune by an `if` — okay2 has no
 * `Monad`; `Choose.guard(p)` is the explicit spelling.
 */
sealed trait Choose extends Row { type Op[+A] = Choose.Op[A] }

object Choose {
  /** one of the alternatives; none is failure (the branch is pruned) */
  final case class Op[+A](as: Seq[A])

  implicit val effect: Effect[Choose] = Effect.of[Choose]

  /** one of the given alternatives */
  def choose[A](as: A*): A ! Choose = Free.inject[Choose, A](Op(as))

  /** no alternatives: the branch dies */
  def fail[A]: A ! Choose = Free.inject[Choose, A](Op(Seq.empty))

  /** keep the branch exactly when `p` holds — `if` in a `for`, spelled
   * as a step */
  def guard(p: Boolean): Unit ! Choose = if (p) pure[Choose, Unit](()) else fail[Unit]

  /** all the results of all the branches, for a program whose row
   * mentions `Choose` anywhere */
  def runChoice[A, R <: Row](a: A ! R)(implicit rm: Remove[Choose, R]): Seq[A] ! rm.Out =
    runChoiceAt[A, rm.Out](rm.split(a))

  /** `runChoice` at the handler's own shape: a Cont-valued handler that
   * resumes the continuation once per alternative and appends */
  def runChoiceAt[A, F <: Row](a: A ! (Choose + F)): Seq[A] ! F =
    Effects.handle[A, Seq[A], Choose, F](a)(x => pure[F, Seq[A]](Seq(x)))(
      new Interpr[Choose, Seq[A] ! F] {
        def apply[X](c: Op[X]): Cont[X, Seq[A] ! F, Seq[A] ! F] =
          shift[X, Seq[A] ! F, Seq[A] ! F] { k =>
            c.as.foldLeft(pure[F, Seq[A]](Seq.empty))((acc, x) => acc.flatMap(s => k(x).map(s ++ _)))
          }
      })
}

/**
 * Backtracking search over the nondeterminism effect — LogicT
 * (Kiselyov, Shan, Friedman, Sabry 2005) rebuilt on Choose. The one
 * primitive is `msplit`: the FIRST answer and a program producing the
 * rest. Everything else derives: `cut`, `ifte` (the soft cut), `gnot`
 * (negation as failure), `interleave` (the FAIR or), `fairBind`,
 * `observe` (the first n answers of a possibly infinite search).
 * Alternatives are a Seq, and a LazyList IS a Seq — so infinite choice
 * points cost nothing to construct, and fairness makes them
 * searchable. Search-state effects F forward: an operation met on a
 * branch's path runs when the search first crosses it.
 */
object Logic {

  /** all of it at once, on a lazy stream: alternatives explored
   * depth-first, left to right */
  private def alts[A, F <: Row](ps: Seq[A ! (Choose + F)]): A ! (Choose + F) =
    Free.inject[Choose, A ! (Choose + F)](Choose.Op(ps)).plus[F].flatMap(identity)

  /** construction must do NO work: recursive search combinators hide
   * behind a unit bind */
  private def defer[A, F <: Row](p: => A ! (Choose + F)): A ! (Choose + F) =
    pure[Choose + F, Unit](()).flatMap(_ => p)

  private def none[A, F <: Row]: A ! (Choose + F) = Free.inject[Choose, A](Choose.Op(Seq.empty)).plus[F]

  /**
   * The primitive: the first answer with the rest-of-the-search as a
   * program, or None — the search is empty. Depth-first, left to
   * right; F-operations on the way forward and run once, when crossed.
   * The worklist is a LazyList: infinite choice points stay unforced.
   */
  def msplit[A, F <: Row](m: A ! (Choose + F)): Option[(A, A ! (Choose + F))] ! F = {
    type P = A ! (Choose + F)
    def go(stack: LazyList[P]): Option[(A, P)] ! F = stack match {
      case p #:: rest => Free.resume(p) match {
        case Return(a) => pure[F, Option[(A, P)]](Some((a, alts[A, F](rest))))
        case Inject(e) => go(Bind(Inject[Choose + F, A](e), (x: A) => Return[Choose + F, A](x)) #:: rest)
        case Bind(Inject(e), k) =>
          split[Choose, F, Any, Option[(A, P)] ! F](e) { c =>
            go(c.as.to(LazyList).map(x => k(x)) #::: rest)
          } { g => Inject[F, Any](g).flatMap(x => go(k(x) #:: rest)) }
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }
      case _ => pure[F, Option[(A, P)]](None)   // empty: the search is exhausted
    }
    go(LazyList(m))
  }

  /** at most one answer: the cut — commits to the first success and
   * throws the rest of the search away */
  def cut[A, F <: Row](m: A ! (Choose + F)): A ! (Choose + F) =
    msplit[A, F](m).plus[Choose].at[Choose + F].flatMap {
      case Some((a, _)) => pure[Choose + F, A](a)
      case None => none[A, F]
    }

  /** the soft cut: if cond has ANY answer, then th over ALL its
   * answers; el ONLY when cond has none */
  def ifte[A, B, F <: Row](cond: A ! (Choose + F))(th: A => B ! (Choose + F))(el: => B ! (Choose + F)): B ! (Choose + F) =
    msplit[A, F](cond).plus[Choose].at[Choose + F].flatMap {
      case Some((a, rest)) => alts[B, F](Seq(defer(th(a)), defer(rest.flatMap(th))))
      case None => el
    }

  /** negation as failure: succeeds (with unit) exactly when the search fails */
  def gnot[A, F <: Row](m: A ! (Choose + F)): Unit ! (Choose + F) =
    ifte[A, Unit, F](m)(_ => none[Unit, F])(pure[Choose + F, Unit](()))

  /** the FAIR or: answers of a and b take turns — an infinite a cannot
   * starve b */
  def interleave[A, F <: Row](a: A ! (Choose + F), b: => A ! (Choose + F)): A ! (Choose + F) =
    msplit[A, F](a).plus[Choose].at[Choose + F].flatMap {
      case Some((x, rest)) => alts[A, F](Seq(pure[Choose + F, A](x), defer(interleave(b, rest))))
      case None => b
    }

  /** the FAIR bind: each answer of m gets a turn before any single
   * f-branch monopolizes the search */
  def fairBind[A, B, F <: Row](m: A ! (Choose + F))(f: A => B ! (Choose + F)): B ! (Choose + F) =
    msplit[A, F](m).plus[Choose].at[Choose + F].flatMap {
      case Some((a, rest)) => interleave(f(a), fairBind(rest)(f))
      case None => none[B, F]
    }

  /** the first n answers (a possibly infinite search stays lazy) */
  def observe[A, F <: Row](n: Int)(m: A ! (Choose + F)): Seq[A] ! F =
    if (n <= 0) pure[F, Seq[A]](Seq.empty)
    else msplit[A, F](m).flatMap {
      case Some((a, rest)) => observe[A, F](n - 1)(rest).map(a +: _)
      case None => pure[F, Seq[A]](Seq.empty)
    }
}
