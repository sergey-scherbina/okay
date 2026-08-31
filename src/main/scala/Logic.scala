package okay

import okay.!.*

/**
 * Backtracking search over the nondeterminism effect — LogicT
 * (Kiselyov, Shan, Friedman, Sabry 2005) rebuilt on Choose. The one
 * primitive is msplit: the FIRST answer and a program producing the
 * rest. Everything else derives: once (the cut that keeps one
 * answer), ifte (the soft cut — else runs only when there is NO
 * answer, negation-as-failure in one line), interleave (the FAIR or
 * — two infinite branches take turns), >>- (the fair bind — a
 * productive branch cannot starve its siblings), observe (the first
 * n answers of a possibly infinite search).
 *
 * Alternatives are a Seq, and a LazyList IS a Seq — so infinite
 * choice points cost nothing to construct, and fairness is what
 * makes them searchable. Search-state effects F forward: an
 * operation met on a branch's path runs when the search first
 * crosses it.
 */
object Logic {

  /** all of it at once, on a lazy stream: alternatives explored
   * depth-first, left to right */
  private def alts[A, F[+_]](ps: Seq[A ! (Choose + F)]): A ! (Choose + F) =
    effect[Choose + F, A ! (Choose + F)](Choose(ps)).flatMap(identity)

  /** construction must do NO work (the laziness contract): recursive
   * search combinators hide behind a unit bind */
  private def defer[A, F[+_]](p: => A ! (Choose + F)): A ! (Choose + F) =
    pure(()).flatMap(_ => p)

  /**
   * The primitive: the first answer with the rest-of-the-search as a
   * program, or None — the search is empty. Depth-first, left to
   * right; F-operations on the way forward and run once, when
   * crossed. The worklist is a LazyList: infinite choice points
   * (Choose over a LazyList of alternatives) stay unforced.
   */
  def msplit[A, F[+_]](m: A ! (Choose + F))
  : Option[(A, A ! (Choose + F))] ! F =
    def go(stack: LazyList[A ! (Choose + F)]): Option[(A, A ! (Choose + F))] ! F =
      stack match
        case LazyList() => pure(None)
        case p #:: rest => (p.resume: @unchecked) match
          case Pure(a) => pure(Some((a, alts(rest))))
          case Effect(e) => <|>[Choose, F](e) match
            case Left(c) => go(c.as.to(LazyList).map(a => Pure(a): A ! (Choose + F)) #::: rest)
            case Right(g) => Effect(g).flatMap(a => go(Pure(a) #:: rest))
          case Bind(Effect(e), k) => <|>[Choose, F](e) match
            case Left(c) => go(c.as.to(LazyList).map(x => k(x)) #::: rest)
            case Right(g) => Effect(g).flatMap(x => go(k(x) #:: rest))

    go(LazyList(m))

  /** at most one answer: the cut — commits to the first success and
   * throws the rest of the search away */
  def once[A, F[+_]](m: A ! (Choose + F)): A ! (Choose + F) =
    !.widen[Option[(A, A ! (Choose + F))], F, Choose](msplit(m)).flatMap:
      case Some((a, _)) => pure(a)
      case None => effect(Choose(Seq.empty))

  /** the soft cut: if cond has ANY answer, then th over ALL its
   * answers; el ONLY when cond has none. (A plain flatMap cannot say
   * "no answer"; an ordinary cut would lose cond's other answers.) */
  def ifte[A, B, F[+_]](cond: A ! (Choose + F))
                                   (th: A => B ! (Choose + F))
                                   (el: => B ! (Choose + F)): B ! (Choose + F) =
    !.widen[Option[(A, A ! (Choose + F))], F, Choose](msplit(cond)).flatMap:
      case Some((a, rest)) => alts(Seq(defer(th(a)), defer(rest.flatMap(th))))
      case None => el

  /** negation as failure: succeeds (with unit) exactly when the
   * search fails */
  def gnot[A, F[+_]](m: A ! (Choose + F)): Unit ! (Choose + F) =
    ifte(m)(_ => effect(Choose(Seq.empty)))(pure(()))

  /** the FAIR or: answers of a and b take turns — an infinite a
   * cannot starve b */
  def interleave[A, F[+_] : TypeableK](a: A ! (Choose + F), b: => A ! (Choose + F))
  : A ! (Choose + F) =
    !.widen[Option[(A, A ! (Choose + F))], F, Choose](msplit(a)).flatMap:
      case Some((x, rest)) => alts(Seq(pure(x), defer(interleave(b, rest))))
      case None => b

  /** the FAIR bind: each answer of m gets a turn before any single
   * f-branch monopolizes the search */
  def fairBind[A, B, F[+_] : TypeableK](m: A ! (Choose + F))
                                       (f: A => B ! (Choose + F)): B ! (Choose + F) =
    !.widen[Option[(A, A ! (Choose + F))], F, Choose](msplit(m)).flatMap:
      case Some((a, rest)) => interleave(f(a), fairBind(rest)(f))
      case None => effect(Choose(Seq.empty))

  extension [A, F[+_]](m: A ! (Choose + F))
    /** fairBind as an operator, LogicT's spelling */
    inline def >>-[B](f: A => B ! (Choose + F))(using TypeableK[F]): B ! (Choose + F) =
      fairBind(m)(f)

  /** the first n answers (a possibly infinite search stays lazy) */
  def observe[A, F[+_] : TypeableK](n: Int)(m: A ! (Choose + F)): Seq[A] ! F =
    if n <= 0 then pure(Seq.empty)
    else msplit(m).flatMap:
      case Some((a, rest)) => observe(n - 1)(rest).map(a +: _)
      case None => pure(Seq.empty)
}
