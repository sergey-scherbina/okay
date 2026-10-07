package okay.freer

import okay.*

import okay.freer.!.*
import scala.annotation.tailrec

/**
 * Backtracking search over the nondeterminism effect — LogicT
 * (Kiselyov, Shan, Friedman, Sabry 2005) rebuilt on Choose. The one
 * primitive is msplit: the FIRST answer and a program producing the
 * rest. Everything else derives: cut (the one that keeps one
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
  private def alts[A, F[+_]](ps: Seq[A ! Choose + F]): A ! Choose + F =
    effect[Choose + F, A ! Choose + F](Choose(ps)).flatMap(identity)

  /** construction must do NO work (the laziness contract): recursive
   * search combinators hide behind a unit bind */
  private def defer[A, F[+_]](p: => A ! Choose + F): A ! Choose + F =
    pure(()).flatMap(_ => p)

  /**
   * The primitive: the first answer with the rest-of-the-search as a
   * program, or None — the search is empty. Depth-first, left to
   * right; F-operations on the way forward and run once, when
   * crossed. The worklist is a LazyList: infinite choice points
   * (Choose over a LazyList of alternatives) stay unforced.
   */
  def msplit[A, F[+_]](m: A ! Choose + F)
  : Option[(A, A ! Choose + F)] ! F =
    type O = Option[(A, A ! Choose + F)]
    // the search as a frame (handle-frames-catch): the continuation run at each alternative in turn, the first
    // answer found the answer, the alternatives not yet run handed out as programs pending on that continuation
    def frame(x: A ! Choose + F): Shift.U[F, O] =
      HandleFrames.handling[A, O, F]("msplit", summon[TypeableK[Choose]].test, a => pure(Some((a, alts(Nil)))))(
        (op, k) =>
          val c = op.asInstanceOf[Choose[Any]]
          // a branch point something is shared through (logic-cut-releases): a throw out of the search abandons it
          Forked.sharedOf(c.as) match
            case Nil => first(c.as.to(LazyList), k, Nil)
            case held => HandleFrames.unwinding[O, F](() => held.foreach(_.abandon()))(first(c.as.to(LazyList), k, held)))(x)
    def first(as: LazyList[Any], k: Any => O ! F, held: List[Shared]): O ! F = as match
      case LazyList() => pure(None)
      case b #:: more => k(b).flatMap:
        case None => again1(more, k, held)
        case Some((a, r)) => pure(Some((a, restOf(r #:: more.map(c => defer(reflect[A, F](HandleFrames.pending[O, F](
          k(c).asInstanceOf[Shift.U[F, O]])))), sharedIn(r) ::: held))))
    // the next alternative from inside flatMap: on a machine, a step of its loop, not a host frame
    def again1(as: LazyList[Any], k: Any => O ! F, held: List[Shared]): O ! F = first(as, k, held)
    // a forwarded operation resumes the walk from inside flatMap, a call
    // that cannot be a jump; `again` takes it, so `go` stays a checked loop
    def again(d: Int)(stack: LazyList[A ! Choose + F], held: List[Shared]): O ! F = go(d)(stack, held)
    // `held`: the branch points something is shared through that this search has met (logic-cut-releases) — handed
    // out with the rest, and abandoned when a throw leaves the search
    @tailrec def go(d: Int)(stack0: LazyList[A ! Choose + F], held: List[Shared]): O ! F =
      val stack = if held.isEmpty then stack0 else unwinding(held)({ val _ = stack0.isEmpty; stack0 })
      stack match
        case LazyList() => pure(None)
        case p #:: rest => ((if held.isEmpty then p.resumeRun else unwinding(held)(p.resumeRun)): @unchecked) match
          case Return(a) => pure(Some((a, restOf(rest, held))))
          case i @ Inject(e) => split[Choose, F](e)
            (c => go(d)(c.as.to(LazyList).map(a => Return(a): A ! Choose + F) #::: rest, Forked.sharedOf(c.as) ::: held))
            (_ => forwarded[Choose, F](i).flatMap(a => again(d)(Return(a) #:: rest, held)))
          case Bind(i @ Inject(e), k) => split[Choose, F](e)
            (c => go(d)(c.as.to(LazyList).map(x => k(x)) #::: rest, Forked.sharedOf(c.as) ::: held))
            (_ => forwarded[Choose, F](i).flatMap(x => again(d)(k(x) #:: rest, held)))
          // a nested run at the head (an inner search, a handler): forced, as its fold below the limit
          case y => go(d)(HandleFrames.shallow(y, d) #:: rest, held)

    HandleFrames.run[O, F](d => go(d)(LazyList(m), Nil), frame(m))

  /** `body` — the walk forcing user code — with the branch points `held` abandoned when it throws */
  private inline def unwinding[T](held: List[Shared])(inline body: T): T =
    try body
    catch case t: Throwable => { held.foreach(_.abandon()); throw t }

  /** a split's rest: the alternatives left, with the branch points they still hold (logic-cut-releases) */
  private def restOf[A, F[+_]](rest: Seq[A ! Choose + F], held: List[Shared]): A ! Choose + F =
    if held.isEmpty then alts(rest) else alts(Forked(rest, Shared.Group(held)))

  /** the branch points a rest `msplit` handed out holds */
  private def sharedIn[A, F[+_]](rest: A ! Choose + F): List[Shared] = rest match
    case Bind(Inject(c: Choose[?]), _) => Forked.sharedOf(c.as)
    case _ => Nil

  /**
   * THE REST OF A SEARCH DROPPED (logic-cut-releases): no alternative in `rest` — a rest `msplit` handed out — will
   * be started, so what they share with the branches already run is released once those are done. `cut` and
   * `observe` say it themselves; a combinator of one's own that drops a rest says it here.
   */
  def abandon[A, F[+_]](rest: A ! Choose + F): Unit = sharedIn(rest).foreach(_.abandon())

  /** a split back into a search: its answer, then the rest */
  private def reflect[A, F[+_]](o: Option[(A, A ! Choose + F)] ! F): A ! Choose + F =
    !.widen[Option[(A, A ! Choose + F)], F, Choose](o).flatMap:
      case None => alts(Nil)
      case Some((a, r)) => alts(Seq(pure(a), r))

  /** at most one answer: the cut — commits to the first success and
   * throws the rest of the search away. `once` until logic-cut
   * (2026-09-16): that word is `!.once` now, the by-need effect, and
   * a file importing both `!.*` and `Logic.*` had the two collide. */
  def cut[A, F[+_]](m: A ! Choose + F): A ! Choose + F =
    !.widen[Option[(A, A ! Choose + F)], F, Choose](msplit(m)).flatMap:
      case Some((a, rest)) => abandon(rest); pure(a)
      case None => effect(Choose(Seq.empty))

  /** the soft cut: if cond has ANY answer, then th over ALL its
   * answers; el ONLY when cond has none. (A plain flatMap cannot say
   * "no answer"; an ordinary cut would lose cond's other answers.) */
  def ifte[A, B, F[+_]](cond: A ! Choose + F)
                                   (th: A => B ! Choose + F)
                                   (el: => B ! Choose + F): B ! Choose + F =
    !.widen[Option[(A, A ! Choose + F)], F, Choose](msplit(cond)).flatMap:
      case Some((a, rest)) => alts(Seq(defer(th(a)), defer(rest.flatMap(th))))
      case None => el

  /** negation as failure: succeeds (with unit) exactly when the
   * search fails */
  def gnot[A, F[+_]](m: A ! Choose + F): Unit ! Choose + F =
    ifte(m)(_ => effect(Choose(Seq.empty)))(pure(()))

  /** the FAIR or: answers of a and b take turns — an infinite a
   * cannot starve b */
  def interleave[A, F[+_] : TypeableK](a: A ! Choose + F, b: => A ! Choose + F)
  : A ! Choose + F =
    !.widen[Option[(A, A ! Choose + F)], F, Choose](msplit(a)).flatMap:
      case Some((x, rest)) => alts(Seq(pure(x), defer(interleave(b, rest))))
      case None => b

  /** the FAIR bind: each answer of m gets a turn before any single
   * f-branch monopolizes the search */
  def fairBind[A, B, F[+_] : TypeableK](m: A ! Choose + F)
                                       (f: A => B ! Choose + F): B ! Choose + F =
    !.widen[Option[(A, A ! Choose + F)], F, Choose](msplit(m)).flatMap:
      case Some((a, rest)) => interleave(f(a), fairBind(rest)(f))
      case None => effect(Choose(Seq.empty))

  extension [A, F[+_]](m: A ! Choose + F)
    /** fairBind as an operator, LogicT's spelling */
    inline def >>-[B](f: A => B ! Choose + F)(using TypeableK[F]): B ! Choose + F =
      fairBind(m)(f)

  /** the first n answers (a possibly infinite search stays lazy) */
  def observe[A, F[+_] : TypeableK](n: Int)(m: A ! Choose + F): Seq[A] ! F =
    // none more wanted: what is left of the search is dropped, and says so (logic-cut-releases)
    if n <= 0 then pure(()).map(_ => { abandon(m); Seq.empty })
    else msplit(m).flatMap:
      case Some((a, rest)) => observe(n - 1)(rest).map(a +: _)
      case None => pure(Seq.empty)
}
