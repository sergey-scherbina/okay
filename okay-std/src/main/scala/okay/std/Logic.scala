package okay.std

import okay.{!, Run, Within, defer, effect, pure}
import okay.core.Split
import okay.one.Cont
import okay.one.row.{In, Pure as OnePure}
import okay.one.machine.Machine
import okay.one.machine.Machine.Head
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
 * makes them searchable. Search-state effects (the rest of the
 * program's, besides `Choose`) forward: an operation met on a branch's
 * path runs when the search first crosses it.
 */
object Logic {

  /** all of it at once, on a lazy stream: alternatives explored
   * depth-first, left to right */
  private def alts[A, Es[_]](ps: Seq[A ! Es])(using c: Within[Choose, Es]): A ! Es =
    effect[Choose, A ! Es](Choose(ps)).at[Es].flatMap(identity)

  /** no answer: the branch dies */
  private def none[A, Es[_]](using Within[Choose, Es]): A ! Es = effect[Choose, A](Choose(Seq.empty)).at[Es]

  /**
   * The primitive: the first answer with the rest-of-the-search as a
   * program, or None — the search is empty. Depth-first, left to
   * right; the other effects' operations on the way forward and run
   * once, when crossed. The alternatives are a LazyList: infinite
   * choice points (Choose over a LazyList of alternatives) stay
   * unforced.
   *
   * THE SEARCH STEPPED (the kernel's `Machine.Head`): the program is
   * run to its next operation and handed over paused, its rest `k` the
   * machine's own. A `Choose` puts its alternatives, the paused program
   * resumed with each (a paused program resumes any number of times),
   * at the front of the worklist; another effect's operation is
   * performed at the place the split runs and the program resumed with
   * the answer;
   * the first value is the answer, and THE WORKLIST LEFT IS THE REST —
   * one value, the remaining search, resumed where it stopped (`Rest`).
   * Split again at the same place, the rest is that worklist taken up
   * again: nothing re-run, no layer of reflection per answer — linear
   * in the work of the search, as LogicT's msplit.
   *
   * WHERE A REST RUNS. A rest is the search's `k`s at the place
   * `msplit` ran. Split again at the same paths (`observe`,
   * `interleave`, `fairBind`) it goes on from where it was; run
   * anywhere else (`runChoice` of it, among the program's alternatives,
   * under more handlers) each of its operations is performed at the
   * place of the same effect there, the two places' paths read side by
   * side (`Stepping.relocate`). An effect of it not among those of the
   * place is refused loudly (`IllegalStateException`).
   */
  def msplit[A, Es[_]](m: A ! Es)(using s: Split[Es, Choose]): Option[(A, A ! Es)] ! s.Rest =
    Run.of(Cont.defer(Run.kernel[A, Es, OnePure](m) match
      // unchecked: a `Rest` that is this `A ! Es` has its `A`
      case Cont.Defer(r: Rest[A] @unchecked) => new Search[A, Es].next(r.branches, r.held)
      case e => new Search[A, Es].next(LazyList(() => Machine.head(e)), Nil)))

  /** a pending branch of a search: its `k` resumed, stepped to its next operation */
  private type Branch[A] = () => Head[OnePure, A]

  /** the key of `Choose`: its constructor's */
  private val chooseKey: okay.KeyOf[Choose] = summon[okay.KeyOf[Choose]]

  /**
   * THE REST OF A SEARCH: the branches not yet run of a split — data, a worklist — and the branch points they still
   * hold (logic-cut-releases). Split again, the worklist is taken up again (`msplit`); run anywhere else, it is a
   * `Choose` of its branches, each stepped on with its operations performed again at their places (by key).
   */
  private final class Rest[A](val branches: LazyList[Branch[A]], val held: List[Shared])
      extends (() => okay.one.![A, OnePure]):
    def apply(): okay.one.![A, OnePure] =
      val as: Seq[A ! okay.Pure] = branches.map(b => Stepping.Rest[okay.Pure, A](b))
      val forked = if held.isEmpty then as else Forked(as, Shared.Group(held))
      Cont.perform[Choose, OnePure, A ! okay.Pure](Choose(forked))(using chooseKey.in).flatMap(q => Run.kernel(q))

  private def rest[A, Es[_]](bs: LazyList[Branch[A]], held: List[Shared]): A ! Es = Run.of(Cont.Defer(Rest(bs, held)))

  /** a split: the worklist walked to the first answer */
  private final class Search[A, Es[_]]:
    type O = Option[(A, A ! Es)]

    /** the next branch, or none: the search is empty */
    def next(bs: LazyList[Branch[A]], held: List[Shared]): okay.one.![O, OnePure] = bs match
      case LazyList() => Cont.pure(None)
      case b #:: more => walk(b(), more, held)

    // a forwarded operation resumes the walk from inside flatMap, a call that cannot be a jump; `again` takes it
    private def again(st: Head[OnePure, A], bs: LazyList[Branch[A]], held: List[Shared]): okay.one.![O, OnePure] =
      walk(st, bs, held)

    @tailrec private def walk(st: Head[OnePure, A], bs: LazyList[Branch[A]], held: List[Shared]): okay.one.![O, OnePure] =
      st match
        case Head.Done(a) => Cont.pure(Some((a, rest[A, Es](bs, held))))
        case p: Head.Paused[?, ?, f, x, ?, ?, ?, ?, ?] =>
          if !Stepping.own(p.in, chooseKey.ref) then
            Cont.perform[f, OnePure, x](p.op)(using Stepping.placed(p.in)).flatMap(v => again(p.resume(v), bs, held))
          else
            val c = p.op.asInstanceOf[Choose[Any]]
            val front = c.as.to(LazyList).map(v => () => Stepping.resume(p, v))
            Forked.sharedOf(c.as) match
              case Nil => front #::: bs match
                case LazyList() => Cont.pure(None)
                case b #:: more => walk(b(), more, held)
              // a branch point something is shared through (logic-cut-releases): a throw out of the search abandons it
              case shared =>
                Cont.recover(Cont.defer(next(front #::: bs, shared ::: held)))(t => { shared.foreach(_.abandon()); throw t })

  /** the branch points a rest `msplit` handed out holds */
  private def sharedIn[A, Es[_]](rest: A ! Es): List[Shared] = Run.kernel[A, Es, OnePure](rest) match
    case Cont.Defer(r: Rest[?]) => r.held
    case _ => Nil

  /**
   * THE REST OF A SEARCH DROPPED (logic-cut-releases): no alternative in `rest` — a rest `msplit` handed out — will
   * be started, so what they share with the branches already run is released once those are done. `cut` and
   * `observe` say it themselves; a combinator of one's own that drops a rest says it here.
   */
  def abandon[A, Es[_]](rest: A ! Es): Unit = sharedIn(rest).foreach(_.abandon())

  /** at most one answer: the cut — commits to the first success and
   * throws the rest of the search away. `once` until logic-cut
   * (2026-09-16): that word is `!.once` now, the by-need effect, and
   * a file importing both `!.*` and `Logic.*` had the two collide. */
  def cut[A, Es[_]](m: A ! Es)(using s: Split[Es, Choose], w: Within[s.Rest, Es], c: Within[Choose, Es]): A ! Es =
    msplit(m).at[Es].flatMap:
      case Some((a, rest)) => abandon(rest); pure[A, Es](a)
      case None => none[A, Es]

  /** the soft cut: if cond has ANY answer, then th over ALL its
   * answers; el ONLY when cond has none. (A plain flatMap cannot say
   * "no answer"; an ordinary cut would lose cond's other answers.) */
  def ifte[A, B, Es[_]](cond: A ! Es)
                       (th: A => B ! Es)
                       (el: => B ! Es)
                       (using s: Split[Es, Choose], w: Within[s.Rest, Es], c: Within[Choose, Es]): B ! Es =
    msplit(cond).at[Es].flatMap:
      case Some((a, rest)) => alts[B, Es](Seq(defer(th(a)), defer(rest.flatMap(th))))
      case None => el

  /** negation as failure: succeeds (with unit) exactly when the
   * search fails */
  def gnot[A, Es[_]](m: A ! Es)(using s: Split[Es, Choose], w: Within[s.Rest, Es], c: Within[Choose, Es]): Unit ! Es =
    ifte[A, Unit, Es](m)(_ => none[Unit, Es])(pure[Unit, Es](()))

  /** the FAIR or: answers of a and b take turns — an infinite a
   * cannot starve b */
  def interleave[A, Es[_]](a: A ! Es, b: => A ! Es)
                          (using s: Split[Es, Choose], w: Within[s.Rest, Es], c: Within[Choose, Es]): A ! Es =
    msplit(a).at[Es].flatMap:
      case Some((x, rest)) => alts[A, Es](Seq(pure[A, Es](x), defer(interleave(b, rest))))
      case None => b

  /** the FAIR bind: each answer of m gets a turn before any single
   * f-branch monopolizes the search */
  def fairBind[A, B, Es[_]](m: A ! Es)(f: A => B ! Es)
                           (using s: Split[Es, Choose], w: Within[s.Rest, Es], c: Within[Choose, Es]): B ! Es =
    msplit(m).at[Es].flatMap:
      case Some((a, rest)) => interleave(f(a), fairBind(rest)(f))
      case None => none[B, Es]

  extension [A, Es[_]](m: A ! Es)
    /** fairBind as an operator, LogicT's spelling */
    inline def >>-[B](f: A => B ! Es)(using s: Split[Es, Choose], w: Within[s.Rest, Es], c: Within[Choose, Es]): B ! Es =
      fairBind(m)(f)

  /** the first n answers (a possibly infinite search stays lazy) */
  def observe[A, Es[_]](n: Int)(m: A ! Es)(using s: Split[Es, Choose]): Seq[A] ! s.Rest =
    // none more wanted: what is left of the search is dropped, and says so (logic-cut-releases)
    if n <= 0 then pure[Unit, s.Rest](()).map(_ => { abandon(m); Seq.empty })
    else msplit(m).flatMap:
      case Some((a, rest)) => observe(n - 1)(rest).map(a +: _)
      case None => pure[Seq[A], s.Rest](Seq.empty)
}
