package okay.std

import okay.{!, Run, Within, defer, effect, pure}
import okay.core.{+:, Eff, Paths, Req, Row, Split}
import okay.one.Cont
import okay.one.row.In
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
    new Alts[A, Es](ps, [R <: Row] => (ch: Paths[Es, R]) => c.paths(ch).find(Req.only))

  /** no answer: the branch dies */
  private def none[A, Es[_]](using Within[Choose, Es]): A ! Es = effect[Choose, A](Choose(Seq.empty)).at[Es]

  /**
   * ALTERNATIVES AS A PROGRAM: a `Choose` of `as` at its place, `choose` finding it, the one chosen run — a class,
   * so that the branch points the rest of a split holds can be read off it (`sharedIn`).
   */
  private final class Alts[A, Es[_]](val as: Seq[A ! Es], choose: [R <: Row] => Paths[Es, R] => In[Choose, R])
      extends Run[Es, A]:
    def on[R <: Row](ps: Paths[Es, R]): Eff[R, A] =
      Cont.perform[Choose, R, A ! Es](Choose(as))(using choose(ps)).flatMap(_.fit(ps))

  /**
   * The primitive: the first answer with the rest-of-the-search as a
   * program, or None — the search is empty. Depth-first, left to
   * right; the other effects' operations on the way forward and run
   * once, when crossed. The alternatives are a LazyList: infinite
   * choice points (Choose over a LazyList of alternatives) stay
   * unforced.
   *
   * A HANDLER OF `Choose` (the kernel's `Deep`): at a choice its rest
   * `k` is run at each alternative in turn, the first answer found the
   * answer, the alternatives not yet run handed out as programs pending
   * on that `k` — data, resumed when the rest of the search is.
   *
   * WHERE A REST RUNS. `k` is the search's rest at the place `msplit`
   * ran, its other effects' operations going to the handlers there by
   * their paths. A rest is run where the answer of the split is used —
   * among the program's alternatives, the same place — or under
   * handlers put over that place (a split of the rest, `observe`,
   * `interleave`, `runChoice` of it): there `k` is seen under them,
   * its operations past them. A rest moved anywhere else — out of the
   * scope of a handler of one of its effects, or under a new handler
   * of one of them — is refused loudly (`IllegalStateException`):
   * where its operations would go is not a place `k` has.
   */
  def msplit[A, Es[_]](m: A ! Es)(using s: Split[Es, Choose]): Option[(A, A ! Es)] ! s.Rest =
    new Run[s.Rest, Option[(A, A ! Es)]]:
      def on[T <: Row](ps: Paths[s.Rest, T]): Eff[T, Option[(A, A ! Es)]] =
        val at = new Searched[Es, T](s.paths(ps))
        Cont.handle(m.fit(at.template))(new Splitting[A, Es, T](at))

  /** the answer of a split: the first answer and the rest, or none */
  private type Split1[A, Es[_]] = Option[(A, A ! Es)]

  /** the handler of a split at the place `T` */
  private final class Splitting[A, Es[_], T <: Row](at: Searched[Es, T])
      extends okay.one.handler.Deep[Choose, T, A, Split1[A, Es]]:
    type O = Split1[A, Es]
    def ret(a: A): Eff[T, O] = Cont.pure(Some((a, at.alts(Nil))))
    def apply[X](op: Choose[X], k: X => Eff[T, O]): Eff[T, O] =
      // a branch point something is shared through (logic-cut-releases): a throw out of the search abandons it
      Forked.sharedOf(op.as) match
        case Nil => first(op.as.to(LazyList), k, Nil)
        case held => Cont.recover(first(op.as.to(LazyList), k, held))(t => { held.foreach(_.abandon()); throw t })
    // the next alternative from inside flatMap: on the machine, a step of its loop, not a host frame
    private def first[X](as: LazyList[X], k: X => Eff[T, O], held: List[Shared]): Eff[T, O] = as match
      case LazyList() => Cont.pure(None)
      case b #:: more => k(b).flatMap:
        case None => first(more, k, held)
        case Some((a, r)) => Cont.pure(Some((a, restOf(r #:: more.map(c => at.pending(k, c)), sharedIn(r) ::: held))))

    /** a split's rest: the alternatives left, with the branch points they still hold (logic-cut-releases) */
    private def restOf(rest: Seq[A ! Es], held: List[Shared]): A ! Es =
      if held.isEmpty then at.alts(rest) else at.alts(Forked(rest, Shared.Group(held)))

  /**
   * THE PLACE A SPLIT RAN AT, `T`, and its programs' places: `template`, the paths of the program's effects under
   * the split's handler — `Choose` at its head, the rest's as they were at `T`. A program of the split (a rest,
   * alternatives pending on its `k`) fitted at another place is put there by reading the two side by side: the
   * place of `Choose`, and how many handlers the rest's effects are under now, past those at `T`.
   */
  private final class Searched[Es[_], T <: Row](val template: Paths[Es, Choose +: T]):
    /** alternatives of this split, at whatever place they are fitted */
    def alts[A](as: Seq[A ! Es]): A ! Es = new Alts[A, Es](as, [R <: Row] => (ps: Paths[Es, R]) => place(ps)._1)

    /** the alternative `c` pending on `k`: run when the rest of the search gets to it — its split back into a
     * search, its answer, then the rest */
    def pending[A, X](k: X => Eff[T, Split1[A, Es]], c: X): A ! Es = new Run[Es, A]:
      def on[R <: Row](ps: Paths[Es, R]): Eff[R, A] = Cont.defer {
        val (choose, depth) = place(ps)
        Searched.under[R, Split1[A, Es]](k(c), depth).flatMap: o =>
          val next: Seq[A ! Es] = o match
            case None => Nil
            case Some((a, r)) => Seq(pure[A, Es](a), r)
          Cont.perform[Choose, R, A ! Es](Choose(next))(using choose).flatMap(_.fit(ps))
      }

    /** at the place `ps`: `Choose`'s place, and how many handlers more than at `T` the other effects are under */
    def place[R <: Row](ps: Paths[Es, R]): (In[Choose, R], Int) =
      var choose: In[Choose, R] | Null = null
      var depth = -1
      def walk[E1[_], E2[_]](t: Paths[E1, ?], p: Paths[E2, ?]): Unit = (t, p) match
        case (to: Paths.One[?, ?], po: Paths.One[?, ?]) => (to.head: Any) match
          case _: In.Here[?, ?] => choose = po.head.asInstanceOf[In[Choose, R]]
          case th: In.There[?, ?, ?] => Searched.over(th.in, po.head) match
            case -1 => Searched.moved()
            case d => if depth == -1 then depth = d else if depth != d then Searched.moved()
          case at => if at != po.head then Searched.moved()
        case (tb: Paths.Both[?, ?, ?], pb: Paths.Both[?, ?, ?]) => walk(tb.left, pb.left); walk(tb.right, pb.right)
        case (_: Paths.None[?], _: Paths.None[?]) => ()
        case _ => Searched.moved()
      walk(template, ps)
      (choose.nn, if depth == -1 then 0 else depth)

  private object Searched:
    /** how many handlers `now` is under past `was`: `now` is `was` with that many more steps; -1 if it is not */
    def over[E1[_], E2[_]](was: In[E1, ?], now: In[E2, ?]): Int =
      @tailrec def go[E3[_]](n: In[E3, ?], d: Int): Int =
        if n == was then d
        else n match
          case th: In.There[?, ?, ?] => go(th.in, d + 1)
          case _ => -1
      go(now, 0)

    def moved(): Nothing = throw IllegalStateException(
      "Logic: the rest of a split run where its effects are not as they were at the split, under more handlers — " +
        "out of the scope of a handler of one of them, or under a new handler of one of them; run the rest where " +
        "the split's answer is used")

    /** `p`, a program over the place of a split, under `depth` handlers more: its operations past them. Its row
     * read only through its operations' paths, which `place` checked */
    def under[R <: Row, A](p: Eff[?, A], depth: Int): Eff[R, A] =
      @tailrec def go(q: Eff[Row, A], d: Int): Eff[Row, A] =
        if d == 0 then q else go(Cont.widen[Choose, Row, A](q).asInstanceOf[Eff[Row, A]], d - 1)
      go(p.asInstanceOf[Eff[Row, A]], depth).asInstanceOf[Eff[R, A]]

  /** the branch points a rest `msplit` handed out holds */
  private def sharedIn[A, Es[_]](rest: A ! Es): List[Shared] = rest match
    case a: Alts[?, ?] => Forked.sharedOf(a.as)
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
