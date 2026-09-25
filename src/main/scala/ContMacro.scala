package okay

import scala.quoted.*

/**
 * `shift`'s compile-time layer (specs/cont-stack.md Layer 1 A, plan
 * stage B). A body whose every use of its continuation `k` is a TAIL
 * call `k(v)`, with `v` not mentioning `k`, has nothing left to do
 * after the call: it IS `pure(v)`, computed when the runner reaches the
 * shift. So it is rewritten to exactly that, `Cont.tailShift(() => v)`
 * (a `Delay` the runner's loop walks), or to `Cont.tailPure(v)` when `v` is
 * a literal or a stable name and there are no statements before it.
 * No leaf, no `Reentry`, no nested frame, no room counted, no switch.
 *
 * Tail positions are followed through a block's result, both branches
 * of an `if`, every case of a `match`, an ascription and an inlined
 * call's expansion. Anything else — `k` in a statement, a condition, a
 * scrutinee, a guard, a nested lambda, an argument, under `try` (whose
 * `finally` would run BEFORE the rest instead of after), a branch that
 * answers without calling `k` — leaves the body as it was:
 * `Free.Inject(Shift.of(f))`, the runtime layer's case, byte for byte
 * the tree `shift` built before this macro existed.
 *
 * LAYER 1 B (cont-stack-layer1-b, plan stage E): a body that USES the
 * answer — `k(1) + k(10)`, `a :: k(x)`, `s"${k(a)}"`, `PState`'s
 * `s => k(s)(s2)` — is CPS-transformed SELECTIVELY (Rompf, Maier &
 * Odersky, ICFP 2009) into a `Cont.Body`: each `k(e)` becomes a `Call`
 * naming what is left, an application of the answer (`k(a)(s2)`) an
 * `Ap`, a lambda answer that calls `k` a `Fun`; the runner walks the
 * body in its own loop (Cont.scala, `step`'s pending stack). What ran
 * before a call still runs before it: every k-free part evaluated
 * ahead of a call is bound to a val first (A-normal form), unless it
 * is a literal, a stable name or a lambda. The transform follows a
 * block's statements and result, an `if` or `match` in tail
 * position, an application's function part and arguments in order,
 * an ascription, an inlined expansion; a by-name argument is left
 * as it is. Where `k` flows anywhere else — into a by-name argument,
 * a conditional that is not in tail position, a lambda that is not
 * the whole answer, a `try`, a loop, a value position (`xs.map(k)`) —
 * the body stays opaque, as before, and Layer 2 keeps it safe. The
 * rest of Layer 1 B (known higher-order functions, visible user
 * functions, `direct`) is backlog cont-stack-layer1-c.
 *
 * Public because an expansion at a user's call site calls it (the
 * same standing as `Distinct.impl`); not an API.
 */
object ContMacro:

  /** the transform's "cannot read this" — caught once, at the top */
  private object Opaque extends scala.util.control.ControlThrowable

  def shift[A: Type, S: Type, R: Type](f: Expr[(A => S) => R])(using q: Quotes): Expr[Cont[A, S, R]] =
    import q.reflect.*

    def fallback: Expr[Cont[A, S, R]] = '{ Cont.shiftLeaf[A, S, R]($f) }

    /** does `t` mention `k` anywhere */
    def mentions(k: Symbol, t: Tree): Boolean =
      new TreeAccumulator[Boolean]:
        def foldTree(found: Boolean, tree: Tree)(owner: Symbol): Boolean =
          found || (tree match
            case i: Ident if i.symbol == k => true
            case _ => foldOverTree(false, tree)(owner))
      .foldTree(false, t)(Symbol.spliceOwner)

    /** `k(v)` / `k.apply(v)`, with `v` free of `k` */
    object TailCall:
      def unapply(t: Term)(using k: Symbol): Option[Term] = t match
        case Apply(Select(i: Ident, "apply"), List(v)) if i.symbol == k && !mentions(k, v) => Some(v)
        case Apply(i: Ident, List(v)) if i.symbol == k && !mentions(k, v) => Some(v)
        case _ => None

    /** the body with every tail `k(v)` replaced by `v`, or None when the
     * body is not tail-shaped. `throw` in a tail position is kept: it
     * answers nothing, so it needs no `k`. */
    def rewrite(t: Term)(using k: Symbol): Option[Term] = t match
      case TailCall(v) => Some(v)
      case Inlined(call, bindings, e) if !bindings.exists(mentions(k, _)) =>
        rewrite(e).map(Inlined.copy(t)(call, bindings, _))
      case Typed(e, _) => rewrite(e)
      case Block(stats, e) if !stats.exists(mentions(k, _)) =>
        rewrite(e).map(Block.copy(t)(stats, _))
      case If(c, a, b) if !mentions(k, c) =>
        for a2 <- rewrite(a); b2 <- rewrite(b) yield If.copy(t)(c, a2, b2)
      case Match(s, cases) if !mentions(k, s) && !cases.exists(c => c.guard.exists(mentions(k, _))) =>
        val rhs = cases.map(c => rewrite(c.rhs))
        if rhs.forall(_.isDefined) then
          Some(Match.copy(t)(s, cases.zip(rhs).map((c, r) => CaseDef.copy(c)(c.pattern, c.guard, r.get))))
        else None
      case _ if t.tpe <:< TypeRepr.of[Nothing] && !mentions(k, t) => Some(t) // a throw
      case _ => None

    /** a value that can be read eagerly without changing when anything
     * happens: a literal, or a stable name */
    def eager(t: Term): Boolean = t match
      case Literal(_) => true
      case i: Ident => i.symbol.isValDef && !i.symbol.flags.is(Flags.Mutable) && !i.symbol.flags.is(Flags.Lazy)
      case Inlined(_, Nil, e) => eager(e)
      case Typed(e, _) => eager(e)
      case _ => false

    // ---- Layer 1 B: the selective CPS transform ----

    val fn1 = defn.FunctionClass(1)
    /** `X => Y`'s two types */
    def function1(t: TypeRepr): Option[(TypeRepr, TypeRepr)] = t.dealias.widen match
      case AppliedType(tc, List(x, y)) if tc.typeSymbol == fn1 => Some((x, y))
      case _ => None

    /** what is left to do with a value: `Done` at answer type `rt`
     * (the tail), or a rest that builds the body from it */
    case class Kont(rt: TypeRepr, rest: Option[Term => Term])

    def done(rt: TypeRepr, v: Term): Term = rt.asType match
      case '[r] => '{ Cont.Body.Done[r](${ v.asExprOf[r] }) }.asTerm

    def feed(kont: Kont, v: Term): Term = kont.rest match
      case None => done(kont.rt, v)
      case Some(f) => f(v)

    /** a fresh `name => rest(name)` of type `in => Body[rt]` */
    def lam(name: String, in: TypeRepr, rt: TypeRepr)(rest: Term => Term): Term =
      val out = rt.asType match
        case '[r] => TypeRepr.of[Cont.Body[r]]
      Lambda(Symbol.spliceOwner, MethodType(List(name))(_ => List(in), _ => out),
        (meth, ps) => rest(Ref(ps.head.symbol)).changeOwner(meth))

    /** `t` with every `from` replaced by `to` */
    def subst(t: Term, from: Symbol, to: Term): Term =
      new TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
          case i: Ident if i.symbol == from => to
          case _ => super.transformTerm(tree)(owner)
      .transformTerm(t)(Symbol.spliceOwner)

    /** a stable, effect-free term: evaluating it has no order to keep */
    def trivial(t: Term): Boolean = t match
      case Literal(_) | This(_) | New(_) | Super(_, _) => true
      case i: Ident =>
        val fl = i.symbol.flags
        !(fl.is(Flags.Method) || fl.is(Flags.Mutable) || fl.is(Flags.Lazy))
      case Select(q, _) =>
        val fl = t.symbol.flags
        trivial(q) && (fl.is(Flags.Module) || fl.is(Flags.Package) || (t.symbol.isValDef && !fl.is(Flags.Mutable) && !fl.is(Flags.Lazy)))
      case Typed(e, _) => trivial(e)
      case Lambda(_, _) => true
      case _ => false

    var fresh = 0
    /** a k-free value: in place if trivial, else bound to a val FIRST,
     * so it still runs before the calls of `k` that follow it */
    def value(t: Term, kont: Kont): Term =
      if trivial(t) then feed(kont, t)
      else
        fresh += 1
        val sym = Symbol.newVal(Symbol.spliceOwner, s"cps$$$fresh", t.tpe.widen, Flags.EmptyFlags, Symbol.noSymbol)
        Block(List(ValDef(sym, Some(t.changeOwner(sym)))), feed(kont, Ref(sym)))

    /** `k(e)` / `k.apply(e)`: the argument, which may mention `k` itself */
    object KCall:
      def unapply(t: Term)(using k: Symbol): Option[Term] = t match
        case Apply(Select(i: Ident, "apply"), List(e)) if i.symbol == k => Some(e)
        case Apply(i: Ident, List(e)) if i.symbol == k => Some(e)
        case _ => None

    /** `Call(k, e, s => rest)` */
    def call(e: Term, kont: Kont)(using k: Symbol): Term = kont.rt.asType match
      case '[r] =>
        '{ Cont.Body.Call[A, S, r](${ Ref(k).asExprOf[A => S] }, ${ e.asExprOf[A] },
             ${ lam("s", TypeRepr.of[S], kont.rt)(sv => feed(kont, sv)).asExprOf[S => Cont.Body[r]] }) }.asTerm

    /** `Ap(f, x, y => rest)`, `f` an answer applied to `x` */
    def ap(f: Term, x: Term, kont: Kont): Term = function1(TypeRepr.of[S]) match
      case Some((xt, yt)) => xt.asType match
        case '[xx] => yt.asType match
          case '[yy] => kont.rt.asType match
            case '[r] =>
              '{ Cont.Body.Ap[xx, yy, r](${ f.asExprOf[xx => yy] }, ${ x.asExprOf[xx] },
                   ${ lam("y", yt, kont.rt)(yv => feed(kont, yv)).asExprOf[yy => Cont.Body[r]] }) }.asTerm
      case None => throw Opaque

    /** the parameter types a function term's arguments are matched against */
    def params(fun: Term): List[TypeRepr] = fun.tpe.widen match
      case MethodType(_, ps, _) => ps
      case _ => Nil

    /** arguments in order; a by-name one stays where it is and may not mention `k` */
    def cpsArgs(args: List[Term], ptypes: List[TypeRepr], rt: TypeRepr)(rest: List[Term] => Term)(using k: Symbol): Term =
      def go(i: Int, acc: List[Term]): Term =
        if i == args.length then rest(acc.reverse)
        else
          val a = args(i)
          val byName = ptypes.lift(i).exists { case ByNameType(_) => true; case _ => false }
          a match
            case _ if byName => if mentions(k, a) then throw Opaque else go(i + 1, a :: acc)
            case NamedArg(n, e) => cps(e, Kont(rt, Some(e2 => go(i + 1, NamedArg(n, e2) :: acc))))
            case Typed(Repeated(elems, tpt), tpt2) =>
              cpsArgs(elems, Nil, rt)(es => go(i + 1, Typed(Repeated(es, tpt), tpt2) :: acc))
            case _ => cps(a, Kont(rt, Some(a2 => go(i + 1, a2 :: acc))))
      go(0, Nil)

    /** `fun` in function position: its qualifier and its (curried)
     * arguments are values, in order; the partial application itself
     * is never bound to a val */
    def cpsFun(fun: Term, kont: Kont)(using k: Symbol): Term = fun match
      case Select(q, n) => cps(q, Kont(kont.rt, Some(q2 => feed(kont, Select.copy(fun)(q2, n)))))
      case TypeApply(f2, targs) => cpsFun(f2, Kont(kont.rt, Some(f3 => feed(kont, TypeApply.copy(fun)(f3, targs)))))
      case Apply(f2, args) =>
        cpsFun(f2, Kont(kont.rt, Some(f3 => cpsArgs(args, params(f2), kont.rt)(as => feed(kont, Apply.copy(fun)(f3, as))))))
      case i: Ident => feed(kont, i)
      case _ if !mentions(k, fun) => value(fun, kont)
      case _ => throw Opaque

    /** a block's statements, the k-free prefix kept as it is */
    def cpsStats(stats: List[Statement], e: Term, kont: Kont)(using k: Symbol): Term =
      val (before, from) = stats.span(st => !mentions(k, st))
      val tail = from match
        case Nil => cps(e, kont)
        case (v @ ValDef(name, tpt, Some(rhs))) :: more
            if !v.symbol.flags.is(Flags.Mutable) && !v.symbol.flags.is(Flags.Lazy) =>
          cps(rhs, Kont(kont.rt, Some(r2 =>
            Block(List(ValDef.copy(v)(name, tpt, Some(r2.changeOwner(v.symbol)))), cpsStats(more, e, kont)))))
        case (st: Term) :: more =>
          cps(st, Kont(kont.rt, Some { s2 =>
            val rest = cpsStats(more, e, kont)
            if trivial(s2) then rest else Block(List(s2), rest)
          }))
        case _ => throw Opaque
      if before.isEmpty then tail else Block(before, tail)

    /** the transform: `t` in value position, `kont` what follows its value */
    def cps(t: Term, kont: Kont)(using k: Symbol): Term =
      if !mentions(k, t) then value(t, kont)
      else t match
        case KCall(e) => cps(e, Kont(kont.rt, Some(e2 => call(e2, kont))))
        case Apply(KCall(e), List(x)) =>
          cps(e, Kont(kont.rt, Some(e2 => call(e2, Kont(kont.rt, Some(fv =>
            cps(x, Kont(kont.rt, Some(x2 => ap(fv, x2, kont))))))))))
        case Apply(Select(KCall(e), "apply"), List(x)) =>
          cps(e, Kont(kont.rt, Some(e2 => call(e2, Kont(kont.rt, Some(fv =>
            cps(x, Kont(kont.rt, Some(x2 => ap(fv, x2, kont))))))))))
        case Apply(fun, args) =>
          cpsFun(fun, Kont(kont.rt, Some(f2 => cpsArgs(args, params(fun), kont.rt)(as => feed(kont, Apply.copy(t)(f2, as))))))
        case Inlined(call0, bindings, e) =>
          if bindings.exists(mentions(k, _)) then throw Opaque
          Inlined.copy(t)(call0, bindings, cps(e, kont))
        case Typed(e, _) => cps(e, kont)
        case Lambda(List(p), inner) if kont.rest.isEmpty =>
          function1(kont.rt) match
            case Some((_, r2)) =>
              val in = p.tpt.tpe
              val out = r2.asType match
                case '[r] => TypeRepr.of[Cont.Body[r]]
              val f = Lambda(Symbol.spliceOwner, MethodType(List(p.name))(_ => List(in), _ => out),
                (meth, ps) => subst(cps(inner, Kont(r2, None)), p.symbol, Ref(ps.head.symbol)).changeOwner(meth))
              in.asType match
                case '[s] => r2.asType match
                  case '[r] => done(kont.rt, '{ Cont.Fun[s, r](${ f.asExprOf[s => Cont.Body[r]] }) }.asTerm)
            case None => throw Opaque
        case Block(stats, e) => cpsStats(stats, e, kont)
        case If(c, a, b) =>
          if mentions(k, a) || mentions(k, b) then
            if kont.rest.isDefined then throw Opaque
            cps(c, Kont(kont.rt, Some(c2 => If.copy(t)(c2, cps(a, kont), cps(b, kont)))))
          else cps(c, Kont(kont.rt, Some(c2 => feed(kont, If.copy(t)(c2, a, b)))))
        case Match(sc, cases) =>
          if cases.exists(c => c.guard.exists(mentions(k, _))) then throw Opaque
          if cases.exists(c => mentions(k, c.rhs)) then
            if kont.rest.isDefined then throw Opaque
            cps(sc, Kont(kont.rt, Some(s2 =>
              Match.copy(t)(s2, cases.map(c => CaseDef.copy(c)(c.pattern, c.guard, cps(c.rhs, kont)))))))
          else cps(sc, Kont(kont.rt, Some(s2 => feed(kont, Match.copy(t)(s2, cases)))))
        case _ => throw Opaque

    /** the whole body as `(k: A => S) => Body[R]`, or None where the transform cannot read it */
    def cpsBody(body: Term)(using k: Symbol): Option[Term] =
      try
        val b = cps(body, Kont(TypeRepr.of[R], None))
        Some(Lambda(Symbol.spliceOwner,
          MethodType(List("k"))(_ => List(TypeRepr.of[A => S]), _ => TypeRepr.of[Cont.Body[R]]),
          (meth, ps) => subst(b, k, Ref(ps.head.symbol)).changeOwner(meth)))
      catch case Opaque => None

    f.asTerm.underlyingArgument match
      case Lambda(List(p), body) =>
        given Symbol = p.symbol
        rewrite(body) match
          case Some(v) if eager(v) =>
            '{ Cont.tailPure[A, S, R](${ v.asExprOf[A] }) }
          case Some(v) =>
            '{ Cont.tailShift[A, S, R](() => ${ v.changeOwner(Symbol.spliceOwner).asExprOf[A] }) }
          case None if !mentions(p.symbol, body) => fallback
          case None => cpsBody(body) match
            case Some(b) => '{ Cont.cps[A, S, R](${ b.asExprOf[(A => S) => Cont.Body[R]] }) }
            case None => fallback
      case _ => fallback
