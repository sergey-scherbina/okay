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
 * Public because an expansion at a user's call site calls it (the
 * same standing as `Distinct.impl`); not an API.
 */
object ContMacro:

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

    f.asTerm.underlyingArgument match
      case Lambda(List(p), body) =>
        given Symbol = p.symbol
        rewrite(body) match
          case Some(v) if eager(v) =>
            '{ Cont.tailPure[A, S, R](${ v.asExprOf[A] }) }
          case Some(v) =>
            '{ Cont.tailShift[A, S, R](() => ${ v.changeOwner(Symbol.spliceOwner).asExprOf[A] }) }
          case None => fallback
      case _ => fallback
