package okay

import scala.quoted.*

/**
 * Optics fused in the COMPILER (specs/optics.md, optics-fuse).
 *
 * `optics-fast` measured why compiling an optic at run time is slower
 * than not compiling it: a live chain inlines into the call site and
 * escape analysis flattens its intermediates, while a pair stored in
 * a field hides behind a lambda the JIT will not inline through, so
 * the same intermediates escape and allocate (176 B/op became 296).
 * The answer is not to store the pair but to do the fusion where
 * inlining is not a hope — in the compiler.
 *
 * `Fuse.set(optic)(b)(s)` reads the optic EXPRESSION rather than a
 * value: a chain of `Lens(get, put)` and `Prism.some` joined by
 * `andThen`, which is exactly what `Lens[S](_.f)` expands to. It emits
 * the nested update the way a person would write it, beta-reducing
 * every lambda, so no optic and no intermediate survives.
 *
 * WHAT IT READS: an optic written literally here, or named by an
 * `inline def` (whose definition it follows). The halves must be
 * EXPLICIT — `Lens(_.f, (s, v) => s.copy(f = v))` — because
 * `Lens[S](_.f)` is itself a macro, and an inline argument is captured
 * BEFORE a nested macro in it expands, so a macro cannot see through
 * another macro's call. Measured and not guessed: with a selector-built
 * lens every call fell back and the benchmark showed the live optic's
 * own figure to the byte (168 B/op).
 *
 * IT ALWAYS COMPILES. Anything it cannot read — a selector-built lens,
 * an optic behind a `val`, a traversal — falls back to
 * `optic.set(b)(s)`, the ordinary road. Correctness never depends on
 * the fusion; only speed does.
 *
 * The macro READS the optic and WRITES the update, which the policy in
 * specs/codecs.md did not allow — see specs/optics.md, optics-fuse,
 * for the amendment and its reason.
 */
object Fuse {
  // The two macro implementations below are private, and the inline
  // defs splice them — which makes the compiler generate an accessor
  // it warns is unstable across recompilations (E192). This annotation
  // is the answer to exactly that: private in source, public in the
  // binary — and it wants a QUALIFIED private, which is what the
  // compiler's own error message asks for. Caught by the zero-warning
  // gate one lane late.
  import scala.annotation.publicInBinary

  /** the optic's `set`, fused where the shape allows */
  transparent inline def set[C[_[_, _]], S, T, A, B](inline o: Optic[C, S, T, A, B])(inline b: B)(inline s: S)
                                                    (using fn: C[Function1]): T =
    ${ setImpl('o, 'b, 's, 'fn) }

  /** the optic's `modify`, fused where the shape allows */
  transparent inline def modify[C[_[_, _]], S, T, A, B](inline o: Optic[C, S, T, A, B])(inline f: A => B)(inline s: S)
                                                       (using fn: C[Function1]): T =
    ${ modifyImpl('o, 'f, 's, 'fn) }

  // ---------------------------------------------------------------- what the macro understood

  private enum Plan:
    /** a lens, as its two halves */
    case L(get: Any, put: Any)
    /** Option's `Some` — the prism the tree shapes actually use */
    case Some_
    /** two in sequence */
    case Then(outer: Plan, inner: Plan)

  private def plan(using q: Quotes)(t: q.reflect.Term): Option[Plan] =
    import q.reflect.*
    t match
      case Inlined(_, _, inner) => plan(inner)
      case Block(Nil, inner) => plan(inner)
      case Typed(inner, _) => plan(inner)
      case Apply(TypeApply(Select(outer, "andThen"), _), List(inner)) =>
        for a <- plan(outer); b <- plan(inner) yield Plan.Then(a, b)
      case Apply(TypeApply(Select(Ident("Lens"), "apply"), _), List(get, put)) =>
        Some(Plan.L(get, put))
      case TypeApply(Select(Ident("Prism"), "some"), _) => Some(Plan.Some_)
      // A REFERENCE TO AN `inline def` ARRIVES UNEXPANDED, and this is
      // the second thing the lane turned on: an inline parameter
      // captures the argument's tree as written, so `Fuse.set(myLens)`
      // hands the macro `Select(This, "myLens")` and not the optic. The
      // definition is reachable — an inline def keeps its tree, in this
      // run and through TASTy — so follow it. Without this every call
      // fell back and the fusion was measured to do nothing, which is
      // exactly what the first benchmark said (168 B/op, the live
      // optic's own figure).
      case ref if !ref.symbol.isNoSymbol && ref.symbol.flags.is(Flags.Inline) =>
        ref.symbol.tree match
          case DefDef(_, _, _, Some(rhs)) => plan(rhs)
          case ValDef(_, _, Some(rhs)) => plan(rhs)
          case _ => None
      case _ => None

  // ---------------------------------------------------------------- emitting the update

  /**
   * The fused `set`: `s` is the whole, `b` the new part, and what
   * comes back is the new whole — a lens's own `put`, an Option's
   * `if defined`, nested as deep as the chain goes, with every lambda
   * beta-reduced away.
   *
   * `Option.get` is read rather than bound to a local: the shape it
   * produces is the one a person writes by hand
   * (`s.copy(f = s.f.map(...))` reads the field twice as well), and a
   * field read is not what this lane is about.
   */
  private def emitSet(using q: Quotes)(p: Plan, s: q.reflect.Term, b: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    p match
      case Plan.L(_, put) => beta(apply2(put.asInstanceOf[Term], s, b))
      case Plan.Some_ => optionOf(s, b)
      case Plan.Then(Plan.L(get, put), inner) =>
        val part = beta(apply1(get.asInstanceOf[Term], s))
        beta(apply2(put.asInstanceOf[Term], s, emitSet(inner, part, b)))
      case Plan.Then(Plan.Some_, inner) =>
        optionOf(s, emitSet(inner, Select.unique(s, "get"), b))
      case Plan.Then(outer, inner) =>
        // Then is right-nested by `andThen`; a left-nested pair is
        // flattened here rather than duplicated above
        emitSet(flatten(Plan.Then(outer, inner)), s, b)

  private def emitModify(using q: Quotes)(p: Plan, s: q.reflect.Term, f: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    p match
      case Plan.L(get, put) =>
        val part = beta(apply1(get.asInstanceOf[Term], s))
        beta(apply2(put.asInstanceOf[Term], s, beta(apply1(f, part))))
      case Plan.Some_ => optionOf(s, beta(apply1(f, Select.unique(s, "get"))))
      case Plan.Then(Plan.L(get, put), inner) =>
        val part = beta(apply1(get.asInstanceOf[Term], s))
        beta(apply2(put.asInstanceOf[Term], s, emitModify(inner, part, f)))
      case Plan.Then(Plan.Some_, inner) =>
        optionOf(s, emitModify(inner, Select.unique(s, "get"), f))
      case Plan.Then(outer, inner) => emitModify(flatten(Plan.Then(outer, inner)), s, f)

  /** `andThen` may nest either way; the emitters want it right-nested */
  private def flatten(p: Plan): Plan = p match
    case Plan.Then(Plan.Then(a, b), c) => flatten(Plan.Then(a, flatten(Plan.Then(b, c))))
    case other => other

  /** `if s.isDefined then Some(v) else None` — the shape a nested copy has */
  private def optionOf(using q: Quotes)(s: q.reflect.Term, v: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    val some = Apply(
      TypeApply(Select.unique(Ref(Symbol.requiredModule("scala.Some")), "apply"), List(Inferred(v.tpe.widen))),
      List(v))
    If(Select.unique(s, "isDefined"), some, Ref(Symbol.requiredModule("scala.None")))

  /**
   * The wrappers off a lambda, so that `betaReduce` sees one.
   *
   * This is the whole lane in one method. A lens's halves arrive
   * wrapped — `Inlined`, a `Block` with no statements, an ascription —
   * because they came out of `Lens[S](_.f)`'s own expansion, and
   * `Expr.betaReduce` reduces an application only when the callee is
   * literally a `Lambda`. Without the peeling the macro emitted the
   * right shape and still allocated every closure: 168 B/op against
   * the hand-written 64, measured, which is how this was found. An
   * `Inlined` carrying bindings is kept — those are definitions the
   * body needs.
   */
  private def peel(using q: Quotes)(t: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    t match
      case Inlined(_, Nil, inner) => peel(inner)
      case Block(Nil, inner) => peel(inner)
      case Typed(inner, _) => peel(inner)
      case other => other

  private def apply1(using q: Quotes)(f: q.reflect.Term, x: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    Apply(Select.unique(peel(f), "apply"), List(x))

  private def apply2(using q: Quotes)(f: q.reflect.Term, x: q.reflect.Term, y: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    Apply(Select.unique(peel(f), "apply"), List(x, y))

  private def beta(using q: Quotes)(t: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    Expr.betaReduce(t.asExpr).asTerm

  // ---------------------------------------------------------------- the two entry points

  @publicInBinary private[Fuse] def setImpl[C[_[_, _]]: Type, S: Type, T: Type, A: Type, B: Type](using q: Quotes)(
      o: Expr[Optic[C, S, T, A, B]], b: Expr[B], s: Expr[S], fn: Expr[C[Function1]]): Expr[T] =
    import q.reflect.*
    plan(o.asTerm) match
      case Some(p) => emitSet(p, s.asTerm, b.asTerm).asExprOf[T]
      case None => '{ $o.set($b)(using $fn)($s) }

  @publicInBinary private[Fuse] def modifyImpl[C[_[_, _]]: Type, S: Type, T: Type, A: Type, B: Type](using q: Quotes)(
      o: Expr[Optic[C, S, T, A, B]], f: Expr[A => B], s: Expr[S], fn: Expr[C[Function1]]): Expr[T] =
    import q.reflect.*
    plan(o.asTerm) match
      case Some(p) => emitModify(p, s.asTerm, f.asTerm).asExprOf[T]
      case None => '{ $o.modify($f)(using $fn)($s) }
}
