package okay

import scala.quoted.*
import okay.Optic.{Forget, First, Star}

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
 * `inline def` (whose definition it follows), with its halves written
 * out — and, since optics-zero-tax, `Lens[S](_.f)` as well.
 *
 * That last one was recorded here as impossible, and the record was
 * half right. A macro cannot make another macro expand; it does not
 * have to. `Focus.impl` is an ordinary compile-time function over
 * trees, so `plan` calls it with the selector and the Mirror it finds
 * in the call and reads the result. The correction matters because
 * `Lens[S](_.f)` is the idiomatic way to build a lens here, so while
 * it was unreadable the fusion was off for most code that wanted it.
 *
 * IT ALWAYS COMPILES. Anything it cannot read — an optic behind a
 * `val`, a traversal, a block with statements in it — falls back to
 * `optic.set(b)(s)`, the ordinary road. Correctness never depends on
 * the fusion; only speed does, and TestFuse tells the two apart at
 * run time with a poisoned interpretation rather than trusting a
 * comment.
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
      // `Lens[S](_.f)` IS ITSELF A MACRO, and a macro cannot make
      // another expand — which is true, and was read as a dead end for
      // one lane. It is not one: `Focus.impl` is an ordinary
      // compile-time function over trees, so the expansion this needs
      // is not something to wait for, it is a call. The selector and
      // the Mirror are right here in the tree; hand them over and plan
      // the result.
      //
      // This matters more than it sounds: `Lens[S](_.f)` is the
      // idiomatic way to build a lens in this library, so while it was
      // unreadable the fusion was off for most code that would want it.
      case Apply(Apply(TypeApply(Select(focus, "apply"), List(ta)), List(get)), List(mirror))
          if focus.tpe.widen.typeSymbol.fullName == "okay.Focus" =>
        focus.tpe.widen match
          case AppliedType(_, List(sTpe)) =>
            (sTpe.asType, ta.tpe.asType) match
              case ('[st], '[at]) =>
                plan(Focus.impl[st, at](
                  get.asExprOf[st => at],
                  mirror.asExprOf[scala.deriving.Mirror.ProductOf[st]]).asTerm)
              // the types did not come back as types: refuse rather
              // than guess, which is what every other arm here does
              case _ => None
          case _ => None

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

      // AND A PLAIN `val`, which is how an optic is actually stored.
      // The soundness argument is `plan` itself: it succeeds only on
      // pure optic CONSTRUCTIONS — a `Lens` with its halves,
      // `Prism.some`, a selector, an `andThen` of those — so following
      // a definition can only ever emit what that expression means,
      // and it cannot run anything the program would not have run.
      //
      // What must be excluded is a definition the call site's type
      // does not fix: a `var`, and a member a subclass may override.
      // Hence `fixedValue`. Everything else still falls back.
      case ref if !ref.symbol.isNoSymbol && ref.symbol.isValDef && fixedValue(ref.symbol) =>
        ref.symbol.tree match
          case ValDef(_, _, Some(rhs)) => plan(rhs)
          case _ => None
      case _ => None

  /** a definition whose value the call site's type really fixes */
  private def fixedValue(using q: Quotes)(sym: q.reflect.Symbol): Boolean =
    import q.reflect.*
    !sym.flags.is(Flags.Mutable) && (
      sym.flags.is(Flags.Final) || sym.flags.is(Flags.Private) ||
      sym.maybeOwner.flags.is(Flags.Module) || sym.maybeOwner.flags.is(Flags.Final))

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

  /**
   * THE READ SIDE. A lens chain's `get` is the composed projection —
   * `s.a.b` — with every lambda beta-reduced away, which is what a
   * person writes and what the interpretation cannot be: `Forget`
   * allocates per call and the chain of them per level.
   *
   * A prism has no `get`: an absent focus is not a value. So a plan
   * with `Some_` anywhere answers None here and the caller falls back
   * to the interpretation, which knows what to do about absence.
   */
  private def emitGet(using q: Quotes)(p: Plan, s: q.reflect.Term): Option[q.reflect.Term] =
    import q.reflect.*
    flatten(p) match
      case Plan.L(get, _) => Some(beta(apply1(get.asInstanceOf[Term], s)))
      case Plan.Some_ => None
      case Plan.Then(Plan.L(get, _), inner) =>
        emitGet(inner, beta(apply1(get.asInstanceOf[Term], s)))
      case Plan.Then(_, _) => None

  /**
   * PREVIEW, which unlike `get` knows what to do with absence — and
   * so reads the shape most of this library's optics actually are.
   * `JsonOptic.field` is `at ∘ some`, and every path built from it is
   * an affine.
   *
   * Three shapes, and each is what a person would write:
   *   - a lens step is the projection, wrapped once at the end;
   *   - previewing `some` on an `Option` IS that Option, unwrapped;
   *   - a `some` in the middle is the test, `if defined then … else
   *     None`, with the rest emitted inside it.
   */
  private def emitPreview(using q: Quotes)(p: Plan, s: q.reflect.Term): Option[q.reflect.Term] =
    import q.reflect.*
    flatten(p) match
      case Plan.L(get, _) => Some(someOf(beta(apply1(get.asInstanceOf[Term], s))))
      case Plan.Some_ => Some(s)
      case Plan.Then(Plan.L(get, _), inner) =>
        emitPreview(inner, beta(apply1(get.asInstanceOf[Term], s)))
      case Plan.Then(Plan.Some_, inner) =>
        emitPreview(inner, Select.unique(s, "get")).map(v => ifDefined(s, v))
      case Plan.Then(_, _) => None

  /** a plan the read side can emit: no absence anywhere in it */
  private def readable(p: Plan): Boolean = p match
    case Plan.L(_, _) => true
    case Plan.Some_ => false
    case Plan.Then(a, b) => readable(a) && readable(b)

  /** `andThen` may nest either way; the emitters want it right-nested */
  private def flatten(p: Plan): Plan = p match
    case Plan.Then(Plan.Then(a, b), c) => flatten(Plan.Then(a, flatten(Plan.Then(b, c))))
    case other => other

  /** `Some(v)` */
  private def someOf(using q: Quotes)(v: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    Apply(
      TypeApply(Select.unique(Ref(Symbol.requiredModule("scala.Some")), "apply"), List(Inferred(v.tpe.widen))),
      List(v))

  /** `if s.isDefined then v else None`, where `v` is ALREADY an Option */
  private def ifDefined(using q: Quotes)(s: q.reflect.Term, v: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    If(Select.unique(s, "isDefined"), v, Ref(Symbol.requiredModule("scala.None")))

  /** `if s.isDefined then Some(v) else None` — the shape a nested copy has */
  private def optionOf(using q: Quotes)(s: q.reflect.Term, v: q.reflect.Term): q.reflect.Term =
    ifDefined(s, someOf(v))

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
      // the fallback goes STRAIGHT to the interpretation, never back
      // through `.set` — that extension is now this macro, and calling
      // it here would be an infinite expansion
      case None => '{ $o.apply[Function1]((_: A) => $b)(using $fn)($s) }

  @publicInBinary private[Fuse] def modifyImpl[C[_[_, _]]: Type, S: Type, T: Type, A: Type, B: Type](using q: Quotes)(
      o: Expr[Optic[C, S, T, A, B]], f: Expr[A => B], s: Expr[S], fn: Expr[C[Function1]]): Expr[T] =
    import q.reflect.*
    plan(o.asTerm) match
      case Some(p) => emitModify(p, s.asTerm, f.asTerm).asExprOf[T]
      case None => '{ $o.apply[Function1]($f)(using $fn)($s) }

  // ---------------------------------------------------------------- fusing by default
  //
  // The same two, shaped for the EXTENSION methods, which answer a
  // function rather than a value: the lambda is built inside the quote
  // so the update can be emitted into its body. `o.set(b)(s)` then
  // applies a lambda whose body is the hand-written update, and the
  // one allocation left is the lambda itself — which escape analysis
  // removes where it does not escape, and which the benchmark
  // measures rather than assumes.

  @publicInBinary private[okay] def setFnImpl[C[_[_, _]]: Type, S: Type, T: Type, A: Type, B: Type](using q: Quotes)(
      o: Expr[Optic[C, S, T, A, B]], b: Expr[B], fn: Expr[C[Function1]]): Expr[S => T] =
    import q.reflect.*
    plan(o.asTerm) match
      case Some(p) => lambdaOf[S, T](s => emitSet(p, s, b.asTerm))
      case None => '{ $o.apply[Function1]((_: A) => $b)(using $fn) }

  @publicInBinary private[okay] def modifyFnImpl[C[_[_, _]]: Type, S: Type, T: Type, A: Type, B: Type](using q: Quotes)(
      o: Expr[Optic[C, S, T, A, B]], f: Expr[A => B], fn: Expr[C[Function1]]): Expr[S => T] =
    import q.reflect.*
    plan(o.asTerm) match
      case Some(p) => lambdaOf[S, T](s => emitModify(p, s, f.asTerm))
      case None => '{ $o.apply[Function1]($f)(using $fn) }

  // ---------------------------------------------------------------- the read side's entry points

  @publicInBinary private[okay] def getImpl[C[_[_, _]]: Type, S: Type, T: Type, A: Type, B: Type](using q: Quotes)(
      o: Expr[Optic[C, S, T, A, B]], s: Expr[S],
      fn: Expr[C[[X, Y] =>> Forget[A, X, Y]]]): Expr[A] =
    import q.reflect.*
    plan(o.asTerm).flatMap(p => emitGet(p, s.asTerm)) match
      case Some(t) => t.asExprOf[A]
      case None => '{ $o.apply[[X, Y] =>> Forget[A, X, Y]](Forget[A, A, B]((a: A) => a))(using $fn).run($s) }

  @publicInBinary private[okay] def foldMapImpl[C[_[_, _]]: Type, S: Type, T: Type, A: Type, B: Type, R: Type](using q: Quotes)(
      o: Expr[Optic[C, S, T, A, B]], f: Expr[A => R], s: Expr[S],
      fn: Expr[C[[X, Y] =>> Forget[R, X, Y]]]): Expr[R] =
    import q.reflect.*
    plan(o.asTerm).flatMap(p => emitGet(p, s.asTerm)) match
      case Some(t) => beta(apply1(f.asTerm, t)).asExprOf[R]
      case None => '{ $o.apply[[X, Y] =>> Forget[R, X, Y]](Forget[R, A, B]($f))(using $fn).run($s) }

  @publicInBinary private[okay] def previewImpl[C[_[_, _]]: Type, S: Type, T: Type, A: Type, B: Type](using q: Quotes)(
      o: Expr[Optic[C, S, T, A, B]], s: Expr[S],
      fn: Expr[C[[X, Y] =>> Forget[First[A], X, Y]]]): Expr[Option[A]] =
    import q.reflect.*
    plan(o.asTerm).flatMap(p => emitPreview(p, s.asTerm)) match
      case Some(t) => Typed(t, TypeTree.of[Option[A]]).asExprOf[Option[A]]
      case None =>
        '{ $o.apply[[X, Y] =>> Forget[First[A], X, Y]](
             Forget[First[A], A, B]((a: A) => First(Some(a))))(using $fn).run($s).value }

  @publicInBinary private[okay] def toVectorImpl[C[_[_, _]]: Type, S: Type, T: Type, A: Type, B: Type](using q: Quotes)(
      o: Expr[Optic[C, S, T, A, B]], s: Expr[S],
      fn: Expr[C[[X, Y] =>> Forget[Vector[A], X, Y]]]): Expr[Vector[A]] =
    import q.reflect.*
    plan(o.asTerm).flatMap(p => emitGet(p, s.asTerm)) match
      case Some(t) => '{ Vector(${ t.asExprOf[A] }) }
      case None =>
        '{ $o.apply[[X, Y] =>> Forget[Vector[A], X, Y]](
             Forget[Vector[A], A, B]((a: A) => Vector(a)))(using $fn).run($s) }

  /**
   * The effectful walk, fused: for a lens chain `traverseOf(f)` is
   * `fmap(f(s.a.b), b => put(s, b))` — the read the fusion already
   * emits, the effect, and the write the fusion already emits.
   *
   * Two things make it fall back rather than guess: a prism in the
   * chain (absence needs the interpretation's `pure`), and an
   * `Applicative[F]` that cannot be summoned here, which happens
   * whenever `F` is not known at this call site.
   */
  @publicInBinary private[okay] def traverseOfImpl[C[_[_, _]]: Type, S: Type, T: Type, A: Type, B: Type, F[_]: Type](using q: Quotes)(
      o: Expr[Optic[C, S, T, A, B]], f: Expr[A => F[B]],
      fn: Expr[C[[X, Y] =>> Star[F, X, Y]]]): Expr[S => F[T]] =
    import q.reflect.*
    (plan(o.asTerm), Expr.summon[Applicative[F]]) match
      case (Some(p), Some(ap)) if readable(p) =>
        lambdaIn[S, F[T]](Symbol.spliceOwner) { (owner, s) =>
          emitGet(p, s) match
            case Some(part) =>
              val effect = beta(apply1(f.asTerm, part)).asExprOf[F[B]]
              val put = lambdaIn[B, T](owner)((_, b) => emitSet(p, s, b))
              '{ $ap.fmap($effect, $put) }.asTerm
            // readable(p) already said this cannot happen; if it ever
            // does, the interpretation is still right
            case None => '{ $o.apply[[X, Y] =>> Star[F, X, Y]](Star($f))(using $fn).run(${ s.asExprOf[S] }) }.asTerm
        }
      case _ => '{ $o.apply[[X, Y] =>> Star[F, X, Y]](Star($f))(using $fn).run }

  /**
   * `(s: S) => body(s)`, built with the reflection API rather than as
   * a quote — and the reason is not style. A nested quote opens a NEW
   * `Quotes` context, and the plan holds terms from THIS one; the
   * compiler refuses to mix them, which is the error this shape
   * avoids. One context, one owner, and the body re-owned to the
   * lambda.
   */
  private def lambdaOf[S: Type, T: Type](using q: Quotes)(
      body: q.reflect.Term => q.reflect.Term): Expr[S => T] =
    lambdaIn[S, T](using q)(quotes.reflect.Symbol.spliceOwner)((_, x) => body(x))

  /** the same, owned by a symbol the caller names — which is what a
   * lambda built inside another lambda's body needs */
  private def lambdaIn[S: Type, T: Type](using q: Quotes)(owner: q.reflect.Symbol)(
      body: (q.reflect.Symbol, q.reflect.Term) => q.reflect.Term): Expr[S => T] =
    import q.reflect.*
    Lambda(
      owner,
      MethodType(List("s"))(_ => List(TypeRepr.of[S]), _ => TypeRepr.of[T]),
      (owner, params) => params match
        // the parameter of a Lambda is an Ident, which is a Term; the
        // match says so instead of a cast
        case List(p: Term) => body(owner, p).changeOwner(owner)
        case other => report.errorAndAbort(s"a one-parameter lambda, got: $other")
    ).asExprOf[S => T]
}
