package okay

import scala.quoted.*
import scala.annotation.implicitNotFound

/**
 * The flat block (specs/direct-macro.md): `direct[F] { ... m.reflect ... }`
 * rewrites a plain block into its monad's own flatMap binds, so
 * monadic values read as plain values with no for-comprehension.
 * The macro adds SYNTAX only — every emitted program is one the
 * user could write with flatMap by hand; multi-shot, short-circuit
 * and the stack discipline of the monad are inherited, not
 * re-implemented. (The first cut compiled to the Cont binds of
 * Monadic reflection instead; bench-direct priced that layer at
 * 3.3x over the hand-written chain and the target retired —
 * direct-flatmap-emission in the spec's Decisions.) The block is
 * scoped: marks under a lambda (other than the whitelisted loop
 * combinators) or a by-name argument, and a `finally` around marks,
 * are compile errors with the workaround named — refusing that
 * corner is the entire difference between these few hundred lines
 * and a general CPS transformer.
 */
object Direct:

  extension [F[_], A](m: F[A])
    /**
     * The mark: typechecks as A so the block typechecks BEFORE the
     * macro expands; the macro rewrites every call, so this body
     * only runs when a mark escapes outside a direct block — and
     * then it fails loudly rather than compiling to nothing.
     * The mark is a NAME, deliberately: of the symbols tried, .!
     * shadows object ! and .? is ambiguous with the Throws row-? —
     * the retirements are recorded in specs/direct-macro.md; .!?
     * and prefix ! survive as the symbolic spellings below.
     */
    def reflect: A = throw new IllegalStateException(
      "Direct.reflect outside a direct block — wrap the code in direct[F] { ... }")
    /** the symbolic spelling of the same mark — the survivor of the
     * three-strikes history: .! shadowed object !, .? was ambiguous
     * with the Throws row-?, and .!? — once retired as redundant
     * beside .? — is the one symbol that collides with nothing */
    def !? : A = throw new IllegalStateException(
      "Direct.!? outside a direct block — wrap the code in direct[F] { ... }")

    /**
     * The one-glyph mark for the rows, PREFIX: `!prog` — a program
     * of type `A ! F` collapses under its own type's symbol. Exists
     * because `.?` is AMBIGUOUS on Free rows (Effects carries its
     * own row-`?` extension), `.reflect` is a word where a wizard
     * wants a gesture, and a POSTFIX `.!` was tried and refuted the
     * same hour: the method name `!` shadows the object `!` for
     * every file importing Direct.* — `!.run` broke. The prefix
     * spelling (`unary_!`) carries a different name, shadows
     * nothing, and reads as "perform": `val name = !Form.ask[Name]("who?")`.
     */
    def unary_! : A = m.reflect

  // ONE mark, three spellings, all one dispatch-by-TYPE: .reflect
  // (the name, every scope), .!? (postfix symbol — resurrected once
  // .? retired; the one postfix that collides with nothing), and
  // prefix !prog (unary_!, the one-glyph gesture for rows). An
  // F[T] of the block reflects; an operation of the block's row is
  // injected then reflected — no spelling distinguishes the cases,
  // the type does.

  /**
   * The capability (specs/direct-auto-coloring.md): exists ONLY
   * inside a direct block — the auto-coloring conversions require it,
   * so outside a block they cannot resolve and F[A]-as-A stays the
   * compile error it always was.
   */
  @implicitNotFound("no DirectCtx[${F}]: auto-coloring works only INSIDE a direct block.\nWrap the code in direct[F] { ... } — or use the explicit marks (.reflect / .!? / !prog),\nwhich need no capability.")
  final class DirectCtx[F[_]] private[Direct] ()

  /** marker: G's operations may auto-color inside direct blocks */
  @implicitNotFound("no Direct.Effect[${G}]: auto-coloring is OPT-IN per signature.\nRegister the effect once — `given Direct.Effect[${G}] with {}` — or use the explicit marks\n(.reflect / .!? / !prog), which need no marker.")
  trait Effect[G[_]]

  /** the block's own monadic values auto-color: F[A] as A — a
   * phantom, the macro rewrites every call */
  given selfColor[F[_], A](using DirectCtx[F]): Conversion[F[A], A] =
    _ => throw new IllegalStateException(
      "Direct auto-coloring escaped macro rewriting — this call belongs inside direct[F] { ... }")

  /** marked operations auto-color: G[A] as A, row membership checked
   * by the macro exactly as for .!? */
  given opColor[F[_], G[_], A](using DirectCtx[F], Effect[G]): Conversion[G[A], A] =
    _ => throw new IllegalStateException(
      "Direct auto-coloring escaped macro rewriting — this call belongs inside direct[F] { ... }")

  /** rewrite the block: marks become flatMap binds, the result is
   * F[A]. `direct[F] { block }` names only the monad (the partial
   * type application trick); with an expected type both infer:
   * `val p: Int ! W = direct { ... }`. The block is a context
   * function so DirectCtx is ambient in it (plain blocks adapt);
   * the context lambda is stripped by the macro, never called. */
  inline def direct[F[_]]: DirectApply[F] = DirectApply[F]()

  final class DirectApply[F[_]](private val unit: Unit = ()) extends AnyVal:
    inline def apply[A](inline block: DirectCtx[F] ?=> A)(using inline M: Monad[F]): F[A] =
      ${ directImpl[F, A]('block, 'M) }

  /** a term with its inlining and ascription wrappers taken off */
  private def stripped(using q: Quotes)(t: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    t match
      case Inlined(_, Nil, inner) => stripped(inner)
      case Typed(inner, _) => stripped(inner)
      case _ => t

  private def directImpl[F[_] : Type, A: Type](block: Expr[DirectCtx[F] ?=> A],
                                               M: Expr[Monad[F]])
                                              (using Quotes): Expr[F[A]] =
    import quotes.reflect.*
    // the block arrives as a context lambda; take its body — the
    // lambda is never called (see the entry note below)
    val topBody: Term = stripped(block.asTerm) match
      case Block(List(dd: DefDef), _: Closure) =>
        dd.rhs.getOrElse(report.errorAndAbort("empty direct block"))
          .changeOwner(Symbol.spliceOwner)
      case other => report.errorAndAbort(
        "a Direct mark as a non-literal block (a stored context-function value) " +
          "cannot be rewritten by direct's v1", other.pos)
    pipeline[F, A](topBody, M)

  /** the compilation pipeline at ONE monad — recursive for try
   * bodies (direct-try): a try's body is its own sub-block, compiled
   * at the try's type, then bound as one mark. The emission target
   * is plain `F[T]` terms (direct-flatmap-emission): a bind is a
   * Monad.flatMap call, the pure tail is M.pure — exactly the
   * program a careful hand would write, with no Cont layer between
   * the block and its monad. */
  /** the compiler's own evidence that V is a T, summoned at macro
   * time and spliced: the generated code upcasts through it, so no
   * `asInstanceOf` is ever emitted — the macro checked V <:< T on
   * the TypeReprs before asking, and a refusal here would be its bug */
  private def upcast[V: Type, T: Type](using q: Quotes): Expr[V <:< T] =
    Expr.summon[V <:< T].getOrElse(
      q.reflect.report.errorAndAbort(s"direct: ${Type.show[V]} is not a ${Type.show[T]} (macro bug)"))

  private def pipeline[F[_] : Type, A: Type](using q: Quotes)(topLevelBody: q.reflect.Term,
                                             M0: Expr[Monad[F]]): Expr[F[A]] =
    import q.reflect.*
    // ONE instance for the whole block: the summoned Monad
    // expression is hoisted to a val, so every emitted bind shares
    // it — the given for Free is a parameterized class the splice
    // would otherwise re-evaluate per bind. Built by hand (Symbol/
    // Block, not a quote) so the term stays in THIS Quotes context.
    val mmSym = Symbol.newVal(Symbol.spliceOwner, "mm$direct",
      TypeRepr.of[Monad[F]], Flags.EmptyFlags, Symbol.noSymbol)
    val mmVal = ValDef(mmSym, Some(M0.asTerm.changeOwner(mmSym)))
    val body = compileAll[F, A](topLevelBody, Ref(mmSym).asExprOf[Monad[F]])
    Block(List(mmVal), body.asTerm).asExprOf[F[A]]

  private def compileAll[F[_] : Type, A: Type](using q: Quotes)(topLevelBody0: q.reflect.Term,
                                               M: Expr[Monad[F]]): Expr[F[A]] =
    import q.reflect.*
    val topLevelBody = topLevelBody0.changeOwner(Symbol.spliceOwner)

    val directSym = TypeRepr.of[Direct.type].typeSymbol
    val markSyms = (directSym.methodMember("reflect") ++ directSym.methodMember("!?")
      ++ directSym.methodMember("unary_!")).toSet
    val colorSyms = (directSym.methodMember("selfColor") ++
      directSym.methodMember("opColor")).toSet

    def calleeRoot(t: Term): Symbol = t match
      case Apply(f, _) => calleeRoot(f)
      case TypeApply(f, _) => calleeRoot(f)
      case Inlined(_, Nil, inner) => calleeRoot(inner)
      case _ => t.symbol

    /** the marked value: an explicit mark call OR an inserted
     * auto-coloring conversion call — one dispatch serves both,
     * because markTerm decides value-vs-operation by TYPE */
    def asMark(t: Term): Option[Term] = t match
      case Apply(TypeApply(fun, _), List(m)) if markSyms(fun.symbol) => Some(m)
      case Apply(Select(conv, "apply"), List(x))
        if colorSyms(calleeRoot(conv)) => Some(x)
      case _ => None


    def hasMark(t: Tree): Boolean =
      var found = false
      val probe = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case term: Term if asMark(term).isDefined => found = true
            case _ => if !found then super.traverseTree(tree)(owner)
      probe.traverseTree(t)(Symbol.spliceOwner)
      found

    /** the block's effect row, if its F is the program monad A ! Row */
    lazy val rowOf: Option[TypeRepr] =
      TypeRepr.of[F].appliedTo(TypeRepr.of[scala.Unit]).dealias match
        case AppliedType(f, List(row, _)) if f.typeSymbol == freeClass => Some(row)
        case _ => None

    lazy val injectApply: Symbol =
      Symbol.requiredModule("okay.Free.Inject").methodMember("apply").head

    /** Free.Inject[Row, elem](op) — the op lifted into the row program */
    def injectTerm(op: Term, elem: TypeRepr, row: TypeRepr): Term =
      Apply(TypeApply(Ref(injectApply), List(Inferred(row), Inferred(elem.widen))), List(op))

    def refuse(t: Tree, where: String): Nothing =
      report.errorAndAbort(
        s"a Direct mark (.reflect) $where cannot be rewritten by direct's v1 — " +
          "bind the marked value to a val before it, or use a for-comprehension over Monadic",
        t.pos)

    /**
     * Compile a term to either a pure term (no marks) or an F[elem]
     * term. Eff CARRIES its element type: F is invariant in general,
     * so the value type travels with the term instead of being
     * parsed back out of it. Pure children that precede an effectful
     * child are NOT reordered: the ANF hoisting below binds children
     * left to right.
     */
    enum Out:
      case Pure(t: Term)
      case Eff(f: Term, elem: TypeRepr) // f : F[elem]

    /** run f with the TypeRepr as a Type given */
    def tpe2[R](tpe: TypeRepr)(f: [T] => Type[T] ?=> R): R =
      tpe.asType match
        case '[t] => f[t]
      end match

    /** M.pure(t) at t's type */
    def pureF(t: Term): Term =
      tpe2(t.tpe.widen) { [T] => (tT: Type[T]) ?=>
        '{ $M.pure[T](${ t.asExprOf[T] }) }.asTerm
      }

    /**
     * ONE mark, dispatched by type: an F[elem] of this block IS
     * already the term to bind; an operation of this block's row is
     * injected into the row program first. Anything else is refused
     * with both possibilities named.
     */
    def markTerm(m: Term, elem: TypeRepr, at: Position): Term =
      val fT = TypeRepr.of[F].appliedTo(elem.widen)
      if m.tpe <:< fT then m
      else rowOf match
        case Some(row) if m.tpe <:< row.appliedTo(elem.widen) =>
          injectTerm(m, elem, row)
        case _ =>
          report.errorAndAbort(
            s"the marked value has type ${m.tpe.show} — neither this block's ${fT.show}" +
              rowOf.fold("")(r => s" nor an operation of its row ${r.show}"), at)

    /** fa.flatMap(v => body(v)) — body built from a reference to v,
     * returning an F[resTpe] term */
    def bind(fa: Term, vTpe: TypeRepr, resTpe: TypeRepr)(body: Term => Term): Term =
      tpe2(vTpe.widen) { [T] => (tT: Type[T]) ?=>
        tpe2(resTpe.widen) { [B] => (tB: Type[B]) ?=>
          val fa2 = fa.asExprOf[F[T]]
          '{
            $M.flatMap[T]($fa2)[B]((v: T) =>
              ${ body('v.asTerm).changeOwner(Symbol.spliceOwner)
                   .asExprOf[F[B]] })
          }.asTerm
        }
      }

    /** a whitelisted-combinator call: xs.<name>(x => body), with or
     * without the type application the collections put on it */
    object HofCall:
      def unapply(t: Term): Option[(Term, String, ValDef, Term)] = t match
        case Apply(TypeApply(Select(xs, nm), _), List(Lambda(List(p), b))) =>
          Some((xs, nm, p, b))
        case Apply(Select(xs, nm), List(Lambda(List(p), b))) =>
          Some((xs, nm, p, b))
        case _ => None

    /** xs.iterator, built by name so ArrayOps and IterableOnce both
     * serve; refuses receivers with no iterator */
    def iteratorOf(xs: Term): Term =
      if xs.tpe.typeSymbol.methodMember("iterator").isEmpty
        && xs.tpe.baseClasses.forall(_.methodMember("iterator").isEmpty)
      then refuse(xs, "as a loop receiver with no .iterator")
      Select.unique(xs, "iterator")

    /** a term in STATEMENT position: marks compile, and a markless
     * value of the block's own effectful type RUNS — the do-notation
     * reading, so `for t <- xs do Writer(t)` tells instead of
     * silently building and dropping the op */
    def statementF(t: Term): Out =
      compile(t) match
        case e: Out.Eff => e
        case Out.Pure(p) =>
          runnableElem(p) match
            case Some(el) => Out.Eff(markTerm(p, el, t.pos), el)
            case None => Out.Pure(p)

    /** the compiled term at exactly F[tpe] — a no-op when the types
     * already agree; a real fmap when a branch narrows (F need not
     * be covariant, so ascription cannot widen it) */
    def asFAt(o: Out, tpe: TypeRepr): Term =
      tpe2(tpe.widen) { [T] => (tT: Type[T]) ?=>
        o match
          case Out.Pure(p) => '{ $M.pure[T](${ p.asExprOf[T] }) }.asTerm
          case Out.Eff(f, e) =>
            if e.widen =:= tpe.widen then f
            else tpe2(e.widen) { [V] => (tV: Type[V]) ?=>
              // a STATEMENT's value is discarded (Scala's own rule for a
              // Unit position) — said so, not cast; anything else is an
              // upcast the compiler vouches for
              if TypeRepr.of[T] =:= TypeRepr.of[Unit] then
                '{ $M.fmap[V, Unit](${ f.asExprOf[F[V]] }, (_: V) => ()) }.asTerm
              else
                val ev = upcast[V, T]
                '{ $M.fmap[V, T](${ f.asExprOf[F[V]] }, (x: V) => $ev(x)) }.asTerm
            }
      }

    def asF(o: Out): Term = o match
      case Out.Eff(f, _) => f
      case Out.Pure(p) => pureF(p)

    /** for x <- xs do body — run per element, in order; the loop
     * recurses over an immutable, LAZY LazyList so multi-shot re-entry
     * is sound and an unbounded receiver is only forced as far as the
     * monad drives it; a lazy F defers the recursive call inside its
     * own flatMap, exactly as a hand-written loop would */
    /** the value type a loop BODY compiles at — computable from the
     * types alone, so the outer quote can name it BEFORE the body is
     * compiled against the emitted binder (a nested quote inside the
     * splice referencing an outer-bound type param does not pickle) */
    def bodyElemOf(lbody: Term): TypeRepr =
      if hasMark(lbody) then lbody.tpe.widen
      else runnableElemT(lbody.tpe).getOrElse(lbody.tpe.widen)

    def foreachLoop(xs: Term, param: ValDef, lbody: Term): Term =
      tpe2(param.tpt.tpe.widen) { [T] => (tT: Type[T]) ?=>
        val bodyElem = bodyElemOf(lbody)
        tpe2(bodyElem.widen) { [V] => (tV: Type[V]) ?=>
          '{
            val items: LazyList[T] = ${ iteratorOf(xs).asExprOf[Iterator[T]] }.to(LazyList)
            def loop(rest: LazyList[T]): F[Unit] = rest match
              case h #:: tl =>
                $M.flatMap[V](${
                  asFAt(statementF(subst(lbody, param.symbol, 'h.asTerm)), bodyElem)
                    .changeOwner(Symbol.spliceOwner).asExprOf[F[V]]
                })[Unit]((_: V) => loop(tl))
              case _ => $M.pure(())
            loop(items)
          }.asTerm
        }
      }

    /** for x <- xs yield body — the traverse shape; results come out
     * as a List, accepted where the node's type allows it (the loop
     * is emitted AT the node's type, so an invariant F needs no
     * widening after the fact) */
    def mapLoop(t: Term, xs: Term, param: ValDef, lbody: Term): Term =
      tpe2(param.tpt.tpe.widen) { [T] => (tT: Type[T]) ?=>
        tpe2(lbody.tpe.widen) { [U] => (tU: Type[U]) ?=>
          if !(TypeRepr.of[List[U]] <:< t.tpe.widen) then
            refuse(t, s"in a for-yield whose collection type ${t.tpe.widen.show} cannot hold a List " +
              "(v1 yields a List; .toList the receiver or collect explicitly)")
          val uRepr = lbody.tpe.widen
          tpe2(t.tpe.widen) { [W] => (tW: Type[W]) ?=>
            '{
              val items: LazyList[T] = ${ iteratorOf(xs).asExprOf[Iterator[T]] }.to(LazyList)
              def loop(rest: LazyList[T], acc: List[U]): F[W] = rest match
                case h #:: tl =>
                  $M.flatMap[U](${
                    asFAt(compile(subst(lbody, param.symbol, 'h.asTerm)), uRepr)
                      .changeOwner(Symbol.spliceOwner).asExprOf[F[U]]
                  })[W]((b: U) => loop(tl, b :: acc))
                case _ => $M.pure[W](${ upcast[List[U], W] }(acc.reverse)) // List[U] <:< W checked above
              loop(items, Nil)
            }.asTerm
          }
        }
      }

    /** the loop shapes, receiver hoisted first if it is marked */
    def hofLoop(t: Term, xs: Term, nm: String, param: ValDef, lbody: Term): Out =
      val loopElem = if nm == "foreach" then TypeRepr.of[Unit] else t.tpe.widen
      def emit(xsPure: Term): Term = nm match
        case "foreach" => foreachLoop(xsPure, param, lbody)
        case "map" => mapLoop(t, xsPure, param, lbody)
      if hasMark(xs) then
        compile(xs) match
          case Out.Eff(c, e) => Out.Eff(bind(c, e, loopElem)(v => emit(v)), loopElem)
          case Out.Pure(p) => Out.Eff(emit(p), loopElem)
      else Out.Eff(emit(xs), loopElem)

    /** replace references to `sym` with `ref` */
    def subst(t: Term, sym: Symbol, ref: Term): Term =
      val m = new TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
          case id: Ident if id.symbol == sym => ref
          case _ => super.transformTerm(tree)(owner)
      m.transformTerm(t)(Symbol.spliceOwner)

    /** compile an expression */
    def compile(t0: Term): Out =
      val t = stripped(t0)
      asMark(t) match
        case Some(m) =>
          compile(m) match
            case Out.Pure(pm) => Out.Eff(markTerm(pm, t.tpe, t.pos), t.tpe.widen)
            case Out.Eff(cm, ce) => // marks inside the marked value: bind, then mark
              Out.Eff(bind(cm, ce, t.tpe)(v => markTerm(v, t.tpe, t.pos)), t.tpe.widen)
        case None =>
          if hasMark(t) then compileMarked(t)
          else t match
            // a markless Block still goes through compileBlock: a
            // bare statement of the block's own effectful type RUNS
            // (do-notation), and only compileBlock can see it.
            // Lambdas are Blocks too — their bodies stay untouched.
            case Lambda(_, _) => Out.Pure(t)
            case Block(stats, expr) => compileBlock(stats, expr)
            // markless loops whose BODY is the block's own effectful
            // type: statement semantics reach them too — otherwise
            // `for t <- xs do Writer(t)` with no mark anywhere would
            // build and drop each op natively
            case HofCall(xs, "foreach", param, lbody)
              if runnableElemT(lbody.tpe).isDefined =>
              hofLoop(t, xs, "foreach", param, lbody)
            case While(c, b) if runnableElemT(b.tpe).isDefined =>
              compileMarked(t)
            case _ => Out.Pure(t)

    /** t contains marks below the root — dispatch on shape */
    def compileMarked(t: Term): Out = t match
      // whitelisted combinators FIRST — for-do and for-yield desugar
      // to foreach/map with a lambda, and the general lambda refusal
      // below must not claim them
      case HofCall(xs, nm @ ("foreach" | "map"), param, lbody) if hasMark(lbody) =>
        hofLoop(t, xs, nm, param, lbody)

      // BEFORE Block: a Lambda IS Block(DefDef :: Nil, Closure), and
      // the block case would claim it with a vaguer message
      case l @ Lambda(_, _) => refuse(l, "under a lambda")
      case Block(stats, expr) => compileBlock(stats, expr)

      case If(c, th, el) =>
        val branchTpe = t.tpe.widen
        val thF = asFAt(compile(th), branchTpe)
        val elF = asFAt(compile(el), branchTpe)
        compile(c) match
          case Out.Pure(pc) =>
            Out.Eff(If.copy(t)(pc, thF, elF), branchTpe)
          case Out.Eff(cc, ce) =>
            Out.Eff(bind(cc, ce, branchTpe) { v =>
              If.copy(t)(v, thF, elF)
            }, branchTpe)

      case Match(scrut, cases) =>
        val branchTpe = t.tpe.widen
        def casesC = cases.map { cd =>
          val bodyC = asFAt(compile(cd.rhs), branchTpe)
          CaseDef.copy(cd)(cd.pattern, cd.guard.map {
            g => if hasMark(g) then refuse(g, "in a pattern guard") else g
          }, bodyC)
        }
        compile(scrut) match
          case Out.Pure(ps) => Out.Eff(Match.copy(t)(ps, casesC), branchTpe)
          case Out.Eff(cs, se) =>
            Out.Eff(bind(cs, se, branchTpe)(v => Match.copy(t)(v, casesC)), branchTpe)

      // Boolean && / || are compiler intrinsics (their method type is
      // by-value, the short-circuit is magic) — desugar to the If they
      // mean and recurse, keeping the short-circuit
      case Apply(sel @ Select(l, "&&"), List(r)) if sel.symbol.owner == defn.BooleanClass =>
        compile(If(l, r, Literal(BooleanConstant(false))))
      case Apply(sel @ Select(l, "||"), List(r)) if sel.symbol.owner == defn.BooleanClass =>
        compile(If(l, Literal(BooleanConstant(true)), r))

      case While(cond, wbody) =>
        // cond and body splice INSIDE def loop, so they re-evaluate
        // per iteration by construction; a lazy F defers the
        // recursive call inside its own flatMap — the loop inherits
        // F's stack discipline exactly as a hand-written one would.
        // The body is statement position: bare ops of the block RUN.
        val (bodyF, bodyE) = statementF(wbody) match
          case Out.Eff(f, e) => (f.changeOwner(Symbol.spliceOwner), e)
          case Out.Pure(p) =>
            val p2 = p.changeOwner(Symbol.spliceOwner)
            (pureF(p2), p2.tpe.widen)
        tpe2(bodyE.widen) { [U] => (tU: Type[U]) ?=>
          compile(cond) match
            case Out.Pure(pc0) =>
              // a PURE condition needs no bind: a plain `if` per
              // iteration (direct-flatmap-emission fusion #1)
              val pc = pc0.changeOwner(Symbol.spliceOwner)
              Out.Eff('{
                def loop(): F[Unit] =
                  if ${ pc.asExprOf[Boolean] } then
                    $M.flatMap[U](${ bodyF.asExprOf[F[U]] })[Unit]((_: U) => loop())
                  else $M.pure(())
                loop()
              }.asTerm, TypeRepr.of[Unit])
            case Out.Eff(cf0, _) =>
              val cf = cf0.changeOwner(Symbol.spliceOwner)
              Out.Eff('{
                def loop(): F[Unit] =
                  $M.flatMap[Boolean](${ cf.asExprOf[F[Boolean]] })[Unit]((c: Boolean) =>
                    if c then $M.flatMap[U](${ bodyF.asExprOf[F[U]] })[Unit]((_: U) => loop())
                    else $M.pure(()))
                loop()
              }.asTerm, TypeRepr.of[Unit])
        }
      case tr @ Try(b, cases, fin) =>
        // direct-try: the body is its own sub-block, compiled at the
        // try's type through the recursive pipeline; the whole try
        // becomes ONE mark over CanTry's seam. Marked catch bodies
        // go through the pipeline too; finalizers stay refused.
        if fin.isDefined then refuse(tr, "with a finalizer (direct-try v1)")
        // literal branches make the try's type a UNION of singletons
        // (0 | 7): join it by hand — the sub-block compiles at the join
        def joinUnions(t: TypeRepr): TypeRepr = t.dealias match
          case OrType(a, b) =>
            val (ja, jb) = (joinUnions(a).widen, joinUnions(b).widen)
            if ja =:= jb then ja
            else OrType(ja, jb)
          case other => other.widen
        val bT = joinUnions(tr.tpe.widen)
        tpe2(bT) { [T] => (tT: Type[T]) ?=>
          val bodyT = joinUnions(b.tpe.widen)
          val subF: Term = tpe2(bodyT) { [B] => (tB: Type[B]) ?=>
            val raw = pipeline[F, B](b.changeOwner(Symbol.spliceOwner), M)
            // a body ending in throw types Nothing <: T: upcast
            // through the monad (F need not be covariant)
            if bodyT =:= TypeRepr.of[T] then raw.asTerm
            else
              val ev = upcast[B, T]
              '{
                given Monad[F] = $M
                ${ raw }.flatMap((x: B) => $M.pure[T]($ev(x)))
              }.asTerm
          }
          // catch bodies may carry marks too: a marked rhs goes
          // through the same pipeline at the join type; a pure rhs
          // stays a cheap pure-wrap. Marked GUARDS remain refused.
          def caseBody(c: CaseDef): Term =
            c.guard.foreach(g => if hasMark(g) then refuse(g, "in a catch guard"))
            if hasMark(c.rhs) then
              tpe2(joinUnions(c.rhs.tpe.widen)) { [H] => (tH: Type[H]) ?=>
                val hp = pipeline[F, H](c.rhs.changeOwner(Symbol.spliceOwner), M)
                if TypeRepr.of[H] =:= TypeRepr.of[T] then hp.asTerm
                else
                  val ev = upcast[H, T]
                  '{
                    given Monad[F] = $M
                    ${ hp }.flatMap((x: H) => $M.pure[T]($ev(x)))
                  }.asTerm
              }
            else '{ $M.pure[T](${ c.rhs.asExprOf[T] }) }.asTerm
          val handler: Term = '{ (e: Throwable) =>
            ${ Match('{ e }.asTerm,
                 cases.map(c => CaseDef.copy(c)(c.pattern, c.guard, caseBody(c)))
                 :+ CaseDef(Wildcard(), None, '{ throw e }.asTerm)
               ).asExprOf[F[T]] }
          }.asTerm
          val guarded: Term = '{
            scala.compiletime.summonInline[CanTry[F]]
              .tryIn[T](${ subF.asExprOf[F[T]] })(${ handler.asExprOf[Throwable => F[T]] })
          }.asTerm
          Out.Eff(guarded, bT)
        }

      // application shapes: ANF-hoist children left to right
      // an assignment with a marked rhs: bind, then assign the value
      case Assign(lhs, rhs) =>
        compile(rhs) match
          case Out.Pure(p) => Out.Pure(Assign.copy(t)(lhs, p))
          case Out.Eff(c, e) =>
            Out.Eff(bind(c, e, TypeRepr.of[Unit]) { v =>
              pureF(Assign.copy(t)(lhs, v))
            }, TypeRepr.of[Unit])

      // application spines: hoist VALUE slots only (receiver
      // qualifier, arguments) left to right; the callee structure —
      // Selects, TypeApplies, curried Apply lists — is rebuilt, never
      // hoisted (a partially applied method is not a value)
      case _: (Apply | TypeApply | Select) =>
        spineSlots(t) match
          case Some((slots, rebuild)) => anf(slots, t.tpe)(rebuild)
          case None => refuse(t, "in a call shape v1 does not rewrite")

      case Typed(e, tp) => anf(List(e), t.tpe) { case List(e2) => Typed.copy(t)(e2, tp) }

      case other => refuse(other, s"in an unsupported position (${other.getClass.getSimpleName})")

    /** decompose an application spine into its hoistable value slots
     * and a rebuild function over replacements for those slots */
    def spineSlots(t: Term): Option[(List[Term], List[Term] => Term)] = t match
      case Apply(fun, args) =>
        fun.tpe.widen match
          // NOT isInstanceOf: the reflect API's types are abstract and
          // erase to TypeRepr, so isInstanceOf[ByNameType] is always
          // true — the pattern match goes through the API's TypeTest
          case mt: MethodType if args.exists(hasMark) &&
            mt.paramTypes.exists { case _: ByNameType => true; case _ => false } =>
            refuse(t, "under a by-name argument")
          case _ => ()
        spineSlots(fun).map { (fs, fr) =>
          (fs ++ args, vs => {
            val (fvs, avs) = vs.splitAt(fs.length)
            Apply.copy(t)(fr(fvs), avs)
          })
        }
      case TypeApply(fun, targs) =>
        spineSlots(fun).map((fs, fr) => (fs, vs => TypeApply.copy(t)(fr(vs), targs)))
      case sel @ Select(qual, nm) =>
        Some((List(qual), { case List(q) => Select.copy(sel)(q, nm) }))
      case id: Ident => Some((Nil, _ => id))
      case _ => None

    /** a child whose evaluation nobody can observe being moved */
    def trivial(t: Term): Boolean = t match
      case _: Ident | _: Literal | _: This => true
      case Typed(inner, _) => trivial(inner)
      case _ => false

    /** hoist the children in evaluation order: an effectful child
     * binds; a pure child that PRECEDES a later effectful one is
     * bound to a val first, so it runs before that effect and once
     * (not after it, and not once per continuation under multi-shot
     * — the order inversion the 2026-09-02 audit found); pure
     * children after the last effect pass through */
    def anf(children: List[Term], resTpe: TypeRepr)(rebuild: List[Term] => Term): Out =
      val lastEff = children.lastIndexWhere(hasMark)
      def loop(rest: List[Term], i: Int, acc: List[Term]): Term =
        rest match
          case Nil => pureF(rebuild(acc.reverse))
          case c :: tail =>
            if !hasMark(c) then
              if i < lastEff && !trivial(c) then
                val sym = Symbol.newVal(Symbol.spliceOwner, s"hoisted$i", c.tpe.widen,
                  Flags.EmptyFlags, Symbol.noSymbol)
                Block(List(ValDef(sym, Some(c))), loop(tail, i + 1, Ref(sym) :: acc))
              else loop(tail, i + 1, c :: acc)
            else compile(c) match
              case Out.Pure(p) => loop(tail, i + 1, p :: acc)
              case Out.Eff(ce, e) =>
                bind(ce, e, resTpe) { v =>
                  loop(tail, i + 1, v :: acc)
                }
      Out.Eff(loop(children, 0, Nil), resTpe.widen)

    /** a Block with statements: fold vals/exprs into binds */
    def compileBlock(stats: List[Statement], expr: Term): Out =
      stats match
        case Nil => compile(expr)
        case (vd @ ValDef(name, tpt, Some(rhs))) :: rest =>
          if vd.symbol.flags.is(Flags.Lazy) && hasMark(rhs) then refuse(vd, "in a lazy val")
          compile(rhs) match
            case Out.Pure(p) =>
              wrapPure(vd, p, rest, expr)
            case Out.Eff(c, e) =>
              // the val KEEPS its symbol, re-bound to the continuation's
              // parameter: a later def or an assignment (for a var)
              // still refers to it — substitution would strand them
              Out.Eff(bind(c, e, expr.tpe) { v =>
                val vd2 = ValDef.copy(vd)(vd.name, vd.tpt, Some(v))
                asF(wrapStat(vd2, rest, expr))
              }, expr.tpe.widen)
        // an assignment in STATEMENT position binds straight into the
        // assignment (direct-flatmap-emission fusion #2) — the
        // expression-position Assign in compileMarked would bind into
        // a pure and pay a second bind to sequence it
        case (a @ Assign(lhs, rhs)) :: rest if hasMark(rhs) =>
          compile(rhs) match
            case Out.Pure(p) => wrapStat(Assign.copy(a)(lhs, p), rest, expr)
            case Out.Eff(c, e) =>
              Out.Eff(bind(c, e, expr.tpe) { v =>
                asF(wrapStat(Assign.copy(a)(lhs, v), rest, expr))
              }, expr.tpe.widen)
        case (dd: Definition) :: rest =>
          if hasMark(dd) then refuse(dd, "inside a nested definition")
          wrapStat(dd, rest, expr)
        case (st: Term) :: rest =>
          compile(st) match
            case Out.Pure(p) =>
              runnableElem(p) match
                // do-notation: a bare statement of the block's F or
                // row type RUNS, its value dropped — the `_ <-` reading
                case Some(elem) =>
                  Out.Eff(bind(markTerm(p, elem, st.pos), elem, expr.tpe) { _ =>
                    asF(compileBlock(rest, expr))
                  }, expr.tpe.widen)
                case None =>
                  // a FOREIGN marked type can be neither run nor
                  // meaningfully dropped
                  if discardedMonadic(p.tpe) then
                    report.errorAndAbort(
                      s"a value of ${p.tpe.widen.show} is discarded in statement position, " +
                        "and it is neither this block's monad nor an operation of its row — " +
                        "it cannot run here; bind it or move it to its own block", st.pos)
                  wrapStat(p, rest, expr)
            case Out.Eff(c, e) =>
              Out.Eff(bind(c, e, expr.tpe) { _ =>
                asF(compileBlock(rest, expr))
              }, expr.tpe.widen)
        case other :: _ => refuse(other, "in an unsupported statement")

    lazy val freeClass = Symbol.requiredClass("okay.Free")

    /** is this a monadic/marked value a statement would silently drop? */
    def discardedMonadic(tpe: TypeRepr): Boolean =
      val w = tpe.widen.dealias
      if w.derivesFrom(freeClass) then true
      else w match
        case AppliedType(g, args) if args.nonEmpty =>
          // the candidate constructor is g with all but the LAST
          // argument fixed — Writer[String, Unit] asks Effect[[X] =>>
          // Writer[String, X]]
          val lam =
            if args.lengthIs == 1 then g
            else TypeLambda(List("X"), _ => List(TypeBounds.empty),
              tl => g.appliedTo(args.init :+ tl.param(0)))
          (args.lengthIs == 1 && g =:= TypeRepr.of[F]) || (Implicits.search(
            TypeRepr.of[Effect].appliedTo(lam)) match
            case _: ImplicitSearchSuccess => true
            case _ => false)
        case _ => false

    /**
     * The element type under which a bare statement can RUN in this
     * block — p is an F[T] of the block's monad or an operation of
     * its row. Candidates come from the type's own arguments (answer
     * type last, so tried first) and, for programs, from the Free
     * base type; the <:< check makes the guesses safe.
     */
    def runnableElem(p: Term): Option[TypeRepr] = runnableElemT(p.tpe)

    def runnableElemT(tpe0: TypeRepr): Option[TypeRepr] =
      val w = tpe0.widen.dealias
      val fromFree = w.baseType(freeClass) match
        case AppliedType(_, List(_, t)) => List(t)
        case _ => Nil
      val fromArgs = w match
        case AppliedType(_, args) => args.reverse
        case _ => Nil
      // None.type-like singletons carry no arguments of their own —
      // the base type at the block's monad does (Option[Nothing])
      val fromFBase =
        val fs = TypeRepr.of[F].dealias.typeSymbol
        if fs.exists && fs.isClassDef then w.baseType(fs) match
          case AppliedType(_, args) if args.nonEmpty => List(args.last)
          case _ => Nil
        else Nil
      (fromFree ++ fromArgs ++ fromFBase).find { t =>
        tpe0 <:< TypeRepr.of[F].appliedTo(t.widen) ||
          rowOf.exists(r => tpe0 <:< r.appliedTo(t.widen))
      }

    /** keep a pure statement in front of the compiled rest */
    def wrapStat(s: Statement, rest: List[Statement], expr: Term): Out =
      compileBlock(rest, expr) match
        case Out.Pure(p) => Out.Pure(Block(List(s), p))
        case Out.Eff(c, e) => Out.Eff(Block(List(s), c), e)

    def wrapPure(vd: ValDef, rhs: Term, rest: List[Statement], expr: Term): Out =
      wrapStat(ValDef.copy(vd)(vd.name, vd.tpt, Some(rhs)), rest, expr)

    asFAt(compile(topLevelBody), TypeRepr.of[A]).asExprOf[F[A]]
