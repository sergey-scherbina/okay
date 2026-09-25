package okay
package macros

import scala.quoted.*
import scala.annotation.tailrec

/**
 * THE CORE of the `direct` compiler, and the class the phases mix
 * into (direct-compiler-phases, 2026-09-20 — before it, one
 * 1460-line method of nested defs). What lives here is the
 * recursion knot the phases reach through DirectPhase's abstract
 * `compile`/`compileBlock`: an expression compiles to a pure term or
 * an `F[elem]` term, a marked shape dispatches on its form, an
 * application hoists its value slots left to right (ANF), a block
 * folds its statements into binds. The phases:
 *
 *   DirectMarks     what the reader wrote — marks, colourings, uses
 *   DirectRow       what F is — the row, runnable values, the lifts
 *   DirectEmit      the monad's words — pure, flatMap, fmap
 *   DirectDefer     the pre-pass — Free.delay around recursive calls
 *   DirectVals      val / lazy val / def at the program type
 *   DirectLoops     for-do, for-yield, the fused statement tail
 *   DirectParallel  independent binds spawned together
 *
 * A test reaches one phase on its own through the probes in
 * src/test/scala/DirectProbe.scala.
 */
private[okay] object DirectCompiler:
  /** the compilation pipeline at ONE monad — recursive for try
   * bodies (direct-try): a try's body is its own sub-block, compiled
   * at the try's type, then bound as one mark. The emission target
   * is plain `F[T]` terms (direct-flatmap-emission): a bind is a
   * Monad.flatMap call, the pure tail is M.pure — exactly the
   * program a careful hand would write, with no Cont layer between
   * the block and its monad. */
  def pipeline[F[_] : Type, A: Type](using q: Quotes)(topLevelBody: q.reflect.Term,
                                     M0: Expr[Monad[F]],
                                     eager: Boolean,
                                     parallel: Boolean,
                                     stage0: Option[q.reflect.Term] = None): Expr[F[A]] =
    import q.reflect.*
    // ONE instance for the whole block: the summoned Monad
    // expression is hoisted to a val, so every emitted bind shares
    // it — the given for Free is a parameterized class the splice
    // would otherwise re-evaluate per bind. Built by hand (Symbol/
    // Block, not a quote) so the term stays in THIS Quotes context.
    // For a STAGED block, at the summoned instance's PRECISE type, not
    // `Monad[F]`: the emission then selects `flatMap`/`pure`/`fmap` on
    // this val by name (DirectEmit) and names the given's `override
    // inline` members, which the inliner reduces — measured as the
    // difference between 152 568 and 84 568 B/op on the same block.
    // Every other block keeps `Monad[F]` and the quote road, byte for
    // byte what it emitted before: tried on all carriers, the precise
    // type broke two — `ctxMonad[E]`'s declared `E ?=> A` result is a
    // type the typer auto-applies ("bad adapt", TestDirectTryCtx), and
    // a Free block that REBUILDS a lambda (programLambda, a nested
    // block under `Delim.shift`) left the inlined binds' proxies with
    // an owner LambdaLift could not find (TestBookInTheSystem). The
    // Free road was then built with both laws kept (direct-inline-
    // bind-free, 2026-09-23) and REFUTED by its number: bytes 0.97,
    // time 1.00 — the gap to a hand-written Free program is the
    // deferred self-call, not the virtual bind
    val mmSym = Symbol.newVal(Symbol.spliceOwner, "mm$direct",
      if stage0.isDefined then M0.asTerm.tpe.widen else TypeRepr.of[Monad[F]], Flags.EmptyFlags, Symbol.noSymbol)
    val mmVal = ValDef(mmSym, Some(M0.asTerm.changeOwner(mmSym)))
    // the Stager object the same way (direct-staged): an inline
    // argument is substituted at every use, and a `new` there would
    // be one instance per operation; the val keeps the object's own
    // type, which is what its inline `stage` resolves on
    val stVal = stage0.map { st =>
      val sym = Symbol.newVal(Symbol.spliceOwner, "st$direct", st.tpe.widen, Flags.EmptyFlags, Symbol.noSymbol)
      ValDef(sym, Some(st.changeOwner(sym)))
    }
    val compiler = new DirectCompiler[F](q, Type.of[F], Ref(mmSym).asExprOf[Monad[F]], eager, parallel,
      stVal.map(v => Ref(v.symbol)))
    val body = compiler.compileAll[A](topLevelBody.asExpr)
    Block(List(mmVal) ++ stVal, body.asTerm).asExprOf[F[A]]

private[okay] final class DirectCompiler[F[_]](val q: Quotes, val fT: Type[F],
                                               val M: Expr[Monad[F]],
                                               val eager: Boolean,
                                               val parallel: Boolean,
                                               val stage: Option[q.reflect.Term] = None)
  extends DirectDefer[F] with DirectLoops[F] with DirectParallel[F]:
  import q.reflect.*

  /** the whole pipeline at one monad: the defer pre-pass, then the
   * block, delivered at exactly `F[A]` */
  def compileAll[A: Type](body: Expr[Any]): Expr[F[A]] =
    val topLevelBody = deferSelfCalls(body.asTerm.changeOwner(Symbol.spliceOwner))
    // a generator block whose WHOLE body is a for-yield: emitted, the
    // block answers () (specs/generators.md)
    // …decided HERE, before any statement is compiled: a bind emitted
    // for an earlier statement carries the block's result type, and a
    // result type that changes under it is a cast exception
    val body1 = stripped(topLevelBody) match
      case t if yieldLoop(t).isDefined => Block(List(t), Literal(UnitConstant()))
      case Block(stats, expr) if yieldLoop(expr).isDefined => Block(stats :+ expr, Literal(UnitConstant()))
      case _ => topLevelBody
    asFAt(compile(body1), TypeRepr.of[A]).asExprOf[F[A]]

  /** compile an expression */
  def compile(t0: Term): Out =
    val t = stripped(t0)
    t match
      // the inliner's proxy bindings (`(s + "!").tell`): a block, and
      // `stripped` only takes off an Inlined with none
      case Inlined(_, bindings, inner) if bindings.nonEmpty =>
        return compile(Block(bindings, inner))
      case _ => ()
    asMark(t) match
      // a staged block's LEAF program (`State.get[Int]`): staged from
      // the term as written, BEFORE compile flattens its inlining
      // wrappers — the inliner's proxy for the operation (`val a$proxy
      // = Get(); Inject(a$proxy)`) would otherwise become a statement
      // of the block, and the op an identifier no inline match can
      // reduce on. A leaf with marks in its argument takes the road
      // below, and is then refused by markTerm's staged case
      case Some(m) if stage.isDefined && !hasMark(m) && m.tpe.widen.dealias.derivesFrom(freeClass) =>
        Out.Eff(markTerm(m, t.tpe, t.pos), t.tpe.widen)
      case Some(m) =>
        compile(m) match
          case Out.Pure(pm) => Out.Eff(markTerm(pm, t.tpe, t.pos), t.tpe.widen)
          case Out.Eff(cm, ce) => // marks inside the marked value: bind, then mark
            Out.Eff(bind(cm, ce, t.tpe)(v => markTerm(v, t.tpe, t.pos)), t.tpe.widen)
      case None =>
        if hasMark(t) || isPullForeach(t) then compileMarked(t)
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
          case While(_, b) if runnableElemT(b.tpe).isDefined =>
            compileMarked(t)
          case _ => Out.Pure(t)

  /**
   * A lambda whose body is a PROGRAM OF THIS BLOCK'S ROW: compile
   * that body through the pipeline and keep the lambda. The `try`
   * body's treatment, and the nested def's, and sound for the same
   * reason: the body already ENDS at the block's program type, so
   * binding the marks inside it changes neither the lambda's type
   * nor where it is evaluated.
   *
   * The block's OWN row, not any row: a lambda answering at another
   * row would need that row's `Monad` summoned and its type carried
   * into the pipeline, and the shape that wants this — a `Delim`
   * continuation handler — answers at the row it was written in.
   */
  def programLambda(params: List[ValDef], body: Term): Option[Out] =
    body.tpe.widen.dealias.baseType(freeClass) match
      case AppliedType(_, List(_, e))
        if body.tpe.widen <:< TypeRepr.of[F].appliedTo(e.widen) =>
        // the body is an expression ANSWERING a program, so compiling
        // it gives `F[F[T]]` — one flatMap brings it back to the
        // lambda's own result type
        val compiled: Term = compile(body) match
          case Out.Pure(q) => q
          case Out.Eff(c, ce) => bind(c, ce, e.widen)(v => v)
        Some(Out.Pure(Lambda(Symbol.spliceOwner,
          MethodType(params.map(_.name))(_ => params.map(_.tpt.tpe), _ => body.tpe.widen),
          (owner, args) =>
            val m = new TreeMap:
              override def transformTerm(tree: Term)(o: Symbol): Term = tree match
                case id: Ident if params.exists(_.symbol == id.symbol) =>
                  args(params.indexWhere(_.symbol == id.symbol)).asInstanceOf[Term]
                case _ => super.transformTerm(tree)(o)
            m.transformTerm(compiled)(owner).changeOwner(owner))))
      case _ => None

  /** t contains marks below the root — dispatch on shape */
  def compileMarked(t: Term): Out = t match
    // whitelisted combinators FIRST — for-do and for-yield desugar
    // to foreach/map with a lambda, and the general lambda refusal
    // below must not claim them
    case FoldCall(xs, z, accP, elemP, body) if hasMark(body) || hasMark(z) || loopHasMark(xs, body) =>
      foldLoop(t, xs, z, accP, elemP, body)
    case HofCall(xs, nm, param, lbody) if loopNames(nm) && loopHasMark(xs, lbody) =>
      hofLoop(t, xs, nm, param, lbody)
    // a source's loop fires on the RECEIVER'S TYPE, marks or not: an
    // unmarked `Pull` loop is a `Unit ! G` in statement position,
    // which does not run bare unless G is the whole row (v3)
    case HofCall(xs, "foreach", param, lbody) if isPull(peelGuards(xs)._1) =>
      hofLoop(t, xs, "foreach", param, lbody)

    // BEFORE Block: a Lambda IS Block(DefDef :: Nil, Closure), and
    // the block case would claim it with a vaguer message
    case l @ Lambda(params, body) =>
      // A lambda whose RESULT IS A PROGRAM is compiled as its own
      // sub-block (direct-program-lambda, 2026-09-16) — the `try`
      // body's treatment, and the nested def's. It is sound for the
      // same reason both of those are: the body already ENDS at a
      // program type, so binding the marks inside it changes neither
      // the lambda's type nor where it is evaluated; the macro only
      // rewrites what is already there. This is what lets a
      // continuation handler read as ordinary code —
      // `Delim.shift(p)(k => { "x".tell; !k(n) })` with no inner
      // block. Every other lambda keeps the refusal: rewriting a
      // higher-order argument generically is the expensive half of
      // the problem, and the refusal is the whole difference between
      // these few hundred lines and a CPS transformer.
      programLambda(params, body).getOrElse(refuse(l, "under a lambda"))
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
      // The body is statement position (bare ops of the block RUN)
      // and compiles against `loop()` as its tail (direct-tail-
      // fusion): one bind per iteration for the fused shapes, the
      // hand-written recursion exactly.
      compile(cond) match
        case Out.Pure(pc0) =>
          // a PURE condition needs no bind: a plain `if` per
          // iteration (direct-flatmap-emission fusion #1)
          val pc = pc0.changeOwner(Symbol.spliceOwner)
          Out.Eff('{
            def loop(): F[Unit] =
              if ${ pc.asExprOf[Boolean] } then
                ${
                  compileTail(wbody, () => '{ loop() }.asTerm, TypeRepr.of[Unit])
                    .changeOwner(Symbol.spliceOwner).asExprOf[F[Unit]]
                }
              else $M.pure(())
            loop()
          }.asTerm, TypeRepr.of[Unit])
        case Out.Eff(cf0, _) =>
          val cf = cf0.changeOwner(Symbol.spliceOwner)
          Out.Eff('{
            def loop(): F[Unit] =
              $M.flatMap[Boolean](${ cf.asExprOf[F[Boolean]] })[Unit]((c: Boolean) =>
                if c then
                  ${
                    compileTail(wbody, () => '{ loop() }.asTerm, TypeRepr.of[Unit])
                      .changeOwner(Symbol.spliceOwner).asExprOf[F[Unit]]
                  }
                else $M.pure(()))
            loop()
          }.asTerm, TypeRepr.of[Unit])
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
          val raw = DirectCompiler.pipeline[F, B](b.changeOwner(Symbol.spliceOwner), M, eager, parallel, stage)
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
              val hp = DirectCompiler.pipeline[F, H](c.rhs.changeOwner(Symbol.spliceOwner), M, eager, parallel, stage)
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
    case Apply(_, _) | TypeApply(_, _) | Select(_, _) =>
      spineSlots(t) match
        case Some((slots, rebuild)) => anf(slots, t.tpe)(rebuild)
        case None => refuse(t, "in a call shape v1 does not rewrite")

    case Typed(e, tp) => anf(List(e), t.tpe) {
      case e2 :: Nil => Typed.copy(t)(e2, tp)
      case other => report.errorAndAbort(s"direct: one slot expected, got ${other.length} (macro bug)")
    }

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
        // a VARARGS argument — `s"..${x}.."` is StringContext.s(args*) —
        // arrives as Typed(Repeated(elems)); its ELEMENTS are the slots,
        // and the Repeated is rebuilt around their replacements
        // (direct-tell, 2026-09-16: a mark inside an interpolation was
        // "unsupported position (SeqLiteral)" before)
        val argSlots: List[(List[Term], List[Term] => Term)] = args.map {
          case ty @ Typed(rep @ Repeated(elems, et), tpt) =>
            (elems, es => Typed.copy(ty)(Repeated.copy(rep)(es, et), tpt))
          case rep @ Repeated(elems, et) =>
            (elems, es => Repeated.copy(rep)(es, et))
          case a => (List(a), {
            case x :: Nil => x
            case other => report.errorAndAbort(s"direct: one slot expected, got ${other.length} (macro bug)")
          })
        }
        (fs ++ argSlots.flatMap(_._1), vs => {
          val (fvs, rest0) = vs.splitAt(fs.length)
          var rest = rest0
          val newArgs = argSlots.map { (ss, rb) =>
            val (mine, r) = rest.splitAt(ss.length)
            rest = r
            rb(mine)
          }
          Apply.copy(t)(fr(fvs), newArgs)
        })
      }
    case TypeApply(fun, targs) =>
      spineSlots(fun).map((fs, fr) => (fs, vs => TypeApply.copy(t)(fr(vs), targs)))
    case sel @ Select(qual, nm) =>
      Some((List(qual), {
        case q :: Nil => Select.copy(sel)(q, nm)
        case other => report.errorAndAbort(s"direct: one slot expected, got ${other.length} (macro bug)")
      }))
    case id: Ident => Some((Nil, _ => id))
    case _ => None

  /** a child whose evaluation nobody can observe being moved */
  @tailrec def trivial(t: Term): Boolean = t match
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

  // ------------------------------------------------------------
  // INDEPENDENT BINDS, RUN TOGETHER (specs/applicative-static.md,
  // stage 3; off unless `import Direct.parallelBinds.given`).
  //
  // The analysis is bracket abstraction's own question, and Turner
  // answered it in 1979 for the same syntax: `[x](a b)` needs `S`
  // when x occurs free on both sides and `K` when it does not. Here
  // a val's right-hand side either mentions a name bound earlier in
  // the run or it does not; if it does, the run ends there.

  /** a Block with statements: fold vals/exprs into binds */
  def compileBlock(stats: List[Statement], expr: Term): Out =
    val run = if parallel then independentRun(stats) else Nil
    if run.length >= 2 then parallelGroup(run, stats.drop(run.length), expr)
    else compileBlockSeq(stats, expr)

  /** the one-after-another road, which is all there was before
   * stage 3 and is still what runs without the import */
  def compileBlockSeq(stats: List[Statement], expr: Term): Out =
    stats match
      case Nil => compile(expr)
      case (vd @ ValDef(name, tpt, Some(rhs))) :: rest =>
        val out0 = compile(rhs)
        val colourless = colourlessVal(vd, out0, rest, expr) { (elem, out) =>
          if vd.symbol.flags.is(Flags.Lazy) then
            val (defs, use) = lazyOnce(vd, rhs, out, elem)
            val (rest2, expr2) = substUses(rest, expr, vd.symbol, use)
            compileBlock(rest2, expr2) match
              case Out.Pure(q) => Out.Pure(Block(defs, q))
              case Out.Eff(c, e) => Out.Eff(Block(defs, c), e)
          else
            val sym = Symbol.newVal(Symbol.spliceOwner, vd.name, elem.widen,
              Flags.EmptyFlags, Symbol.noSymbol)
            val (rest2, expr2) = substUses(rest, expr, vd.symbol, () => Ref(sym))
            Out.Eff(bind(asF(out), elem, expr.tpe) { v =>
              Block(List(ValDef(sym, Some(v))), asF(compileBlock(rest2, expr2)))
            }, expr.tpe.widen)
        }
        colourless.getOrElse(out0 match
          case Out.Pure(p) =>
            wrapPure(vd, p, rest, expr)
          case out @ Out.Eff(_, _) if vd.symbol.flags.is(Flags.Lazy) =>
            val (defs, use) = lazyOnce(vd, rhs, out)
            val (rest2, expr2) = substUses(rest, expr, vd.symbol, use)
            compileBlock(rest2, expr2) match
              case Out.Pure(q) => Out.Pure(Block(defs, q))
              case Out.Eff(c, e) => Out.Eff(Block(defs, c), e)
          case Out.Eff(c, e) =>
            // the val KEEPS its symbol, re-bound to the continuation's
            // parameter: a later def or an assignment (for a var)
            // still refers to it — substitution would strand them
            Out.Eff(bind(c, e, expr.tpe) { v =>
              val vd2 = ValDef.copy(vd)(vd.name, vd.tpt, Some(v))
              // AT the result type, not at the continuation's own: an
              // inline call's proxy block ends in a program value whose
              // type is the PRECISE constructor (`Free.Inject[R, A]`),
              // and a `pure` emitted there does not match the bind's
              // `F[B]` (direct-colourless-val, caught by a handler whose
              // second lookup takes the first's answer)
              asFAt(wrapStat(vd2, rest, expr), expr.tpe)
            }, expr.tpe.widen))
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
      case (dd: DefDef) :: rest if hasMark(dd) &&
        nestedProgramDef(dd, rest, expr)((_, _, _) => Out.Pure(Literal(UnitConstant()))).isDefined =>
        nestedProgramDef(dd, rest, expr) { (defn, rest2, expr2) =>
          wrapStat(defn, rest2, expr2)
        }.get
      // `val _ = m.!?` desugars to `m.!? match { case _ => () }` — a
      // DISCARD. The general road below bound the mark, matched its
      // value into a `pure(())`, and bound THAT again: a match lambda,
      // a pure closure and a second flatMap per statement. Bind the
      // mark straight into the rest instead. Found on the staged
      // carrier (direct-staged), where those five objects were most of
      // the gap to the hand-written program; a Free block pays the
      // same three nodes fewer
      case (m @ Match(scrut, List(CaseDef(Wildcard(), None, body)))) :: rest
        if hasMark(scrut) && (stripped(body) match { case Literal(UnitConstant()) => true; case _ => false }) =>
        compile(scrut) match
          case Out.Eff(c, e) =>
            Out.Eff(bind(c, e, expr.tpe)(_ => asF(compileBlock(rest, expr))), expr.tpe.widen)
          case Out.Pure(p) => wrapStat(Match.copy(m)(p, m.cases), rest, expr)
      // an `import` rides along, see stmtsTail
      case (im: Import) :: rest => wrapStat(im, rest, expr)
      case (dd: Definition) :: rest =>
        if hasMark(dd) then refuse(dd, "inside a nested definition")
        wrapStat(dd, rest, expr)
      // a generator block's for-yield in statement position emits
      case (st: Term) :: rest if yieldLoop(st).isDefined =>
        val (xs, param, body) = yieldLoop(st).get
        hofLoop(st, xs, "foreach", param, body) match
          case Out.Eff(c, e) => Out.Eff(bind(c, e, expr.tpe)(_ => asF(compileBlock(rest, expr))), expr.tpe.widen)
          case Out.Pure(p) => wrapStat(p, rest, expr)
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

  /** keep a pure statement in front of the compiled rest */
  def wrapStat(s: Statement, rest: List[Statement], expr: Term): Out =
    compileBlock(rest, expr) match
      case Out.Pure(p) => Out.Pure(Block(List(s), p))
      case Out.Eff(c, e) => Out.Eff(Block(List(s), c), e)

  def wrapPure(vd: ValDef, rhs: Term, rest: List[Statement], expr: Term): Out =
    wrapStat(ValDef.copy(vd)(vd.name, vd.tpt, Some(rhs)), rest, expr)
