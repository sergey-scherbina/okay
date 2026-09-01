package okay

import scala.quoted.*
import scala.annotation.implicitNotFound

/**
 * The flat block (specs/direct-macro.md): `direct[F] { ... m.reflect ... }`
 * rewrites a plain block into the Cont binds of Monadic
 * (specs/monadic-reflection.md), so monadic values read as plain
 * values with no for-comprehension. The macro adds SYNTAX only —
 * every emitted program is one the user could write with
 * reflect/reify by hand; multi-shot, short-circuit and the stack
 * discipline of the reflected monad are inherited, not
 * re-implemented. v1 is scoped: marks under a lambda or a by-name
 * argument, and `while`/`try` around marks, are compile errors with
 * the workaround named — refusing that corner is the entire
 * difference between these few hundred lines and a general CPS
 * transformer.
 */
object Direct:

  extension [F[_], A](m: F[A])
    /**
     * The mark: typechecks as A so the block typechecks BEFORE the
     * macro expands; the macro rewrites every call, so this body
     * only runs when a mark escapes outside a direct block — and
     * then it fails loudly rather than compiling to nothing.
     * The mark is a NAME, deliberately: three symbols were tried
     * and refuted (.! shadows object !, .!? was redundant, .reflect was
     * ambiguous with the Throws row-?) — the retirement is recorded
     * in specs/direct-macro.md.
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

  /** rewrite the block: marks become Monadic binds, the result is
   * F[A]. `direct[F] { block }` names only the monad (the partial
   * type application trick); with an expected type both infer:
   * `val p: Int ! W = direct { ... }`. The block is a context
   * function so DirectCtx is ambient in it (plain blocks adapt);
   * the context lambda is stripped by the macro, never called. */
  inline def direct[F[_]]: DirectApply[F] = DirectApply[F]()

  final class DirectApply[F[_]](private val unit: Unit = ()) extends AnyVal:
    inline def apply[A](inline block: DirectCtx[F] ?=> A)(using inline M: Monad[F]): F[A] =
      ${ directImpl[F, A]('block, 'M) }

  private def directImpl[F[_] : Type, A: Type](block: Expr[DirectCtx[F] ?=> A],
                                               M: Expr[Monad[F]])
                                              (using Quotes): Expr[F[A]] =
    import quotes.reflect.*
    // the block arrives as a context lambda; take its body — the
    // lambda is never called (see the entry note below)
    def strippedTop(t: Term): Term = t match
      case Inlined(_, Nil, inner) => strippedTop(inner)
      case Typed(inner, _) => strippedTop(inner)
      case _ => t
    val topBody: Term = strippedTop(block.asTerm) match
      case Block(List(dd: DefDef), _: Closure) =>
        dd.rhs.getOrElse(report.errorAndAbort("empty direct block"))
          .changeOwner(Symbol.spliceOwner)
      case other => report.errorAndAbort(
        "a Direct mark as a non-literal block (a stored context-function value) " +
          "cannot be rewritten by direct's v1", other.pos)
    pipeline[F, A](topBody, M)

  /** the compilation pipeline at ONE answer type — recursive for
   * try bodies (direct-try): a try's body is its own sub-block,
   * reified at the try's type, then reflected as one mark */
  private def pipeline[F[_] : Type, A: Type](using q: Quotes)(topLevelBody: q.reflect.Term,
                                             M: Expr[Monad[F]]): Expr[F[A]] =
    import q.reflect.*

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

    def stripped(t: Term): Term = t match
      case Inlined(_, Nil, inner) => stripped(inner)
      case Typed(inner, _) => stripped(inner)
      case _ => t

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
        case AppliedType(f, List(row, _)) if f.typeSymbol.name == "Free" => Some(row)
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

    type CA = Cont[A, F[A], F[A]]

    /**
     * Compile a term to either a pure term (no marks) or an
     * Expr[Cont[T, F[A], F[A]]] for the term's type T. Pure children
     * that precede an effectful child are NOT reordered: the ANF
     * hoisting below binds children left to right.
     */
    enum Out:
      case Pure(t: Term)
      case Eff(c: Term) // : Cont[T, F[A], F[A]] where T = the source term's type

    def contOf(tpe: TypeRepr): TypeRepr =
      TypeRepr.of[Cont].appliedTo(List(tpe.widen, TypeRepr.of[F[A]], TypeRepr.of[F[A]]))

    /** Cont.Pure(t) at t's type */
    def pureCont(t: Term): Term =
      tpe2(t.tpe.widen) { [T] => (_: Type[T]) ?=>
        '{ Cont.Pure[T, F[A]](${ t.asExprOf[T] }) }.asTerm
      }

    /** run f with the TypeRepr as a Type given */
    def tpe2[R](tpe: TypeRepr)(f: [T] => Type[T] ?=> R): R =
      tpe.asType match
        case '[t] => f[t]
      end match

    /** m.reflect over Monadic at element type `elem`:
     * Expr[Cont[elem, F[A], F[A]]] — m must already be F[elem] */
    def reflectTerm(m: Term, elem: TypeRepr): Term =
      tpe2(elem.widen) { [T] => (_: Type[T]) ?=>
        '{
          import okay.Monadic.{reflect as reflected}
          given Monad[F] = $M
          ${ m.asExprOf[F[T]] }.reflected[A]
        }.asTerm
      }

    /**
     * ONE mark, dispatched by type: an F[elem] of this block
     * reflects; an operation of this block's row is injected into
     * the row program first, then reflects. Anything else is refused
     * with both possibilities named.
     */
    def markTerm(m: Term, elem: TypeRepr, at: Position): Term =
      val fT = TypeRepr.of[F].appliedTo(elem.widen)
      if m.tpe <:< fT then reflectTerm(m, elem)
      else rowOf match
        case Some(row) if m.tpe <:< row.appliedTo(elem.widen) =>
          reflectTerm(injectTerm(m, elem, row), elem)
        case _ =>
          report.errorAndAbort(
            s"the marked value has type ${m.tpe.show} — neither this block's ${fT.show}" +
              rowOf.fold("")(r => s" nor an operation of its row ${r.show}"), at)

    /** cont.flatMap(v => body(v)) — body built from a reference to v */
    def bind(cont: Term, vTpe: TypeRepr, resTpe: TypeRepr)(body: Term => Term): Term =
      tpe2(vTpe.widen) { [T] => (_: Type[T]) ?=>
        tpe2(resTpe.widen) { [B] => (_: Type[B]) ?=>
          val c = cont.asExprOf[Cont[T, F[A], F[A]]]
          '{
            $c.flatMap[B, F[A]]((v: T) =>
              ${ body('v.asTerm).changeOwner(Symbol.spliceOwner)
                   .asExprOf[Cont[B, F[A], F[A]]] })
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

    /** a term in STATEMENT position, as a Cont: marks compile, and a
     * markless value of the block's own effectful type RUNS — the
     * do-notation reading, so `for t <- xs do Writer(t)` tells
     * instead of silently building and dropping the op */
    def statementCont(t: Term): Term =
      compile(t) match
        case Out.Eff(c) => c
        case Out.Pure(p) =>
          runnableElem(p) match
            case Some(el) => markTerm(p, el, t.pos)
            case None => pureCont(p)

    /** the value type a Cont term carries */
    def contValue(c: Term): TypeRepr = c.tpe.widen.dealias match
      case AppliedType(_, List(v, _, _)) => v
      case other => report.errorAndAbort(s"not a Cont: ${other.show}")

    /** compile the loop body at the emitted binder h, with statement
     * semantics, erased to Cont[Unit, ...] (foreach drops values) */
    def loopBody(param: ValDef, lbody: Term)(h: Term): Term =
      val c = statementCont(subst(lbody, param.symbol, h))
        .changeOwner(Symbol.spliceOwner)
      tpe2(contValue(c)) { [V] => (_: Type[V]) ?=>
        '{
          ${ c.asExprOf[Cont[V, F[A], F[A]]] }
            .flatMap[Unit, F[A]](_ => Cont.Pure[Unit, F[A]](()))
        }.asTerm
      }

    /** compile the yield body at the emitted binder h (values kept —
     * no statement semantics in a yield) */
    def yieldBody(param: ValDef, lbody: Term)(h: Term): Term =
      asCont(compile(subst(lbody, param.symbol, h)), lbody.tpe)
        .changeOwner(Symbol.spliceOwner)

    /** for x <- xs do body — run per element, in order; the loop
     * recurses over an IMMUTABLE List so multi-shot re-entry is
     * sound, and through flatMap so Bind spill keeps it stack-safe */
    def foreachLoop(xs: Term, param: ValDef, lbody: Term): Term =
      tpe2(param.tpt.tpe.widen) { [T] => (_: Type[T]) ?=>
        '{
          val items: List[T] = ${ iteratorOf(xs).asExprOf[Iterator[T]] }.toList
          def loop(rest: List[T]): Cont[Unit, F[A], F[A]] = rest match
            case Nil => Cont.Pure[Unit, F[A]](())
            case h :: tl =>
              ${ loopBody(param, lbody)('h.asTerm).asExprOf[Cont[Unit, F[A], F[A]]] }
                .flatMap[Unit, F[A]](_ => loop(tl))
          loop(items)
        }.asTerm
      }

    /** for x <- xs yield body — the traverse shape; results come out
     * as a List, accepted where the node's type allows it */
    def mapLoop(t: Term, xs: Term, param: ValDef, lbody: Term): Term =
      tpe2(param.tpt.tpe.widen) { [T] => (_: Type[T]) ?=>
        tpe2(lbody.tpe.widen) { [U] => (_: Type[U]) ?=>
          if !(TypeRepr.of[List[U]] <:< t.tpe.widen) then
            refuse(t, s"in a for-yield whose collection type ${t.tpe.widen.show} cannot hold a List " +
              "(v1 yields a List; .toList the receiver or collect explicitly)")
          '{
            val items: List[T] = ${ iteratorOf(xs).asExprOf[Iterator[T]] }.toList
            def loop(rest: List[T], acc: List[U]): Cont[List[U], F[A], F[A]] = rest match
              case Nil => Cont.Pure[List[U], F[A]](acc.reverse)
              case h :: tl =>
                ${ yieldBody(param, lbody)('h.asTerm).asExprOf[Cont[U, F[A], F[A]]] }
                  .flatMap[List[U], F[A]](b => loop(tl, b :: acc))
            loop(items, Nil)
          }.asTerm
        }
      }

    /** the loop shapes, receiver hoisted first if it is marked */
    def hofLoop(t: Term, xs: Term, nm: String, param: ValDef, lbody: Term): Out =
      def emit(xsPure: Term): Term = nm match
        case "foreach" => foreachLoop(xsPure, param, lbody)
        case "map" => mapLoop(t, xsPure, param, lbody)
      if hasMark(xs) then
        compile(xs) match
          case Out.Eff(c) => Out.Eff(bind(c, xs.tpe, t.tpe)(v => emit(v)))
          case Out.Pure(p) => Out.Eff(emit(p))
      else Out.Eff(emit(xs))

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
            case Out.Pure(pm) => Out.Eff(markTerm(pm, t.tpe, t.pos))
            case Out.Eff(cm) => // marks inside the marked value: bind, then mark
              Out.Eff(bind(cm, m.tpe, t.tpe)(v => markTerm(v, t.tpe, t.pos)))
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
        val thC = asCont(compile(th), th.tpe)
        val elC = asCont(compile(el), el.tpe)
        val branchTpe = t.tpe
        compile(c) match
          case Out.Pure(pc) =>
            Out.Eff(If.copy(t)(pc, asContAt(thC, branchTpe), asContAt(elC, branchTpe)))
          case Out.Eff(cc) =>
            Out.Eff(bind(cc, c.tpe, branchTpe) { v =>
              If.copy(t)(v, asContAt(thC, branchTpe), asContAt(elC, branchTpe))
            })

      case Match(scrut, cases) =>
        val branchTpe = t.tpe
        def casesC = cases.map { cd =>
          val bodyC = asContAt(asCont(compile(cd.rhs), cd.rhs.tpe), branchTpe)
          CaseDef.copy(cd)(cd.pattern, cd.guard.map {
            g => if hasMark(g) then refuse(g, "in a pattern guard") else g
          }, bodyC)
        }
        compile(scrut) match
          case Out.Pure(ps) => Out.Eff(Match.copy(t)(ps, casesC))
          case Out.Eff(cs) =>
            Out.Eff(bind(cs, scrut.tpe, branchTpe)(v => Match.copy(t)(v, casesC)))

      // Boolean && / || are compiler intrinsics (their method type is
      // by-value, the short-circuit is magic) — desugar to the If they
      // mean and recurse, keeping the short-circuit
      case Apply(sel @ Select(l, "&&"), List(r)) if sel.symbol.owner == defn.BooleanClass =>
        compile(If(l, r, Literal(BooleanConstant(false)))) match
          case out @ Out.Eff(_) => out
          case Out.Pure(p) => Out.Pure(p)
      case Apply(sel @ Select(l, "||"), List(r)) if sel.symbol.owner == defn.BooleanClass =>
        compile(If(l, Literal(BooleanConstant(true)), r)) match
          case out @ Out.Eff(_) => out
          case Out.Pure(p) => Out.Pure(p)

      case While(cond, wbody) =>
        // cond and body splice INSIDE def loop, so they re-evaluate
        // per iteration by construction; recursion rides Bind spill
        val condC = asCont(compile(cond), cond.tpe).changeOwner(Symbol.spliceOwner)
        // the body is statement position: bare ops of the block RUN
        val bodyC = statementCont(wbody).changeOwner(Symbol.spliceOwner)
        tpe2(contValue(bodyC)) { [U] => (_: Type[U]) ?=>
          Out.Eff('{
            def loop(): Cont[Unit, F[A], F[A]] =
              ${ condC.asExprOf[Cont[Boolean, F[A], F[A]]] }
                .flatMap[Unit, F[A]](c =>
                  if c then
                    ${ bodyC.asExprOf[Cont[U, F[A], F[A]]] }
                      .flatMap[Unit, F[A]](_ => loop())
                  else Cont.Pure[Unit, F[A]](()))
            loop()
          }.asTerm)
        }
      case tr @ Try(b, cases, fin) =>
        // direct-try: the body is its own sub-block, reified at the
        // try's type through the recursive pipeline; the whole try
        // becomes ONE mark over CanTry's seam. The catch bodies stay
        // pure (marks there remain v2); finalizers likewise.
        if fin.isDefined then refuse(tr, "with a finalizer (direct-try v1)")
        if cases.exists(c => hasMark(c.rhs)) then
          refuse(tr, "with a mark inside a catch body (direct-try v1)")
        // literal branches make the try's type a UNION of singletons
        // (0 | 7): join it by hand — the sub-block reifies at the join
        def joinUnions(t: TypeRepr): TypeRepr = t.dealias match
          case OrType(a, b) =>
            val (ja, jb) = (joinUnions(a).widen, joinUnions(b).widen)
            if ja =:= jb then ja
            else OrType(ja, jb)
          case other => other.widen
        val bT = joinUnions(tr.tpe.widen)
        tpe2(bT) { [T] => (_: Type[T]) ?=>
          val bodyT = joinUnions(b.tpe.widen)
          val subF: Term = tpe2(bodyT) { [B] => (_: Type[B]) ?=>
            val raw = pipeline[F, B](b.changeOwner(Symbol.spliceOwner), M)
            // a body ending in throw types Nothing <: T: upcast
            // through the monad (F need not be covariant)
            if bodyT =:= TypeRepr.of[T] then raw.asTerm
            else '{
              given Monad[F] = $M
              ${ raw }.flatMap((x: B) => $M.pure[T](x.asInstanceOf[T]))
            }.asTerm
          }
          val handler: Term = '{ (e: Throwable) =>
            ${ Match('{ e }.asTerm,
                 cases.map(c => CaseDef.copy(c)(c.pattern, c.guard,
                   '{ $M.pure[T](${ c.rhs.asExprOf[T] }) }.asTerm))
                 :+ CaseDef(Wildcard(), None, '{ throw e }.asTerm)
               ).asExprOf[F[T]] }
          }.asTerm
          val guarded: Term = '{
            scala.compiletime.summonInline[CanTry[F]]
              .tryIn[T](${ subF.asExprOf[F[T]] })(${ handler.asExprOf[Throwable => F[T]] })
          }.asTerm
          Out.Eff(reflectTerm(guarded, bT))
        }

      // application shapes: ANF-hoist children left to right
      // an assignment with a marked rhs: bind, then assign the value
      case Assign(lhs, rhs) =>
        compile(rhs) match
          case Out.Pure(p) => Out.Pure(Assign.copy(t)(lhs, p))
          case Out.Eff(c) =>
            Out.Eff(bind(c, rhs.tpe, TypeRepr.of[Unit]) { v =>
              pureCont(Assign.copy(t)(lhs, v))
            })

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

    /** hoist the children: pure ones pass through unless an effectful
     * one precedes them (order!), effectful ones bind */
    def anf(children: List[Term], resTpe: TypeRepr)(rebuild: List[Term] => Term): Out =
      val firstEff = children.indexWhere(hasMark)
      def loop(rest: List[Term], i: Int, acc: List[Term]): Term =
        rest match
          case Nil => pureCont(rebuild(acc.reverse))
          case c :: tail =>
            if i < firstEff || !hasMark(c) then loop(tail, i + 1, c :: acc)
            else compile(c) match
              case Out.Pure(p) => loop(tail, i + 1, p :: acc)
              case Out.Eff(ce) =>
                bind(ce, c.tpe, resTpe) { v =>
                  loop(tail, i + 1, v :: acc)
                }
      Out.Eff(loop(children, 0, Nil))

    /** a Block with statements: fold vals/exprs into binds */
    def compileBlock(stats: List[Statement], expr: Term): Out =
      stats match
        case Nil => compile(expr)
        case (vd @ ValDef(name, tpt, Some(rhs))) :: rest =>
          if vd.symbol.flags.is(Flags.Lazy) && hasMark(rhs) then refuse(vd, "in a lazy val")
          compile(rhs) match
            case Out.Pure(p) =>
              wrapPure(vd, p, rest, expr)
            case Out.Eff(c) =>
              Out.Eff(bind(c, rhs.tpe, expr.tpe) { v =>
                val restT = compileBlock(rest.map(substStat(_, vd.symbol, v)),
                  subst(expr, vd.symbol, v))
                asCont(restT, expr.tpe)
              })
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
                    asCont(compileBlock(rest, expr), expr.tpe)
                  })
                case None =>
                  // a FOREIGN marked type can be neither run nor
                  // meaningfully dropped
                  if discardedMonadic(p.tpe) then
                    report.errorAndAbort(
                      s"a value of ${p.tpe.widen.show} is discarded in statement position, " +
                        "and it is neither this block's monad nor an operation of its row — " +
                        "it cannot run here; bind it or move it to its own block", st.pos)
                  wrapStat(p, rest, expr)
            case Out.Eff(c) =>
              Out.Eff(bind(c, st.tpe, expr.tpe) { _ =>
                asCont(compileBlock(rest, expr), expr.tpe)
              })
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

    def substStat(s: Statement, sym: Symbol, ref: Term): Statement = s match
      case t: Term => subst(t, sym, ref)
      case vd @ ValDef(n, tp, rhs) => ValDef.copy(vd)(n, tp, rhs.map(subst(_, sym, ref)))
      case _ => s

    /** keep a pure statement in front of the compiled rest */
    def wrapStat(s: Statement, rest: List[Statement], expr: Term): Out =
      compileBlock(rest, expr) match
        case Out.Pure(p) => Out.Pure(Block(List(s), p))
        case Out.Eff(c) => Out.Eff(Block(List(s), c))

    def wrapPure(vd: ValDef, rhs: Term, rest: List[Statement], expr: Term): Out =
      val vd2 = ValDef.copy(vd)(vd.name, vd.tpt, Some(rhs))
      compileBlock(rest, expr) match
        case Out.Pure(p) => Out.Pure(Block(List(vd2), p))
        case Out.Eff(c) => Out.Eff(Block(List(vd2), c))

    def asCont(o: Out, tpe: TypeRepr): Term = o match
      case Out.Eff(c) => c
      case Out.Pure(p) => pureCont(p)

    /** widen a Cont's value type to the join the branches share */
    def asContAt(c: Term, tpe: TypeRepr): Term =
      tpe2(tpe.widen) { [T] => (_: Type[T]) ?=>
        Typed(c, TypeTree.of[Cont[T, F[A], F[A]]])
      }

    val res: Expr[Cont[A, F[A], F[A]]] =
      asCont(compile(topLevelBody), TypeRepr.of[A]).asExprOf[Cont[A, F[A], F[A]]]
    '{ Monadic.reify[F, A, A]($res)(using $M) }
