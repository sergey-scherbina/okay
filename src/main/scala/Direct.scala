package okay

import scala.quoted.*

/**
 * The flat block (specs/direct-macro.md): `direct[F] { ... m.? ... }`
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
     */
    def ? : A = throw new IllegalStateException(
      "Direct.? outside a direct block — wrap the code in direct[F] { ... }")
    /** the named spelling of the same mark */
    def reflect: A = m.?

  extension [G[_], A](op: G[A])
    /**
     * The operation mark: `op.!?` is `effect(op).?` — lifts a raw
     * effect operation into the block's row program and reflects it,
     * so `Writer("a").!?` needs no effect[Row, A] spelling. Valid
     * only in a block whose F is a program monad (`A ! Row`).
     */
    def !? : A = throw new IllegalStateException(
      "Direct.!? outside a direct block — wrap the code in direct[F] { ... }")

  /** rewrite the block: marks become Monadic binds, the result is
   * F[A]. `direct[F] { block }` names only the monad (the partial
   * type application trick); with an expected type both infer:
   * `val p: Int ! W = direct { ... }`. */
  inline def direct[F[_]]: DirectApply[F] = DirectApply[F]()

  final class DirectApply[F[_]](private val unit: Unit = ()) extends AnyVal:
    inline def apply[A](inline block: A)(using inline M: Monad[F]): F[A] =
      ${ directImpl[F, A]('block, 'M) }

  private def directImpl[F[_] : Type, A: Type](block: Expr[A], M: Expr[Monad[F]])
                                              (using Quotes): Expr[F[A]] =
    import quotes.reflect.*

    val directSym = TypeRepr.of[Direct.type].typeSymbol
    val markSyms = (directSym.methodMember("?") ++ directSym.methodMember("reflect")).toSet
    val opMarkSyms = directSym.methodMember("!?").toSet

    /** the mark's receiver, if this term is a mark call */
    def asMark(t: Term): Option[Term] = t match
      case Apply(TypeApply(fun, _), List(m)) if markSyms(fun.symbol) => Some(m)
      case _ => None

    /** the operation mark's receiver, if this term is an op-mark call */
    def asOpMark(t: Term): Option[Term] = t match
      case Apply(TypeApply(fun, _), List(m)) if opMarkSyms(fun.symbol) => Some(m)
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
            case term: Term if asMark(term).isDefined || asOpMark(term).isDefined =>
              found = true
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
    def injectTerm(op: Term, elem: TypeRepr, at: Position): Term =
      val row = rowOf.getOrElse(report.errorAndAbort(
        s"op.!? needs an effect-row block (its F must be a program monad A ! Row); " +
          s"this block's F[Unit] is ${TypeRepr.of[F[Unit]].dealias.show}", at))
      val expected = row.appliedTo(elem.widen)
      if !(op.tpe <:< expected) then
        report.errorAndAbort(
          s"the operation has type ${op.tpe.show}, which is not in this block's row: " +
            expected.show, at)
      Apply(TypeApply(Ref(injectApply), List(Inferred(row), Inferred(elem.widen))), List(op))

    def refuse(t: Tree, where: String): Nothing =
      report.errorAndAbort(
        s"a Direct mark (.? / .reflect) $where cannot be rewritten by direct's v1 — " +
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
     * Expr[Cont[elem, F[A], F[A]]] */
    def reflectTerm(m: Term, elem: TypeRepr): Term =
      tpe2(elem.widen) { [T] => (_: Type[T]) ?=>
        if !(m.tpe <:< TypeRepr.of[F[T]]) then
          report.errorAndAbort(
            s"the marked value has type ${m.tpe.show}, not this block's ${TypeRepr.of[F[T]].show}" +
              " — in an effect-row block spell operations at the row: effect[Row, A](op).?",
            m.pos)
        '{
          import okay.Monadic.{reflect as reflected}
          given Monad[F] = $M
          ${ m.asExprOf[F[T]] }.reflected[A]
        }.asTerm
      }

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
            case Out.Pure(pm) => Out.Eff(reflectTerm(pm, t.tpe))
            case Out.Eff(cm) => // marks inside the marked value: bind, then reflect
              Out.Eff(bind(cm, m.tpe, t.tpe)(v => reflectTerm(v, t.tpe)))
        case None => asOpMark(t) match
          case Some(op) =>
            compile(op) match
              case Out.Pure(p) =>
                Out.Eff(reflectTerm(injectTerm(p, t.tpe, t.pos), t.tpe))
              case Out.Eff(c) =>
                Out.Eff(bind(c, op.tpe, t.tpe)(v =>
                  reflectTerm(injectTerm(v, t.tpe, t.pos), t.tpe)))
          case None =>
            if !hasMark(t) then Out.Pure(t)
            else compileMarked(t)

    /** t contains marks below the root — dispatch on shape */
    def compileMarked(t: Term): Out = t match
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

      case w @ While(_, _) => refuse(w, "inside a while (v2)")
      case tr @ Try(_, _, _) => refuse(tr, "inside a try (v2)")

      // application shapes: ANF-hoist children left to right
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
            case Out.Pure(p) => wrapStat(p, rest, expr)
            case Out.Eff(c) =>
              Out.Eff(bind(c, st.tpe, expr.tpe) { _ =>
                asCont(compileBlock(rest, expr), expr.tpe)
              })
        case other :: _ => refuse(other, "in an unsupported statement")

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
      asCont(compile(block.asTerm), TypeRepr.of[A]).asExprOf[Cont[A, F[A], F[A]]]
    '{ Monadic.reify[F, A, A]($res)(using $M) }
