package okay
package macros

import scala.quoted.*
import scala.language.implicitConversions
import scala.annotation.tailrec
import okay.Direct.*

/** the macros behind `direct` and `Direct.staged` (specs/direct-macro.md; okay-macros-package) */
@scala.annotation.publicInBinary private[okay] object DirectMacros:

  def stagedImpl[Sig[+_] : Type, R: Type, A: Type](st: Expr[Stager[Sig, R]],
                                                                 block: Expr[DirectCtx[Handled[Sig, R, *]] ?=> A],
                                                                 b: Expr[Binds])
                                                                (using Quotes): Expr[Handled[Sig, R, A]] =
    import quotes.reflect.*
    type F[X] = Handled[Sig, R, X]
    val topBody: Term = blockBody[F, A](block).asTerm
    val m = Expr.summon[Monad[F]].getOrElse(
      report.errorAndAbort("direct.staged: no Monad[Handled[Sig, R, *]] (macro bug)"))
    DirectCompiler.pipeline[F, A](topBody, m,
      eager = true,
      parallel = b.asTerm.tpe <:< TypeRepr.of[Binds.Parallel.type],
      stage0 = Some(st.asTerm))

  /** a term with its inlining and ascription wrappers taken off */
  @tailrec def stripped(using q: Quotes)(t: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    t match
      case Inlined(_, Nil, inner) => stripped(inner)
      case Typed(inner, _) => stripped(inner)
      case _ => t

  /** the block arrives as a context lambda; this is its body — the
   * lambda is never called (see the entry note below). An `Expr`, so
   * the phase probes (src/test/scala/DirectProbe.scala) can take the
   * same body into a compiler of their own Quotes path. */
  def blockBody[F[_] : Type, A: Type](block: Expr[DirectCtx[F] ?=> A])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    stripped(block.asTerm) match
      case Block(List(dd: DefDef), _: Closure) =>
        dd.rhs.getOrElse(report.errorAndAbort("empty direct block"))
          .changeOwner(Symbol.spliceOwner).asExpr
      case other => report.errorAndAbort(
        "a Direct mark as a non-literal block (a stored context-function value) " +
          "cannot be rewritten by direct's v1", other.pos)

  def directImpl[F[_] : Type, A: Type](block: Expr[DirectCtx[F] ?=> A],
                                               M: Expr[Applicative[F]],
                                               d: Expr[Deferral],
                                               b: Expr[Binds])
                                              (using Quotes): Expr[F[A]] =
    import quotes.reflect.*
    val topBody: Term = blockBody[F, A](block).asTerm
    Expr.summon[Monad[F]] match
      // a monad: the road every existing block takes, unchanged
      case Some(m) =>
        DirectCompiler.pipeline[F, A](topBody, m,
          d.asTerm.tpe <:< quotes.reflect.TypeRepr.of[Deferral.Eager.type],
          b.asTerm.tpe <:< quotes.reflect.TypeRepr.of[Binds.Parallel.type])
      // no monad: the idiom bracket, for the carriers that refuse one
      case None => applicativeOnly[F, A](topBody, M)

  /**
   * THE IDIOM BRACKET, for a carrier that has no monad
   * (specs/applicative-do.md).
   *
   * `Validated` refuses a `Monad` on purpose — the consistency law
   * would force `app` to agree with the `flatMap` derivation, which
   * stops at the first error and undoes the collecting the type
   * exists for. So the carriers where direct style reads best were
   * exactly the ones it turned away, with a `no Monad[V]` at the call
   * site before the macro ever ran.
   *
   * What a block needs is decided by the block, not by the carrier: a
   * run of INDEPENDENT binds needs only `Applicative`. This road
   * emits precisely that and nothing else —
   *
   *     val a = m1.reflect          fmap(m1, a => b => body)
   *     val b = m2.reflect    ==>     .app(m2)
   *     body
   *
   * — and refuses every other shape by NAME rather than by a type
   * error about a class the author never mentioned. That refusal is
   * the same stance `direct`'s v1 took generally: refuse the hard
   * corner instead of half-solving it. A loop, a conditional, a
   * statement between the binds, or a right-hand side that mentions
   * an earlier bind all need `flatMap`, and there is none.
   */
  private def applicativeOnly[F[_] : Type, A: Type](using q: Quotes)(
      body: q.reflect.Term, AP: Expr[Applicative[F]]): Expr[F[A]] =
    import q.reflect.*

    val directSym = TypeRepr.of[Direct.type].typeSymbol
    val markSyms = (directSym.methodMember("reflect")
      ++ directSym.methodMember("?") ++ directSym.methodMember("unary_!")).toSet
    val colorSyms = (directSym.methodMember("selfColor") ++
      directSym.methodMember("opColor") ++
      Symbol.requiredModule("okay.Freer").methodMember("directColor")).toSet

    def calleeRoot(t: Term): Symbol = t match
      case Apply(f, _) => calleeRoot(f)
      case TypeApply(f, _) => calleeRoot(f)
      case Inlined(_, Nil, inner) => calleeRoot(inner)
      case _ => t.symbol

    def strip(t: Term): Term = t match
      case Inlined(_, Nil, inner) => strip(inner)
      case Typed(inner, _) => strip(inner)
      case _ => t

    def asMark(t: Term): Option[Term] = t match
      case Apply(TypeApply(fun, _), List(m)) if markSyms(fun.symbol) => Some(m)
      case Apply(Select(conv, "apply"), List(x)) if colorSyms(calleeRoot(conv)) => Some(x)
      case _ => None

    def hasMark(t: Tree): Boolean =
      var found = false
      val tr = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          if !found then tree match
            case term: Term if asMark(term).isDefined => found = true
            case _ => super.traverseTree(tree)(owner)
      tr.traverseTree(t)(Symbol.spliceOwner)
      found

    /** the marked program a val binds, with the inliner's proxy
     * bindings kept around it (direct-parallel-wider-rows found that
     * shape and this is the same walk) */
    def leafOf(rhs: Term): Option[Term] =
      def go(t: Term): Option[Term] = strip(t) match
        case Inlined(_, bs, inner) if bs.nonEmpty => go(Block(bs, inner))
        case Block(stats, expr) =>
          go(expr).map(m => if stats.isEmpty then m else Block(stats, m))
        case other => asMark(other).map(strip)
      go(rhs)

    /**
     * `if` WITH AN EFFECTFUL CONDITION is the Selective rung, and it
     * is the one shape an applicative cannot express: `<*>` runs both
     * of its arguments, so a branch would happen whether or not it
     * was taken. `ifS` runs the scrutinee and then AT MOST ONE side
     * (Mokhov et al. 2019), which for a validator is the difference
     * between reporting a bad shipping address on an order that was
     * never going to be shipped and not reporting it.
     *
     * The shape reaching the macro is `If(mark(cond), then, else)`:
     * the condition must typecheck as a Boolean, so a program in that
     * position has already been marked or auto-coloured.
     */
    def isSelectiveIf(t: Term): Boolean = strip(t) match
      case If(c, _, _) => asMark(strip(c)).isDefined
      case _ => false

    def selectiveIf(t: Term): Term = strip(t) match
      case If(c, th, el) =>
        val ce = asMark(strip(c)).getOrElse(
          report.errorAndAbort("direct: selectiveIf on a pure condition (macro bug)", t.pos))
        val sel = Expr.summon[Selective[F]].getOrElse(report.errorAndAbort(
          "direct: an `if` whose CONDITION is an effect needs a Selective for this " +
            "block's carrier — an Applicative alone would have to run BOTH branches, " +
            "which is what `ifS` exists to avoid. Give the carrier a Selective, or " +
            "bind the condition to a val first.", t.pos))
        ifSOf(strip(ce), strip(th), strip(el), sel, t.tpe.widen)
      case other => other

    /** the carrier's element, when this type is the carrier applied */
    def carrierElem(tpe: TypeRepr): Option[TypeRepr] = tpe.widen.dealias match
      case AppliedType(_, args) if args.nonEmpty &&
        TypeRepr.of[F].appliedTo(args.last) =:= tpe.widen.dealias => Some(args.last)
      case _ => None

    def ifSOf(cond: Term, th: Term, el: Term, sel: Expr[Selective[F]], res: TypeRepr): Term =
      val elem = res.dealias match
        case AppliedType(_, args) if args.nonEmpty => args.last
        case other => report.errorAndAbort(s"direct: expected the carrier applied, got ${other.show}")
      tpe2A[F, A](elem) { [X] => (tX: Type[X]) ?=>
        // the type parameter of an extension comes AFTER the receiver
        // (generalized method syntax), so it is left to inference here
        '{ $sel.ifS(${ cond.asExprOf[F[Boolean]] })(
             ${ th.asExprOf[F[X]] })(${ el.asExprOf[F[X]] }) }.asTerm
      }

    def refuse(at: Position, what: String): Nothing =
      report.errorAndAbort(
        s"direct: this block's carrier has an Applicative but no Monad, so it can run " +
          s"INDEPENDENT binds and nothing else — $what needs flatMap. " +
          "Reorder the block, or give the carrier a Monad.", at)

    def mentions(t: Tree, syms: Set[Symbol]): Boolean =
      if syms.isEmpty then false
      else
        var found = false
        val tr = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            if !found then tree match
              case id: Ident if syms.contains(id.symbol) => found = true
              case _ => super.traverseTree(tree)(owner)
        tr.traverseTree(t)(Symbol.spliceOwner)
        found

    // the block, taken apart: a run of marked vals and a markless tail
    val (stats, result) = strip(body) match
      case Block(ss, e) => (ss, e)
      case e => (Nil, e)

    /** one leaf of the bracket: a name to bind, its type, and the
     * program that fills it */
    final case class Leaf(sym: Symbol, name: String, tpe: TypeRepr, prog: Term)

    /**
     * A COLOURLESS VAL: `val n = nonEmpty(raw.name)`, with no mark and
     * no annotation. Its inferred type IS a program of this carrier,
     * and the monadic road has bound such a val since
     * direct-colourless-val — the applicative road refused it, which
     * was an inconsistency of mine and not a limit of the language.
     *
     * The val's own type decides, exactly as it does there: if it is
     * `F[X]`, the right-hand side is the leaf and the name binds at
     * `X`. A rhs that carries marks of its own is not a plain leaf and
     * still takes the other road.
     */
    def colourless(vd: ValDef, rhs: Term): Option[TypeRepr] =
      if hasMark(rhs) then None
      else vd.tpt.tpe.widen.dealias match
        case AppliedType(_, args) if args.nonEmpty &&
          TypeRepr.of[F].appliedTo(args.last) =:= vd.tpt.tpe.widen.dealias => Some(args.last)
        case _ => None

    val fromVals: List[Leaf] =
      stats.map {
        // the Selective rung FIRST: an `if` with an effectful condition
        // is marked (the condition is), so it would otherwise be taken
        // for a mark whose argument the walk cannot find
        case vd @ ValDef(_, _, Some(rhs)) if isSelectiveIf(rhs) =>
          val elem = carrierElem(vd.tpt.tpe).getOrElse(vd.tpt.tpe.widen)
          Leaf(vd.symbol, vd.name, elem, selectiveIf(rhs))
        case vd @ ValDef(_, _, Some(rhs)) if hasMark(rhs) =>
          leafOf(rhs) match
            case Some(m) => Leaf(vd.symbol, vd.name, vd.tpt.tpe.widen, m)
            case None => refuse(vd.pos, s"`${vd.name}`'s right-hand side")
        case vd @ ValDef(_, _, Some(rhs)) if colourless(vd, rhs).isDefined =>
          Leaf(vd.symbol, vd.name, colourless(vd, rhs).get, rhs)
        case other => refuse(other.pos, "a statement that is not a marked val")
      }

    val _ = fromVals.foldLeft(Set.empty[Symbol]) { (seen, leaf) =>
      if mentions(leaf.prog, seen) then
        refuse(leaf.sym.pos.getOrElse(Position.ofMacroExpansion),
          s"`${leaf.name}`, whose right-hand side uses a name this block binds,")
      seen + leaf.sym
    }

    /**
     * MARKS IN THE RESULT ARE LEAVES TOO — `f(a.reflect, b.reflect)`
     * is the bracket's most natural spelling and refusing it would
     * have left the feature usable only in its long form. Each mark
     * is replaced by a reference to a fresh name, in evaluation
     * order, and the names join the run. They are independent by
     * construction: separate subexpressions of one expression cannot
     * mention each other's answers.
     */
    var hoisted: List[Leaf] = Nil
    // a COLOURED USE of a val this block binds is not a leaf: the
    // conversion wraps the NAME, and the name is bound by the curried
    // lambda below. Hoisting it lifted a reference to `n` out of the
    // scope that defines it, which is exactly what the compiler said.
    val ownNames: Set[Symbol] = fromVals.map(_.sym).toSet
    def ownUse(m: Term): Boolean = strip(m) match
      case id: Ident => ownNames.contains(id.symbol)
      case _ => false
    val body2 =
      if !hasMark(result) then result
      else
        val tm = new TreeMap:
          override def transformTerm(t: Term)(o: Symbol): Term = asMark(t) match
            case Some(m) if ownUse(m) => super.transformTerm(t)(o)
            case Some(m) if !hasMark(m) =>
              val sym = Symbol.newVal(Symbol.spliceOwner, s"ap$$${hoisted.length}",
                t.tpe.widen, Flags.EmptyFlags, Symbol.noSymbol)
              hoisted = hoisted :+ Leaf(sym, sym.name, t.tpe.widen, strip(m))
              Ref(sym)
            case Some(_) => refuse(t.pos, "a mark inside another mark")
            case None => super.transformTerm(t)(o)
        tm.transformTerm(result)(Symbol.spliceOwner)

    val leaves: List[Leaf] = fromVals ++ hoisted

    /** `a1 => a2 => … => result`, with each val's uses re-pointed at
     * its parameter */
    /** `X1 => X2 => … => R`, so the lambda below is BUILT at the type
     * `fmap` will be asked for. The first cut left the result at
     * `Any`, and the splice refused it: "Expected Int => Int => Any,
     * Actual Int => Any" — a function type is not inferred from a
     * nested lambda's body after the fact. */
    def curriedType(ls: List[Leaf], result: TypeRepr): TypeRepr =
      ls.foldRight(result) { (leaf, acc) =>
        defn.FunctionClass(1).typeRef.appliedTo(List(leaf.tpe, acc))
      }

    def curry(ls: List[Leaf], result: Term, owner: Symbol): Term =
      ls match
        case Nil => result
        case leaf :: tail =>
          Lambda(owner,
            MethodType(List(leaf.name))(_ => List(leaf.tpe),
              _ => curriedType(tail, result.tpe.widen)),
            (lam, params) =>
              val ref = params.head.asInstanceOf[Term]
              val m = new TreeMap:
                override def transformTerm(t: Term)(o: Symbol): Term = t match
                  case Apply(Select(conv, "apply"), List(id: Ident))
                    if colorSyms(calleeRoot(conv)) && id.symbol == leaf.sym => ref
                  case id: Ident if id.symbol == leaf.sym => ref
                  case _ => super.transformTerm(t)(o)
              val rest = curry(tail, m.transformTerm(result)(lam), lam)
              rest.changeOwner(lam))

    /** `fmap[X, R]` where `f : X => R` — R is the function's RESULT,
     * not the function. Passing the whole type was off by one and the
     * splice said so exactly: "Expected Int => Int => Int => Int,
     * Actual Int => Int => Int". */
    def fmapOf(fa: Term, elem: TypeRepr, f: Term, ap: Expr[Applicative[F]]): Term =
      val res = f.tpe.widen.dealias match
        case AppliedType(_, List(_, r)) => r
        case other => report.errorAndAbort(s"direct: expected a function, got ${other.show}")
      tpe2A[F, A](elem) { [X] => (tX: Type[X]) ?=>
        tpe2A[F, A](res) { [R] => (tR: Type[R]) ?=>
          '{ $ap.fmap[X, R](${ fa.asExprOf[F[X]] }, ${ f.asExprOf[X => R] }) }.asTerm
        }
      }

    def appOf(ff: Term, fa: Term, elem: TypeRepr, ap: Expr[Applicative[F]]): Term =
      // the carrier's ELEMENT is its LAST type argument: `Validated[E, A]`
      // has two, and reading the only one refused every two-parameter
      // carrier with "expected the carrier applied"
      val fn = ff.tpe.widen.dealias match
        case AppliedType(_, args) if args.nonEmpty => args.last.dealias
        case other => report.errorAndAbort(s"direct: expected the carrier applied, got ${other.show}")
      val res = fn match
        case AppliedType(_, List(_, r)) => r
        case other => report.errorAndAbort(s"direct: expected a function under the carrier, got ${other.show}")
      tpe2A[F, A](elem) { [X] => (tX: Type[X]) ?=>
        tpe2A[F, A](res) { [R] => (tR: Type[R]) ?=>
          '{ $ap.app[X, R](${ ff.asExprOf[F[X => R]] })(${ fa.asExprOf[F[X]] }) }.asTerm
        }
      }

    leaves match
      case Nil => '{ $AP.pure[A](${ body2.asExprOf[A] }) }
      case head :: rest =>
        // fmap the FIRST leaf with the curried rest of the block, then
        // app the others in order — the bracket, left to right
        val curried = curry(leaves, body2, Symbol.spliceOwner)
        val start = fmapOf(head.prog, head.tpe, curried, AP)
        rest.foldLeft(start) { (acc, leaf) =>
          appOf(acc, leaf.prog, leaf.tpe, AP)
        }.asExprOf[F[A]]

  /** run f with the TypeRepr as a Type given (the applicative road's
   * own copy — the monadic pipeline has one in its own scope) */
  private def tpe2A[F[_], A](using q: Quotes)(tpe: q.reflect.TypeRepr)[R](f: [T] => Type[T] ?=> R): R =
    tpe.asType match
      case '[t] => f[t]
