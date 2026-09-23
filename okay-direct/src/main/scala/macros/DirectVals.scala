package okay
package macros

import scala.quoted.*

/**
 * THE THREE WORDS FOR A LOCAL: `val` runs here, `lazy val` is the
 * `Once` cell, `def` is by name — the rules deciding what a val, a
 * lazy val or a nested def at the block's program type MEANS, and
 * the code that gives it that meaning. The nested def compiles its
 * body through the knot; the rest emits through DirectEmit.
 */
private[okay] trait DirectVals[F[_]] extends DirectMarks[F] with DirectRow[F] with DirectEmit[F]:
  import q.reflect.*

  /**
   * `lazy val x: T = rhs` with a mark in rhs (direct-once,
   * specs/direct-macro.md): the by-need word. The rhs compiles to a
   * program, built at the first demand and never again — a cell
   * under a fresh `Once.Handle` — and every use of `x` becomes a mark
   * on that program, so its effects run in the POSITION of the first
   * demand, once. Three words, three semantics, all visible: `val`
   * runs here, `lazy val` runs at first use, a bare mark runs at
   * every use.
   *
   * The cell is the `Once` effect's, so the row has to name it: the
   * macro cannot decide what "once" means under a multi-shot handler
   * (that is handler order, `Once.run` inside or outside the search)
   * and does not try. What it emits, for the block's row R:
   *
   *     val x$handle = new Once.Handle[T]
   *     val x$once: F[T] = Once.at[T, R](x$handle)(h => Inject(Force(h)))((h, a) => Inject(Store(h, a)))(rhs')
   *     ... x$once.reflect ...             // at each use
   *
   * Returns the two definitions and a fresh mark per use.
   *
   * The rule keys on the COMPILED rhs, not on `hasMark`: a rhs that
   * runs an operation by do-notation (`lazy val x = { Writer("x"); 3 }`)
   * carries no mark syntactically and is by-need all the same
   * (direct-once-bare, 2026-09-16 — it bound eagerly for an hour). A
   * pure rhs stays a plain Scala lazy val.
   */
  def lazyOnce(vd: ValDef, rhs: Term, compiled: Out, elem0: TypeRepr = TypeRepr.of[Nothing]): (List[Statement], () => Term) =
    val row = rowOf.getOrElse(
      refuse(vd, "in a lazy val (the block's monad is not a program, so no row can hold the Once cell)"))
    if !onceInRow then report.errorAndAbort(
      "a lazy val with a Direct mark is call-by-need, which is the Once effect here: add it to the " +
        s"block's row — `A ! (Once + ${row.show})` — and run the program with Once.run; " +
        "or write `val` (run now) or a bare mark at each use (run every time)", vd.pos)
    // a lazy val naming itself is a knot at run time and a dangling symbol after the rewrite
    var selfRef = false
    val probe = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
        case id: Ident if id.symbol == vd.symbol => selfRef = true
        case _ => super.traverseTree(tree)(owner)
    probe.traverseTree(rhs)(Symbol.spliceOwner)
    if selfRef then refuse(vd, "in a lazy val that refers to itself")
    val elem = if elem0 =:= TypeRepr.of[Nothing] then vd.tpt.tpe.widen else elem0.widen
    val prog: Term = asFAt(compiled, elem)
    val handleT = TypeRepr.of[Once.Handle].appliedTo(elem)
    val hSym = Symbol.newVal(Symbol.spliceOwner, s"${vd.name}$$handle", handleT,
      Flags.EmptyFlags, Symbol.noSymbol)
    val hVal = ValDef(hSym, Some(tpe2(elem) { [T] => (tT: Type[T]) ?=> '{ new Once.Handle[T]() }.asTerm }))
    val forceApply = Symbol.requiredModule("okay.Once.Force").methodMember("apply").head
    val storeApply = Symbol.requiredModule("okay.Once.Store").methodMember("apply").head
    val atSym = Symbol.requiredModule("okay.Once").methodMember("at").head
    def forceOp(h: Term): Term =
      injectTerm(Apply(TypeApply(Ref(forceApply), List(Inferred(elem))), List(h)),
        TypeRepr.of[Option].appliedTo(elem), row)
    def storeOp(h: Term, a: Term): Term =
      injectTerm(Apply(TypeApply(Ref(storeApply), List(Inferred(elem))), List(h, a)), elem, row)
    val (forceFn, storeFn) = tpe2(elem) { [T] => (tT: Type[T]) ?=>
      ('{ (h: Once.Handle[T]) => ${ forceOp('h.asTerm).asExprOf[F[Option[T]]] } }.asTerm,
       '{ (h: Once.Handle[T], a: T) => ${ storeOp('h.asTerm, 'a.asTerm).asExprOf[F[T]] } }.asTerm)
    }
    // the thunk, built under the owner it is placed under (see Once.at)
    val progThunk = Lambda(Symbol.spliceOwner,
      MethodType(Nil)(_ => Nil, _ => TypeRepr.of[F].appliedTo(elem.widen)),
      (owner, _) => prog.changeOwner(owner))
    val onceT = Apply(Apply(Apply(Apply(
      TypeApply(Ref(atSym), List(Inferred(elem), Inferred(row))),
      List(Ref(hSym))), List(forceFn)), List(storeFn)), List(progThunk))
    val oSym = Symbol.newVal(Symbol.spliceOwner, s"${vd.name}$$once",
      TypeRepr.of[F].appliedTo(elem), Flags.EmptyFlags, Symbol.noSymbol)
    val oVal = ValDef(oSym, Some(onceT))
    (List(hVal, oVal),
      () => Apply(TypeApply(Ref(reflectMark), List(Inferred(TypeRepr.of[F]), Inferred(elem))), List(Ref(oSym))))

  /**
   * A COLOURLESS val of the block's program type (direct-colourless-val,
   * 2026-09-16): `val x = fetch(k)`, no mark and no ascription.
   *
   * Inference gives such a val the PROGRAM type, so the colouring
   * conversion does not fire at the declaration — it fires at every USE,
   * where an `Int` is finally demanded. The val then means what `def`
   * means, and so does `lazy val`: measured as `val, val, lazy val,
   * lazy val, def, def` where the ascribed spelling gives `val, lazy
   * val, def, def`. Three words, one meaning, silently — the opposite of
   * what the block promises.
   *
   * So the DECLARATION decides, as the words do everywhere else in
   * Scala: a val read as a value is a BINDING (by value, run here — the
   * do-notation reading of a bare statement, extended to a val), and a
   * lazy val is the `Once` cell (by need). A val held as a PROGRAM —
   * marked at its uses, passed to `!.once`, stored — is a value and
   * stays untouched; nothing colours it. A val read BOTH ways in one
   * block is refused with both readings named, since binding it would
   * leave its program uses holding an answer.
   *
   * None when the rule does not apply: the ordinary pure-val path takes
   * over.
   */
  def colourlessVal(vd: ValDef, out0: Out, rest: List[Statement], expr: Term)
                   (emit: (TypeRepr, Out) => Out): Option[Out] =
    // the VAL'S OWN TYPE is what decides, not the rhs's: a rhs that uses
    // an earlier colourless val compiles to an Out.Eff, and its element
    // type is then the PROGRAM, not the answer (found on a handler with
    // three dependent lookups, direct-colourless-val)
    runnableElemT(vd.tpt.tpe).flatMap { elem =>
      val (coloured, bare) = useKinds(rest, expr, vd.symbol)
      if bare > 0 && coloured > 0 then
        report.errorAndAbort(
          s"`${vd.name}` holds a program of this block, and the block reads it BOTH ways: " +
            s"as a value ($coloured use(s) — its effects would run once, here) and as a " +
            s"program ($bare use(s) — its effects run at each mark). Pick one: ascribe the " +
            s"answer type (`val ${vd.name}: ${elem.widen.show} = ...`) to run it here, or " +
            "keep it a program and mark every use.", vd.pos)
      else if bare > 0 then None
      else
        // the rhs as a PROGRAM of this block: a pure one is the program
        // itself (marked), an effectful one is bound first and its answer
        // marked — the shape `compile` gives a mark whose value carries marks
        val prog: Out = out0 match
          case Out.Pure(p) => Out.Eff(markTerm(p, elem, vd.pos), elem)
          case Out.Eff(c, e) =>
            Out.Eff(bind(c, e, elem)(v => markTerm(v, elem, vd.pos)), elem)
        Some(emit(elem, prog))
    }

  /**
   * A nested PARAMETERLESS `def` at the block's program type whose body
   * carries marks (direct-nested-def, 2026-09-16): `def plan =
   * effect(GetPlan(user.planId))` beside a `lazy val user`.
   *
   * Such a body is its own program — it ends at the block's program
   * type, so binding the marks inside it changes nothing about what the
   * def means — and it compiles through the same `pipeline` a `try`
   * body does. The def then behaves as `def` always has: by name, a
   * bind (and a run) per use.
   *
   * A fresh symbol, because inference gives `def plan = effect(...)`
   * the PRECISE constructor type `Free.Inject[R, A]` and the compiled
   * body is a `Free[R, A]`; the uses are rewritten with it, so the
   * spelling `z` the reader wrote is what the reader keeps. A def with
   * PARAMETERS, or one whose type is not this block's program, keeps
   * the refusal: v1 does not rewrite a signature.
   */
  def nestedProgramDef(dd: DefDef, rest: List[Statement], expr: Term)
                      (emit: (Statement, List[Statement], Term) => Out): Option[Out] =
    if dd.paramss.nonEmpty then None
    else dd.rhs.flatMap { body =>
      if !hasMark(body) then None
      else runnableElemT(dd.returnTpt.tpe).map { elem =>
        val fT = TypeRepr.of[F].appliedTo(elem.widen)
        val sym = Symbol.newMethod(Symbol.spliceOwner, dd.name + "$prog",
          MethodType(Nil)(_ => Nil, _ => fT))
        // the body is the PROGRAM, not an expression yielding its answer:
        // compile it in this same pass (it reads the block's own locals)
        // and flatten — a body that ends in a program is bound and its
        // answer marked, the shape `colourlessVal` gives a val
        val compiled: Term = compile(body.changeOwner(Symbol.spliceOwner)) match
          case Out.Pure(q) => markTerm(q, elem, dd.pos)
          case Out.Eff(c, e) =>
            // `<:<`, not `=:=`: with `Free[F, +A]` a program at a narrower
            // answer IS one at the declared answer (free-answer-variance)
            if e.widen <:< elem.widen then c
            else bind(c, e, elem)(v => markTerm(v, elem, dd.pos))
        val defn = DefDef(sym, _ => Some(compiled.changeOwner(sym)))
        def prog(): Term = Apply(Ref(sym), Nil)
        def marked(): Term =
          Apply(TypeApply(Ref(reflectMark), List(Inferred(TypeRepr.of[F]), Inferred(elem.widen))),
            List(prog()))
        val (rest2, expr2) = substUsesBy(rest, expr, dd.symbol, marked, prog)
        emit(defn, rest2, expr2)
      }
    }
