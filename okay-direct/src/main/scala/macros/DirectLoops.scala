package okay
package macros

import scala.quoted.*
import scala.annotation.tailrec

/**
 * LOOPS AND THE STATEMENT TAIL: for-do and for-yield over the
 * whitelisted combinators, and the fused statement compiler
 * (`compileTail`/`stmtsTail`, direct-tail-fusion) a loop body — and
 * the core's `while` — runs against its own recursive call as the
 * tail, so an iteration pays only the body's own binds.
 */
private[okay] trait DirectLoops[F[_]] extends DirectVals[F]:
  import q.reflect.*

  /** a whitelisted-combinator call: xs.<name>(x => body), with or
   * without the type application the collections put on it */
  object HofCall:
    def unapply(t: Term): Option[(Term, String, ValDef, Term)] = t match
      case Apply(TypeApply(Select(xs, nm), _), List(Lambda(List(p), b))) =>
        Some((xs, nm, p, b))
      case Apply(Select(xs, nm), List(Lambda(List(p), b))) =>
        Some((xs, nm, p, b))
      // `Direct`'s `foreach` EXTENSION on a `Pull` (v3): the receiver
      // is the extension's first argument, the lambda the second, and
      // the block's `DirectCtx` rides in a using clause after both
      case Apply(inner, List(_)) => inner match
        case Apply(TypeApply(recv, _), List(Lambda(List(p), b))) => sourceOf(recv).map(xs => (xs, "foreach", p, b))
        case Apply(recv, List(Lambda(List(p), b))) => sourceOf(recv).map(xs => (xs, "foreach", p, b))
        case _ => None
      case _ => None

    /** the receiver of `Direct.foreach(src)`, however the method is named at the call */
    private def sourceOf(recv: Term): Option[Term] = recv match
      case Apply(TypeApply(fn, _), List(xs)) if isSourceForeach(fn) => Some(xs)
      case Apply(fn, List(xs)) if isSourceForeach(fn) => Some(xs)
      case _ => None
    private def isSourceForeach(fn: Term): Boolean =
      fn.symbol.name == "foreach" && fn.symbol.owner == directSym

  /** xs.iterator, built by name so ArrayOps and IterableOnce both
   * serve; refuses receivers with no iterator */
  def iteratorOf(xs: Term): Term =
    // a generator as the receiver (specs/generators.md) has an
    // `iterator` member — the stepping reader — so it takes this road
    // as any collection does: the loop's LazyList over it is pulled as
    // the loop drives and memoised, so the body runs as far as it is
    // read and multi-shot re-entry is sound
    if xs.tpe.typeSymbol.methodMember("iterator").isEmpty
      && xs.tpe.baseClasses.forall(_.methodMember("iterator").isEmpty)
    then refuse(xs, "as a loop receiver with no .iterator")
    Select.unique(xs, "iterator")

  /** in a GENERATOR block, a for-yield in statement or final position:
   * `xs.map(x => body)` — with or without marks — or, for several
   * generators, `xs.flatMap(x => inner)` whose inner is itself one.
   * Answers the FOREACH body to emit with: `Writer(body)` for a map,
   * the inner comprehension as a statement for a flatMap (so this
   * hook fires again on it, one level down) */
  def yieldLoop(st: Term): Option[(Term, ValDef, Term)] =
    if !genRow then None
    else stripped(st) match
      case HofCall(xs, "map", param, lbody) => Some((xs, param, tellOf(lbody)))
      case HofCall(xs, "flatMap", param, lbody) if yieldLoop(lbody).isDefined =>
        Some((xs, param, Block(List(lbody), Literal(UnitConstant()))))
      case _ => None

  /** `Writer[W](body)` — the yield as the OPERATION it is (`Say`), a
   * bare statement of the block's row, which runs by do-notation; a
   * `Writer.tell` program would be a program of a narrower row, and
   * those do not run bare */
  def tellOf(lbody: Term): Term =
    val w = genW.getOrElse(report.errorAndAbort("a generator block with no Writer in its row (macro bug)"))
    def say(v: Term): Term =
      Apply(TypeApply(Ref(Symbol.requiredModule("okay.Writer").methodMember("apply").head), List(Inferred(w))), List(v))
    // a yielded value with marks in it is bound to a val FIRST, and the
    // op is built from the val: a mark hoisted out of the op's argument
    // would leave the op as a bound VALUE, and a bound value is not a
    // bare statement — it does not run
    if hasMark(lbody) then
      val sym = Symbol.newVal(Symbol.spliceOwner, "y$gen", lbody.tpe.widen, Flags.EmptyFlags, Symbol.noSymbol)
      Block(List(ValDef(sym, Some(lbody))), say(Ref(sym)))
    else say(lbody)

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

  /**
   * Compile t in STATEMENT position against an explicit tail
   * (direct-tail-fusion): the returned F[tailElem] term runs t's
   * effects, drops t's value, and continues with `tail` — spliced
   * into the LAST bind's own continuation for the fused statement
   * shapes (vals, assigns, pure statements, bare runnable ops), so
   * a loop body pays no separate sequencing bind. Everything else
   * (if/match, nested loops, try) falls back to one sequencing
   * bind — the pre-fusion emission, correct by construction, and a
   * tail duplicated into branches would duplicate code. `tail` is
   * invoked exactly once per emission path and must be a cheap
   * nullary call (loop()/loop(tl)).
   */
  def compileTail(t0: Term, tail: () => Term, tailElem: TypeRepr): Term =
    stripped(t0) match
      case l @ Lambda(_, _) => seqTail(l, tail, tailElem) // a Lambda IS a Block
      case Block(stats, expr) => stmtsTail(stats :+ expr, tail, tailElem)
      case one => stmtsTail(List(one), tail, tailElem)

  /** the unfused floor: one sequencing bind after the statement */
  def seqTail(t: Term, tail: () => Term, tailElem: TypeRepr): Term =
    statementF(t) match
      case Out.Eff(f, e) => bind(f, e, tailElem)(_ => tail())
      case Out.Pure(p) => Block(List(p), tail())

  /** fold statements threading the tail inward; every value is in
   * statement position (dropped) — the loop-body reading */
  def stmtsTail(stats: List[Statement], tail: () => Term, tailElem: TypeRepr): Term =
    stats match
      case Nil => tail()
      case (vd @ ValDef(_, _, Some(rhs))) :: rest =>
        val out0 = compile(rhs)
        val colourless = colourlessVal(vd, out0, rest, Literal(UnitConstant())) { (elem, out) =>
          if vd.symbol.flags.is(Flags.Lazy) then
            val (defs, use) = lazyOnce(vd, rhs, out, elem)
            val (rest2, _) = substUses(rest, Literal(UnitConstant()), vd.symbol, use)
            Out.Pure(Block(defs, stmtsTail(rest2, tail, tailElem)))
          else
            val sym = Symbol.newVal(Symbol.spliceOwner, vd.name, elem.widen,
              Flags.EmptyFlags, Symbol.noSymbol)
            val (rest2, _) = substUses(rest, Literal(UnitConstant()), vd.symbol, () => Ref(sym))
            Out.Pure(bind(asF(out), elem, tailElem) { v =>
              Block(List(ValDef(sym, Some(v))), stmtsTail(rest2, tail, tailElem))
            })
        }
        colourless match
          case Some(Out.Pure(t)) => t
          case Some(Out.Eff(t, _)) => t
          case None => out0 match
            case Out.Pure(p) =>
              Block(List(ValDef.copy(vd)(vd.name, vd.tpt, Some(p))),
                stmtsTail(rest, tail, tailElem))
            case out @ Out.Eff(_, _) if vd.symbol.flags.is(Flags.Lazy) =>
              val (defs, use) = lazyOnce(vd, rhs, out)
              val (rest2, _) = substUses(rest, Literal(UnitConstant()), vd.symbol, use)
              Block(defs, stmtsTail(rest2, tail, tailElem))
            case Out.Eff(c, e) =>
              bind(c, e, tailElem) { v =>
                Block(List(ValDef.copy(vd)(vd.name, vd.tpt, Some(v))),
                  stmtsTail(rest, tail, tailElem))
              }
      case (a @ Assign(lhs, rhs)) :: rest if hasMark(rhs) =>
        compile(rhs) match
          case Out.Pure(p) =>
            Block(List(Assign.copy(a)(lhs, p)), stmtsTail(rest, tail, tailElem))
          case Out.Eff(c, e) =>
            bind(c, e, tailElem) { v =>
              Block(List(Assign.copy(a)(lhs, v)), stmtsTail(rest, tail, tailElem))
            }
      case (dd: DefDef) :: rest if hasMark(dd) &&
        nestedProgramDef(dd, rest, Literal(UnitConstant()))((_, _, _) =>
          Out.Pure(Literal(UnitConstant()))).isDefined =>
        nestedProgramDef(dd, rest, Literal(UnitConstant())) { (defn, rest2, _) =>
          Out.Pure(Block(List(defn), stmtsTail(rest2, tail, tailElem)))
        }.get match
          case Out.Pure(t) => t
          case Out.Eff(t, _) => t
      // an `import` binds nothing and runs nothing: it rides along
      // (direct-import, 2026-09-17 — it used to be "an unsupported
      // statement", which made a scoped spelling like
      // `import Cont.direct.*` unusable inside a block)
      case (im: Import) :: rest =>
        Block(List(im), stmtsTail(rest, tail, tailElem))
      case (dd: Definition) :: rest =>
        if hasMark(dd) then refuse(dd, "inside a nested definition")
        Block(List(dd), stmtsTail(rest, tail, tailElem))
      // a generator block's `for x <- xs yield e` as a statement: emit
      // each e (specs/generators.md) — the loop with `Writer.tell(e)`
      // as its body, which is a bare runnable op and RUNS
      case (st: Term) :: rest if yieldLoop(st).isDefined =>
        val (xs, param, body) = yieldLoop(st).get
        hofLoop(st, xs, "foreach", param, body) match
          case Out.Eff(c, e) => bind(c, e, tailElem)(_ => stmtsTail(rest, tail, tailElem))
          case Out.Pure(p) => Block(List(p), stmtsTail(rest, tail, tailElem))
      case (st: Term) :: rest =>
        val t = stripped(st)
        if hasMark(t) || isPullForeach(t) then
          compile(t) match // marked if/match/nested-loop/mark, or a source loop: one bind, value dropped
            case Out.Eff(c, e) => bind(c, e, tailElem)(_ => stmtsTail(rest, tail, tailElem))
            case Out.Pure(p) => Block(List(p), stmtsTail(rest, tail, tailElem))
        else
          statementF(t) match // do-notation: a bare runnable op RUNS
            case Out.Eff(f, e) => bind(f, e, tailElem)(_ => stmtsTail(rest, tail, tailElem))
            case Out.Pure(p) =>
              if discardedMonadic(p.tpe) then
                report.errorAndAbort(
                  s"a value of ${p.tpe.widen.show} is discarded in statement position, " +
                    "and it is neither this block's monad nor an operation of its row — " +
                    "it cannot run here; bind it or move it to its own block", st.pos)
              Block(List(p), stmtsTail(rest, tail, tailElem))
      case other :: _ => refuse(other, "in an unsupported statement")

  // ---- direct-loops v2 (2026-09-22): guards, the other combinators,
  // the other collections. Every loop below has the shape foreachLoop
  // has — an immutable LazyList, a recursive def, the body compiled
  // per element — and adds one thing each.

  /** a guard peeled off the receiver: `xs.withFilter(x => cond)`, which
   * is what `for x <- xs if cond` desugars to, chained for several */
  object Filtered:
    def unapply(t: Term): Option[(Term, ValDef, Term)] = t match
      case Apply(TypeApply(Select(xs, "withFilter"), _), List(Lambda(List(p), c))) => Some((xs, p, c))
      case Apply(Select(xs, "withFilter"), List(Lambda(List(p), c))) => Some((xs, p, c))
      case _ => None

  /** the receiver with every guard peeled: (xs0, guards in source order) */
  def peelGuards(xs: Term): (Term, List[(ValDef, Term)]) = stripped(xs) match
    case Filtered(inner, p, c) =>
      val (xs0, gs) = peelGuards(inner)
      (xs0, gs :+ (p, c))
    case other => (other, Nil)

  /** `xs.foldLeft(z)((acc, x) => body)` */
  object FoldCall:
    def unapply(t: Term): Option[(Term, Term, ValDef, ValDef, Term)] = t match
      case Apply(Apply(TypeApply(Select(xs, "foldLeft"), _), List(z)), List(Lambda(List(a, x), body))) => Some((xs, z, a, x, body))
      case Apply(Apply(Select(xs, "foldLeft"), List(z)), List(Lambda(List(a, x), body))) => Some((xs, z, a, x, body))
      case _ => None

  /** the element through the guards, then `body`: a guard with marks
   * binds first, a false guard continues with `skip` — an `if` per
   * guard, in source order, and the body once */
  def guarded(h: Term, guards: List[(ValDef, Term)], elem: TypeRepr)(body: Term)(skip: () => Term): Term =
    guards match
      case Nil => body
      case (p, c) :: rest =>
        // `body` is a VALUE, not by-name: a quote built inside a by-name
        // closure left the pickler "unresolved symbols: parameter tU"
        val inner = guarded(h, rest, elem)(body)(skip)
        compile(subst(c, p.symbol, h)) match
          case Out.Pure(pc) => If(pc, inner, skip())
          case Out.Eff(cf, _) => bind(cf, TypeRepr.of[Boolean], elem)(b => If(b, inner, skip()))

  /** the traverse's List[U] as the node's own collection type W:
   * List/Seq/Iterable as is, Vector/IndexedSeq, Set, Map (of pairs) —
   * anything else refused with the workaround */
  def collected[U: Type, W: Type](acc: Expr[List[U]], at: Term): Expr[W] =
    val w = TypeRepr.of[W]
    def as[C: Type](e: Expr[C]): Expr[W] = '{ ${ upcast[C, W] }($e) }
    if TypeRepr.of[List[U]] <:< w then as[List[U]](acc)
    else if TypeRepr.of[Vector[U]] <:< w then as[Vector[U]]('{ $acc.toVector })
    else if TypeRepr.of[Set[U]] <:< w then as[Set[U]]('{ $acc.toSet })
    else w.baseType(Symbol.requiredClass("scala.collection.immutable.Map")) match
      case AppliedType(_, List(k, v)) =>
        tpe2(k) { [K] => (tK: Type[K]) ?=>
          tpe2(v) { [V] => (tV: Type[V]) ?=>
            val ev = Expr.summon[U <:< (K, V)].getOrElse(
              refuse(at, s"in a for-yield into a Map whose yield type ${Type.show[U]} is not a pair"))
            as[Map[K, V]]('{ $acc.toMap(using $ev) })
          }
        }
      case _ =>
        refuse(at, s"in a for-yield whose collection type ${w.show} is not List/Seq/Vector/Set/Map " +
          "(.toList the receiver or collect explicitly)")

  // THE RULE THESE LOOPS OBEY (found the hard way, three pickler
  // crashes): a quote nested inside a splice may name the OUTER
  // quote's symbols — `loop`, `tl`, `acc`, `h` — but may not carry a
  // TYPE TREE of the pattern-bound element type (`(b: u) => …`,
  // `x :: acc`, `Some(h)`): the pickler reports "unresolved symbols:
  // given instance u$given". So each per-element step is built by
  // reflection — `bind` (DirectEmit's own quote, its own level),
  // `consTo`, `Apply(loopFn, …)` — and the only nested quote is a
  // harmless `loop(tl, acc)` the parts are taken from.

  /** the function reference and argument refs of a `loop(…)` call */
  @tailrec private def parts(call: Term): (Term, List[Term]) = call match
    case Apply(fn, args) => (fn, args)
    case Inlined(_, _, inner) => parts(inner)
    case other => report.errorAndAbort(s"direct loops (macro bug): not a call: ${other.show}")

  /** `x :: acc` */
  private def consTo(acc: Term, elem: TypeRepr)(x: Term): Term =
    Apply(TypeApply(Select.unique(acc, "::"), List(Inferred(elem))), List(x))

  /** every element of xs prepended to acc — the flatMap step */
  def prependAll[U: Type](xs: Expr[IterableOnce[U]], acc: Expr[List[U]]): Expr[List[U]] =
    '{ $xs.iterator.foldLeft($acc)((a, x) => x :: a) }

  /** the compiled body at elem, with the loop variable in place of the lambda's parameter */
  private def bodyAt(lbody: Term, param: ValDef, h: Term, elem: TypeRepr): Term =
    asFAt(compile(subst(lbody, param.symbol, h)), elem).changeOwner(Symbol.spliceOwner)

  /** the receiver is a `Pull[A, G]` — a source read by a program
   * (specs/direct-loops.md v3), which takes `pullLoop` below */
  def isPull(xs: Term): Boolean =
    xs.tpe.widen.dealias.derivesFrom(Symbol.requiredClass("okay.Pull"))

  /** `src.foreach(x => body)` over a `Pull`, marks or not — the road
   * fires on the receiver's type, because an unmarked source loop is
   * a `Unit ! G` in statement position that would not run bare */
  def isPullForeach(t: Term): Boolean = stripped(t) match
    case HofCall(xs, "foreach", _, _) => isPull(peelGuards(xs)._1)
    case _ => false

  /**
   * for x <- src do body over a `Pull[t, g]`: the loop is a PROGRAM —
   * one `step` bound per element through the same row lift a mark
   * takes (so a `g` outside the block's row is refused as a mark
   * would be), the body compiled against the recursive call as its
   * tail exactly as `foreachLoop` does, guards honoured.
   */
  def pullLoop(xs: Term, guards: List[(ValDef, Term)], param: ValDef, lbody: Term): Term =
    // the receiver's type as a whole, not `Pull[t, g]` taken apart: a
    // pure source has `G = Pure = Nothing`, which no higher-kinded
    // type pattern matches, and the loop needs the type only to name
    // its parameter — `step` is selected on it by name
    (param.tpt.tpe.widen.asType, xs.tpe.widen.dealias.asType) match
      case ('[t], '[src]) => '{
        def loop(p: src): F[Unit] = ${
          val stepped = TypeRepr.of[Option[(t, src)]]
          val stepTerm = Select.unique('p.asTerm, "step")
          bind(markTerm(stepTerm, stepped, xs.pos), stepped, TypeRepr.of[Unit]) { o =>
            '{
              ${ o.asExprOf[Option[(t, src)]] } match
                case Some((h, tl)) =>
                  ${
                    guarded('h.asTerm, guards, TypeRepr.of[Unit])(
                      compileTail(subst(lbody, param.symbol, 'h.asTerm), () => '{ loop(tl) }.asTerm, TypeRepr.of[Unit]))(
                      () => '{ loop(tl) }.asTerm)
                      .changeOwner(Symbol.spliceOwner).asExprOf[F[Unit]]
                  }
                case None => $M.pure(())
            }.asTerm
          }.changeOwner(Symbol.spliceOwner).asExprOf[F[Unit]]
        }
        loop(${ xs.asExprOf[src] })
      }.asTerm
      case _ => refuse(xs, "over a source whose type the macro could not name (macro bug)")

  /** for x <- xs do body — with guards */
  def foreachLoop(xs: Term, guards: List[(ValDef, Term)], param: ValDef, lbody: Term): Term =
    param.tpt.tpe.widen.asType match
      case '[t] => '{
        val items: LazyList[t] = ${ iteratorOf(xs).asExprOf[Iterator[t]] }.to(LazyList)
        def loop(rest: LazyList[t]): F[Unit] = rest match
          case h #:: tl =>
            ${
              guarded('h.asTerm, guards, TypeRepr.of[Unit])(
                compileTail(subst(lbody, param.symbol, 'h.asTerm), () => '{ loop(tl) }.asTerm, TypeRepr.of[Unit]))(
                () => '{ loop(tl) }.asTerm)
                .changeOwner(Symbol.spliceOwner).asExprOf[F[Unit]]
            }
          case _ => $M.pure(())
        loop(items)
      }.asTerm

  /** for x <- xs yield body — the traverse shape, with guards, into
   * the node's own collection type */
  def mapLoop(t: Term, xs: Term, guards: List[(ValDef, Term)], param: ValDef, lbody: Term): Term =
    val uRepr = lbody.tpe.widen
    val wRepr = t.tpe.widen
    ((param.tpt.tpe.widen.asType, uRepr.asType, wRepr.asType): @unchecked) match
      case ('[tt], '[u], '[w]) => '{
        val items: LazyList[tt] = ${ iteratorOf(xs).asExprOf[Iterator[tt]] }.to(LazyList)
        def loop(rest: LazyList[tt], acc: List[u]): F[w] = rest match
          case h #:: tl =>
            ${
              val (loopFn, List(tlRef, accRef)) = parts('{ loop(tl, acc) }.asTerm): @unchecked
              val step = bind(bodyAt(lbody, param, 'h.asTerm, uRepr), uRepr, wRepr)(b =>
                Apply(loopFn, List(tlRef, consTo(accRef, uRepr)(b))))
              guarded('h.asTerm, guards, wRepr)(step)(() => Apply(loopFn, List(tlRef, accRef)))
                .changeOwner(Symbol.spliceOwner).asExprOf[F[w]]
            }
          case _ => $M.pure[w](${ collected[u, w]('{ acc.reverse }, t) })
        loop(items, Nil)
      }.asTerm

  /** for x <- xs; y <- ys yield … — `xs.flatMap(x => inner)`: the
   * body's value is a collection of U, appended */
  def flatMapLoop(t: Term, xs: Term, guards: List[(ValDef, Term)], param: ValDef, lbody: Term): Term =
    val wRepr = t.tpe.widen
    val uRepr = wRepr.baseType(Symbol.requiredClass("scala.collection.IterableOnce")) match
      case AppliedType(_, List(u)) => u
      case _ => refuse(t, s"in a for-comprehension whose result ${wRepr.show} is not a collection")
    val bRepr = lbody.tpe.widen
    ((param.tpt.tpe.widen.asType, uRepr.asType, bRepr.asType, wRepr.asType): @unchecked) match
      case ('[tt], '[u], '[b], '[w]) =>
        val ev = Expr.summon[b <:< IterableOnce[u]].getOrElse(
          refuse(t, s"in a for-comprehension whose inner result ${bRepr.show} is not a collection of ${uRepr.show}"))
        '{
          val items: LazyList[tt] = ${ iteratorOf(xs).asExprOf[Iterator[tt]] }.to(LazyList)
          def loop(rest: LazyList[tt], acc: List[u]): F[w] = rest match
            case h #:: tl =>
              ${
                val (loopFn, List(tlRef, accRef)) = parts('{ loop(tl, acc) }.asTerm): @unchecked
                val step = bind(bodyAt(lbody, param, 'h.asTerm, bRepr), bRepr, wRepr) { bb =>
                  val asOnce = Apply(Select.unique(ev.asTerm, "apply"), List(bb))
                  Apply(loopFn, List(tlRef,
                    prependAll[u](asOnce.asExprOf[IterableOnce[u]], accRef.asExprOf[List[u]]).asTerm))
                }
                guarded('h.asTerm, guards, wRepr)(step)(() => Apply(loopFn, List(tlRef, accRef)))
                  .changeOwner(Symbol.spliceOwner).asExprOf[F[w]]
              }
            case _ => $M.pure[w](${ collected[u, w]('{ acc.reverse }, t) })
          loop(items, Nil)
        }.asTerm

  /** xs.filter(x => body) — kept where the body answers true */
  def filterLoop(t: Term, xs: Term, guards: List[(ValDef, Term)], param: ValDef, lbody: Term): Term =
    val tRepr = param.tpt.tpe.widen
    val wRepr = t.tpe.widen
    ((tRepr.asType, wRepr.asType): @unchecked) match
      case ('[tt], '[w]) => '{
        val items: LazyList[tt] = ${ iteratorOf(xs).asExprOf[Iterator[tt]] }.to(LazyList)
        def loop(rest: LazyList[tt], acc: List[tt]): F[w] = rest match
          case h #:: tl =>
            ${
              val (loopFn, List(tlRef, accRef)) = parts('{ loop(tl, acc) }.asTerm): @unchecked
              val step = bind(bodyAt(lbody, param, 'h.asTerm, TypeRepr.of[Boolean]), TypeRepr.of[Boolean], wRepr)(b =>
                Apply(loopFn, List(tlRef, If(b, consTo(accRef, tRepr)('h.asTerm), accRef))))
              guarded('h.asTerm, guards, wRepr)(step)(() => Apply(loopFn, List(tlRef, accRef)))
                .changeOwner(Symbol.spliceOwner).asExprOf[F[w]]
            }
          case _ => $M.pure[w](${ collected[tt, w]('{ acc.reverse }, t) })
        loop(items, Nil)
      }.asTerm

  /** xs.exists / forall / find (x => body) — stops at the first
   * element that decides */
  def scanLoop(nm: String, t: Term, xs: Term, guards: List[(ValDef, Term)], param: ValDef, lbody: Term): Term =
    val tRepr = param.tpt.tpe.widen
    val rRepr = t.tpe.widen
    val bool = TypeRepr.of[Boolean]
    tRepr.asType match
      case '[tt] =>
        /** decide(b, h, next): what the loop answers once the body answered b */
        def stepOf(h: Term, next: Term, decide: (Term, Term) => Term): Term =
          bind(bodyAt(lbody, param, h, bool), bool, rRepr)(b => decide(b, next))
        def pureAt(v: Term): Term = pureF(Typed(v, Inferred(rRepr)))
        val someApply = Symbol.requiredModule("scala.Some").methodMember("apply").head
        rRepr.asType match
          case '[r] => '{
            val items: LazyList[tt] = ${ iteratorOf(xs).asExprOf[Iterator[tt]] }.to(LazyList)
            def loop(rest: LazyList[tt]): F[r] = rest match
              case h #:: tl => ${
                val (loopFn, List(tlRef)) = parts('{ loop(tl) }.asTerm): @unchecked
                val next = Apply(loopFn, List(tlRef))
                val h0 = 'h.asTerm
                val step = nm match
                  case "exists" => stepOf(h0, next, (b, nx) => If(b, pureAt(Literal(BooleanConstant(true))), nx))
                  case "forall" => stepOf(h0, next, (b, nx) => If(b, nx, pureAt(Literal(BooleanConstant(false)))))
                  case _ => stepOf(h0, next, (b, nx) =>
                    If(b, pureAt(Apply(TypeApply(Ref(someApply), List(Inferred(tRepr))), List(h0))), nx))
                guarded(h0, guards, rRepr)(step)(() => Apply(loopFn, List(tlRef)))
                  .changeOwner(Symbol.spliceOwner).asExprOf[F[r]]
              }
              case _ => ${
                val end = nm match
                  case "exists" => Literal(BooleanConstant(false))
                  case "forall" => Literal(BooleanConstant(true))
                  case _ => Ref(Symbol.requiredModule("scala.None"))
                pureAt(end).asExprOf[F[r]]
              }
            loop(items)
          }.asTerm

  /** xs.foldLeft(z)((acc, x) => body) — the accumulator threads
   * through the loop; a marked `z` binds first */
  def foldLoop(t: Term, xs0: Term, z: Term, accP: ValDef, elemP: ValDef, body: Term): Out =
    val (xs, guards) = peelGuards(xs0)
    val bRepr = t.tpe.widen
    def emit(xsPure: Term, zPure: Term): Term =
      ((elemP.tpt.tpe.widen.asType, bRepr.asType): @unchecked) match
        case ('[tt], '[b]) => '{
          val items: LazyList[tt] = ${ iteratorOf(xsPure).asExprOf[Iterator[tt]] }.to(LazyList)
          def loop(rest: LazyList[tt], acc: b): F[b] = rest match
            case h #:: tl => ${
              val (loopFn, List(tlRef, accRef)) = parts('{ loop(tl, acc) }.asTerm): @unchecked
              val stepBody = asFAt(compile(subst(subst(body, accP.symbol, accRef), elemP.symbol, 'h.asTerm)), bRepr)
                .changeOwner(Symbol.spliceOwner)
              val step = bind(stepBody, bRepr, bRepr)(bb => Apply(loopFn, List(tlRef, bb)))
              guarded('h.asTerm, guards, bRepr)(step)(() => Apply(loopFn, List(tlRef, accRef)))
                .changeOwner(Symbol.spliceOwner).asExprOf[F[b]]
            }
            case _ => $M.pure[b](acc)
          loop(items, ${ zPure.asExprOf[b] })
        }.asTerm
    def withZ(xsPure: Term): Term =
      if hasMark(z) then
        compile(z) match
          case Out.Eff(c, e) => bind(c, e, bRepr)(v => emit(xsPure, v))
          case Out.Pure(p) => emit(xsPure, p)
      else emit(xsPure, z)
    if hasMark(xs) then
      compile(xs) match
        case Out.Eff(c, e) => Out.Eff(bind(c, e, bRepr)(v => withZ(v)), bRepr)
        case Out.Pure(p) => Out.Eff(withZ(p), bRepr)
    else Out.Eff(withZ(xs), bRepr)

  /** the loop shapes: guards peeled off the receiver, the receiver
   * hoisted first if it is marked */
  def hofLoop(t: Term, xs0: Term, nm: String, param: ValDef, lbody: Term): Out =
    val (xs, guards) = peelGuards(xs0)
    val loopElem = if nm == "foreach" then TypeRepr.of[Unit] else t.tpe.widen
    def emit(xsPure: Term): Term = nm match
      case "foreach" if isPull(xsPure) => pullLoop(xsPure, guards, param, lbody)
      case "foreach" => foreachLoop(xsPure, guards, param, lbody)
      case "map" => mapLoop(t, xsPure, guards, param, lbody)
      case "flatMap" => flatMapLoop(t, xsPure, guards, param, lbody)
      case "filter" => filterLoop(t, xsPure, guards, param, lbody)
      case _ => scanLoop(nm, t, xsPure, guards, param, lbody)
    if hasMark(xs) then
      compile(xs) match
        case Out.Eff(c, e) => Out.Eff(bind(c, e, loopElem)(v => emit(v)), loopElem)
        case Out.Pure(p) => Out.Eff(emit(p), loopElem)
    else Out.Eff(emit(xs), loopElem)

  /** the combinator names a marked lambda argument is rewritten under */
  val loopNames: Set[String] = Set("foreach", "map", "flatMap", "filter", "exists", "forall", "find")

  /** marks anywhere a loop reads: the body, a guard, the receiver */
  def loopHasMark(xs: Term, lbody: Term): Boolean =
    hasMark(lbody) || peelGuards(xs)._2.exists((_, c) => hasMark(c))
