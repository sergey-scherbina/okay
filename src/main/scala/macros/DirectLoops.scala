package okay
package macros

import scala.quoted.*

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
      case (st: Term) :: rest =>
        val t = stripped(st)
        if hasMark(t) then
          compile(t) match // marked if/match/nested-loop/mark: one bind, value dropped
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

  /** for x <- xs do body — run per element, in order; the loop
   * recurses over an immutable, LAZY LazyList so multi-shot re-entry
   * is sound and an unbounded receiver is only forced as far as the
   * monad drives it; the body compiles against `loop(tl)` as its
   * tail, so each element pays only the body's own binds */
  def foreachLoop(xs: Term, param: ValDef, lbody: Term): Term =
    tpe2(param.tpt.tpe.widen) { [T] => (tT: Type[T]) ?=>
      '{
        val items: LazyList[T] = ${ iteratorOf(xs).asExprOf[Iterator[T]] }.to(LazyList)
        def loop(rest: LazyList[T]): F[Unit] = rest match
          case h #:: tl =>
            ${
              compileTail(subst(lbody, param.symbol, 'h.asTerm),
                  () => '{ loop(tl) }.asTerm, TypeRepr.of[Unit])
                .changeOwner(Symbol.spliceOwner).asExprOf[F[Unit]]
            }
          case _ => $M.pure(())
        loop(items)
      }.asTerm
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
