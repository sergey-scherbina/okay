package okay
package macros

import scala.quoted.*

/**
 * THE BLOCK'S MONAD AND ROW: what F is — a program monad `A ! Row`
 * or any other — and what follows from it: which values can RUN as
 * bare statements (do-notation), which a statement would silently
 * drop, how an operation or a narrower program is lifted into the
 * row, whether the row holds a `Once` cell. Reads types; the only
 * terms it builds are the lifts themselves (`Free.Inject`,
 * `RowLift.into`).
 */
private[okay] trait DirectRow[F[_]] extends DirectPhase[F]:
  import q.reflect.*

  lazy val freeClass = Symbol.requiredClass("okay.Free")

  /** the block's effect row, if its F is the program monad A ! Row */
  lazy val rowOf: Option[TypeRepr] =
    TypeRepr.of[F].appliedTo(TypeRepr.of[scala.Unit]).dealias match
      case AppliedType(f, List(row, _)) if f.typeSymbol == freeClass => Some(row)
      case _ => None

  lazy val stagedType: Symbol = Symbol.requiredModule("okay.Staged").typeMember("Staged")

  /** the block's row when its F is `Staged[Row, R, *]` (specs/direct-
   * staged.md) — opaque, so it does not dealias to the function it is.
   * Kept apart from `rowOf`: the defer pre-pass and the Once cells are
   * Free's, and a staged block has neither */
  lazy val stagedRow: Option[TypeRepr] =
    TypeRepr.of[F].appliedTo(TypeRepr.of[scala.Unit]).dealias match
      case AppliedType(f, List(row, _, _)) if f.typeSymbol == stagedType => Some(row)
      case _ => None

  /** the row an operation must belong to, whichever carrier holds it */
  lazy val anyRow: Option[TypeRepr] = rowOf.orElse(stagedRow)

  /** the block's row names Once — the by-need cells have a handler */
  lazy val onceInRow: Boolean =
    rowOf.exists(r => TypeRepr.of[Once[Unit]] <:< r.appliedTo(TypeRepr.of[Unit]))

  lazy val injectApply: Symbol =
    Symbol.requiredModule("okay.Free.Inject").methodMember("apply").head

  /** Free.Inject[Row, elem](op) — the op lifted into the row program */
  def injectTerm(op: Term, elem: TypeRepr, row: TypeRepr): Term =
    Apply(TypeApply(Ref(injectApply), List(Inferred(row), Inferred(elem.widen))), List(op))

  /** the op lifted into THIS block's carrier: `Free.Inject(op)` for a
   * program row, `stage.stage[elem](op)` for a staged one — the
   * inline match inside `stage` sees the operation term and picks
   * the arm at compile time */
  def liftOp(op: Term, elem: TypeRepr, row: TypeRepr): Term = stage match
    // `stripped`: an ascribed operation (`State.Get(): State[Int, Int]`)
    // reaches the inline match as the constructor, not the ascription
    case Some(st) => Apply(TypeApply(Select.unique(st, "stage"), List(Inferred(elem.widen))), List(stripped(op)))
    case None => injectTerm(op, elem, row)

  /**
   * `Free.Inject(op)` under its inlining wrappers — what `State.get`,
   * `Writer.tell`, `Reader.ask` ARE after inlining (`effect(Get())`).
   * The wrappers' bindings (an argument the inliner proxied to a val)
   * come back with the op, so the caller can keep them in front of it.
   */
  def injectedOp(t: Term): Option[(List[Statement], Term)] =
    def walk(t: Term): Option[(List[Statement], Term)] = t match
      case Inlined(_, bs, inner) => walk(inner).map((bs2, op) => (bs ++ bs2, op))
      case Typed(inner, _) => walk(inner)
      case Block(bs, inner) => walk(inner).map((bs2, op) => (bs ++ bs2, op))
      case Apply(TypeApply(f, _), List(op)) if f.symbol == injectApply => Some((Nil, op))
      case Apply(f, List(op)) if f.symbol == injectApply => Some((Nil, op))
      case _ => None
    /** the inliner proxies a by-value argument to a val (`val a$proxy
     * = Get(); Inject(a$proxy)`), and an inline match cannot reduce on
     * a name: put the constructor back where the op stands. The proxy
     * has exactly one use — the op — so its binding goes with it. */
    def resolve(bs: List[Statement], op: Term): (List[Statement], Term) = stripped(op) match
      // the name arrives as `Inlined(None, Nil, Ident(a$proxy))` — stripped first
      case id: Ident =>
        bs.collectFirst { case v: ValDef if v.symbol == id.symbol && v.rhs.isDefined => v } match
          case Some(v) => resolve(bs.filterNot(_ eq v), v.rhs.get)
          case None => (bs, op)
      case _ => (bs, op)
    walk(t).map(resolve)

  /**
   * ONE mark, dispatched by type: an F[elem] of this block IS
   * already the term to bind; an operation of this block's row is
   * injected into the row program first. Anything else is refused
   * with both possibilities named.
   */
  def markTerm(m: Term, elem: TypeRepr, at: Position): Term =
    val fT = TypeRepr.of[F].appliedTo(elem.widen)
    if m.tpe <:< fT then m
    else anyRow match
      case Some(row) if m.tpe <:< row.appliedTo(elem.widen) =>
        liftOp(m, elem, row)
      // a staged block: a LEAF program of the row (`State.get[S]`) is
      // its one operation, staged; anything compound is refused — a
      // staged block's cost is one arm per mark, no interpreter inside
      case Some(row) if stage.isDefined && m.tpe.widen.dealias.derivesFrom(freeClass) =>
        injectedOp(m) match
          case Some((bs, op)) if op.tpe <:< row.appliedTo(elem.widen) =>
            val lifted = liftOp(op, elem, row)
            if bs.isEmpty then lifted else Block(bs, lifted)
          case _ =>
            report.errorAndAbort(
              s"a staged block stages OPERATIONS: the marked value ${m.tpe.show} is a program of the " +
                "row, not one operation of it — perform its operations in the block instead " +
                "(`State.modify(f)` is `val s = State.get.!?; State.set(f(s)).!?`)", at)
      // a program of a NARROWER row: `!Reader.ask[Db]` inside a block at
      // `Writer % String + Reader % Db + State % Long` (direct-narrow-row,
      // 2026-09-16). The row's own combinators — `State.modify`,
      // `Reader.ask`, `Writer.tell` — all answer at their OWN row, so
      // without this every one of them needs a hand-written `.plus[...]`
      // naming the other members, which is what made the test harness
      // in docs/direct-style.md unreadable. The coercion is RowLift's,
      // and its side condition is RowLift's too: an `In[F2, row]`
      // summoned HERE, so the compiler proves the membership and the
      // macro emits no cast of its own.
      case Some(row) => narrowRow(m, elem, row, at)
      case _ =>
        report.errorAndAbort(
          s"the marked value has type ${m.tpe.show} — neither this block's ${fT.show}" +
            anyRow.fold("")(r => s" nor an operation of its row ${r.show}"), at)

  /** `m.at[row]`, when m is a program of a row this block's row CONTAINS */
  def narrowRow(m: Term, elem: TypeRepr, row: TypeRepr, at: Position): Term =
    def refuse: Nothing = report.errorAndAbort(
      s"the marked value has type ${m.tpe.show} — neither this block's " +
        s"${TypeRepr.of[F].appliedTo(elem.widen).show} nor an operation of its row ${row.show}", at)
    val narrow: Option[TypeRepr] = m.tpe.widen.dealias.baseType(freeClass) match
      case AppliedType(_, List(r, e)) if e.widen =:= elem.widen => Some(r)
      case _ => None
    narrow match
      case None => refuse
      // membership by SUBTYPING, which is what it means for a union:
      // `Reader % E <:< (Writer % W + Reader % E + State % S)` holds
      // pointwise, while an `In` search on the reduced row does not
      // (see RowLift.into)
      case Some(r) if r <:< row =>
        val intoSym = TypeRepr.of[RowLift.type].typeSymbol.methodMember("into").head
        Apply(TypeApply(Ref(intoSym),
          List(Inferred(elem.widen), Inferred(r), Inferred(row))), List(m))
      case _ => refuse

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
          TypeRepr.of[Direct.Effect].appliedTo(lam)) match
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
        anyRow.exists(r => tpe0 <:< r.appliedTo(t.widen))
    }
