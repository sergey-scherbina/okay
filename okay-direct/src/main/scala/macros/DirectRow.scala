package okay
package macros


import okay.std.*
import scala.quoted.*

/**
 * THE BLOCK'S MONAD AND ROW: what F is — a program monad `A ! Row`
 * or any other — and what follows from it: which values can RUN as
 * bare statements (do-notation), which a statement would silently
 * drop, how an operation or a narrower program is lifted into the
 * row, whether the row holds a `Once` cell. Reads types; the only
 * terms it builds are the lifts themselves (`Free.Inject`,
 * `Row.into`).
 */
private[okay] trait DirectRow[F[_]] extends DirectPhase[F]:
  import q.reflect.*

  /** the program's class: the root's `Run`, `A ! Es` being `Run[Es, A]` */
  lazy val freeClass = Symbol.requiredClass("okay.Run")

  /**
   * A PROGRAM TYPE, TAKEN APART: `(effects, elem)` of an `A ! Es` — the
   * base type at `Run`, so a program built as `new Run[Es, A]` is one
   * too — or None.
   */
  def programOf(tpe: TypeRepr): Option[(TypeRepr, TypeRepr)] =
    tpe.widen.dealias.baseType(freeClass) match
      case AppliedType(_, List(row, elem)) => Some((row, elem))
      case _ => None

  /** the block's effects, if its F is the program monad `A ! Es` */
  lazy val rowOf: Option[TypeRepr] =
    programOf(TypeRepr.of[F].appliedTo(TypeRepr.of[scala.Unit])).map(_._1)

  lazy val stagedType: Symbol = Symbol.requiredModule("okay.freer.Handled").typeMember("Handled")

  /** the block's row when its F is `Handled[Row, R, *]` (specs/direct-
   * staged.md) — opaque, so it does not dealias to the function it is.
   * Kept apart from `rowOf`: the defer pre-pass and the Once cells are
   * a program's, and a staged block has neither */
  lazy val stagedRow: Option[TypeRepr] =
    TypeRepr.of[F].appliedTo(TypeRepr.of[scala.Unit]).dealias match
      case AppliedType(f, List(row, _, _)) if f.typeSymbol == stagedType => Some(row)
      case _ => None

  /** the row an operation must belong to, whichever carrier holds it */
  lazy val anyRow: Option[TypeRepr] = rowOf.orElse(stagedRow)

  private lazy val bothClass = Symbol.requiredClass("okay.core.Both")
  private lazy val noEffects = Symbol.requiredClass("okay.core.NoEffects")

  /** THE EFFECTS a sum names, one by one, the left first: `F + G` is
   * `Both[F, G, _]`, taken apart; `Pure` names none. BOUNDED by the sum
   * as written in the type */
  def effectsOf(es: TypeRepr): List[TypeRepr] =
    es.appliedTo(TypeRepr.of[Any]).dealias.simplified match
      case AppliedType(b, List(l, r, _)) if b.typeSymbol == bothClass => effectsOf(l) ++ effectsOf(r)
      case AppliedType(n, _) if n.typeSymbol == noEffects => Nil
      case _ => List(es)

  /** the effect of the block's effects an operation of type `tpe`
   * answering `elem` belongs to — the first, as the kernel's path finds
   * the nearest; a staged block's row is a union, and membership there
   * is subtyping */
  def effectOfOp(tpe: TypeRepr, elem: TypeRepr): Option[TypeRepr] =
    rowOf match
      case Some(row) => effectsOf(row).find(e => tpe <:< e.appliedTo(elem.widen))
      case None => stagedRow.filter(r => tpe <:< r.appliedTo(elem.widen))

  /** a GENERATOR block (specs/generators.md): the row names `Stop`,
   * `Gen`'s own effect — and then a for-yield in statement or final
   * position emits each value instead of collecting them */
  lazy val genRow: Boolean =
    rowOf.exists(r => effectsOf(r).exists(e => TypeRepr.of[Stop[Unit]] <:< e.appliedTo(TypeRepr.of[Unit])))

  /** the generator's element type: the `Writer % W` member's W */
  lazy val genW: Option[TypeRepr] =
    rowOf.flatMap { r =>
      effectsOf(r).map(_.appliedTo(TypeRepr.of[Any]).dealias.simplified).collectFirst {
        case AppliedType(w, List(wt, _)) if w.typeSymbol == Symbol.requiredClass("okay.std.Writer") => wt
      }
    }

  lazy val genType: Symbol = Symbol.requiredClass("okay.std.Gen")

  /** `Gen[W]`'s W, when the type is the generator (a value class over the program) */
  def genOf(tpe: TypeRepr): Option[TypeRepr] = tpe.widen.dealias match
    case AppliedType(g, List(w)) if g.typeSymbol == genType => Some(w)
    case _ => None

  /** in a generator block, a `Gen[W]` value — a marked `Gen.emit(w)`,
   * a recursive call's answer, a bare `Gen.stop` — is the program it
   * is: `Gen.program(v)`, and then the block's own F[Unit] */
  def unwrapGen(m: Term): Term =
    if !genRow then m
    else genOf(m.tpe) match
      case Some(_) => Select.unique(m, "program")
      case None => m

  /** the block's row names Once — the by-need cells have a handler */
  lazy val onceInRow: Boolean =
    rowOf.exists(r => effectsOf(r).exists(e => TypeRepr.of[Once[Unit]] <:< e.appliedTo(TypeRepr.of[Unit])))

  /**
   * SHARED OPERATION NODES (specs/effect-op-cost.md D2). `SharedOps`
   * holds ONE program of a fieldless operation; this table reads each
   * such val as the operation it holds, typed at the element the
   * reference was cast to, so a staged block stages it exactly as the
   * fresh node it replaced.
   */
  lazy val sharedNodes: Map[String, Type[?] => Term] = Map(
    // `new`, not `Get()`: the companion's `apply` is a method call the
    // inliner keeps for its effects, so the stage's inline match left a
    // live `Get$.apply()` per block — 1 600 B a run on StagedBenchmark,
    // read in its bytecode; a constructor it drops when nothing uses it
    "okay.std.SharedOps.getNode" -> { case '[e] => '{ new okay.std.State.Get[e, e]() }.asTerm },
    "okay.std.SharedOps.askNode" -> { case '[e] => '{ new okay.std.Reader.Ask[e, e]() }.asTerm })

  /** the LEAF PROGRAMS of the std that are plain defs (not inline), read
   * by a staged block as the operation they perform — `State.get[S]` is
   * `effect(Get[S, S]())`, `State.set[S](s)` is `effect(Update(Put(s)))`:
   * the operation and its element, or None */
  def leafOp(t: Term): Option[(Term, TypeRepr)] = t match
    case TypeApply(f, List(e)) => (sharedKey(f.symbol), e.tpe.widen.asType) match
      case ("okay.std.State.get", '[e]) => Some(('{ new okay.std.State.Get[e, e]() }.asTerm, TypeRepr.of[e]))
      case ("okay.std.Reader.ask", '[e]) => Some(('{ new okay.std.Reader.Ask[e, e]() }.asTerm, TypeRepr.of[e]))
      case _ => None
    case Apply(TypeApply(f, targs), List(x)) => (sharedKey(f.symbol), targs.map(_.tpe.widen.asType)) match
      case ("okay.std.State.set", List('[e])) =>
        Some(('{ new okay.std.State.Update[e, e](okay.std.State.Put[e](${ x.asExprOf[e] })) }.asTerm, TypeRepr.of[e]))
      case ("okay.std.State.modify", List('[e])) =>
        Some(('{ new okay.std.State.Update[e, e](okay.std.State.Modified[e](${ x.asExprOf[e => e] })) }.asTerm, TypeRepr.of[e]))
      case ("okay.std.State.update", List('[e], '[b])) =>
        Some(('{ new okay.std.State.Update[e, b](${ x.asExprOf[e => (b, e)] }) }.asTerm, TypeRepr.of[b]))
      case _ => None
    case _ => None

  /** a symbol's key in that table: its full name, the module's `$` off
   * (a reference from inside the object and one from outside name the
   * same val by different owners' spellings) */
  def sharedKey(sym: Symbol): String = sym.fullName.replace("$", "")

  /** the element a program term answers: `A` of `A ! Es`; any other
   * two-argument carrier its second */
  def elemOf(t: Term): Option[TypeRepr] = programOf(t.tpe) match
    case Some((_, e)) => Some(e)
    case None => t.tpe.widen.dealias match
      case AppliedType(_, List(_, a)) => Some(a)
      case _ => None

  private lazy val directModule = Symbol.requiredModule("okay.Direct")
  private lazy val effectInSym = directModule.methodMember("effectIn").head
  private lazy val liftedSym = directModule.methodMember("lifted").head

  /** `Direct.effectIn[E, Es, elem](op)` — the op a program needing the
   * block's effects, its path found from them */
  def injectTerm(op: Term, elem: TypeRepr, row: TypeRepr): Term =
    effectsOf(row).find(e => op.tpe <:< e.appliedTo(elem.widen)) match
      case Some(e) => Apply(TypeApply(Ref(effectInSym), List(Inferred(e), Inferred(row), Inferred(elem.widen))), List(op))
      case None => report.errorAndAbort(
        s"direct: the operation ${op.tpe.show} is not one of the block's effects ${row.show}", op.pos)

  /** the op lifted into THIS block's carrier: `effectIn(op)` for a
   * program, `stage.stage[elem](op)` for a staged one — the
   * inline match inside `stage` sees the operation term and picks
   * the arm at compile time */
  def liftOp(op: Term, elem: TypeRepr, row: TypeRepr): Term = stage match
    // `stripped`: an ascribed operation (`State.Get(): State[Int, Int]`)
    // reaches the inline match as the constructor, not the ascription
    case Some(st) => Apply(TypeApply(Select.unique(st, "stage"), List(Inferred(elem.widen))), List(stripped(op)))
    case None => injectTerm(op, elem, row)

  /** the root's program words, as a staged block reads them */
  lazy val effectSym: Symbol = calleeRoot('{ okay.effect[Option, Int](None) }.asTerm)
  lazy val pureSym: Symbol = calleeRoot('{ okay.pure[Int, Pure](0) }.asTerm)
  private lazy val runModule = Symbol.requiredModule("okay.Run")
  lazy val runFlatMap: Symbol = runModule.methodMember("flatMap").head
  lazy val runMap: Symbol = runModule.methodMember("map").head
  lazy val runAt: Symbol = runModule.methodMember("at").head

  /**
   * THE INLINER'S PROXIES, SUBSTITUTED. An inline method's by-value
   * argument becomes `val x$proxy = arg` in the `Inlined` bindings and
   * a name where it was used, and an inline match cannot reduce on a
   * name; a continuation arrives as `val f$proxy = (s => …)`. A proxy
   * whose right-hand side is PURE BY CONSTRUCTION — a lambda literal, a
   * literal, a name, a program node (`Inject`/`Pure`/`Bind`), an
   * operation of the row — goes back where its name stands. Any other
   * STAYS a binding, hoisted in front by `unwrap`: substituting even a
   * single-use `val s$proxy = i + 2` into `Set(s$proxy)` made the
   * inline match's scrutinee an expression, the inliner bound it to a
   * val, and the operation was allocated at run time — 1 600 B per run
   * on StagedBenchmark, found by the floor lane.
   */
  private def proxyFree(t: Term, row: TypeRepr): Term =
    // classified by the rhs's CORE (its own inlining wrappers off — a
    // combinator's proxy is `Inlined(call, [its proxies], Inject(…))`)
    def pureRhs(r: Term): Boolean =
      val (_, core) = unwrap(r)
      core match
        case Lambda(_, _) | Literal(_) | Ident(_) | This(_) => true
        case sel: Select if sel.symbol.flags.is(Flags.Module) => true
        // a shared operation node (effect-op-cost D2) is a stable val:
        // substituting it keeps the operation visible to the stager
        case r: Ref if sharedNodes.contains(sharedKey(r.symbol)) => true
        case x if programNode(x) => true
        case x => x.tpe.widen <:< row.appliedTo(TypeRepr.of[Any])
    def substitutable(v: ValDef): Boolean = v.rhs.exists(pureRhs)
    /** bindings split into (kept, substituted-into-the-rest) */
    def rewrite(stats: List[Statement], expr: Term): (List[Statement], Term) =
      stats match
        case Nil => (Nil, expr)
        case (v: ValDef) :: rest if substitutable(v) =>
          val (rest2, expr2) = rewrite(rest, expr)
          val rhs = v.rhs.get
          val sub = new TreeMap:
            override def transformTerm(tree: Term)(o: Symbol): Term = tree match
              case id: Ident if id.symbol == v.symbol => rhs
              case _ => super.transformTerm(tree)(o)
          (rest2.map(s => sub.transformTree(s)(Symbol.spliceOwner).asInstanceOf[Statement]),
            sub.transformTerm(expr2)(Symbol.spliceOwner))
        case s :: rest =>
          val (rest2, expr2) = rewrite(rest, expr)
          (s :: rest2, expr2)
    val m = new TreeMap:
      override def transformTerm(tree: Term)(o: Symbol): Term = tree match
        case Lambda(_, _) => super.transformTerm(tree)(o)
        case Inlined(call, bs, body) =>
          val (bs2, body2) = rewrite(bs.map(transformTree(_)(o).asInstanceOf[Statement]), transformTerm(body)(o))
          Inlined.copy(tree)(call, bs2.map(_.asInstanceOf[Definition]), body2)
        case Block(stats, expr) =>
          val (stats2, expr2) = rewrite(stats.map(transformTree(_)(o).asInstanceOf[Statement]), transformTerm(expr)(o))
          Block.copy(tree)(stats2, expr2)
        case _ => super.transformTerm(tree)(o)
    m.transformTerm(t)(Symbol.spliceOwner)

  /** a term with its wrappers off — inlining, ascription, the block
   * the inliner leaves its remaining proxies in, and Row's
   * coercions, which are casts (`p.asInstanceOf[A ! R]`): the program
   * inside a `.at[Row]` is the program. A lambda is left whole. The
   * bindings come out in front. */
  private def unwrap(t: Term): (List[Statement], Term) = t match
    case Lambda(_, _) => (Nil, t)
    case Inlined(_, bs, inner) => val (bs2, x) = unwrap(inner); (bs ++ bs2, x)
    case Typed(inner, _) => unwrap(inner)
    case Block(bs, inner) => val (bs2, x) = unwrap(inner); (bs ++ bs2, x)
    case TypeApply(Select(inner, "asInstanceOf"), _) => unwrap(inner)
    case _ => (Nil, t)

  /** a call of one of the root's program words (`effect`, `pure`,
   * `flatMap`, `map`) */
  def programNode(t: Term): Boolean =
    val f = calleeRoot(t)
    f == effectSym || f == pureSym || f == runFlatMap || f == runMap || f == runAt

  /**
   * A PROGRAM of the row, staged (specs/direct-staged.md v2): after
   * inlining, `Writer.tell(w)`, a for-comprehension over the row and
   * a hand-written chain are all one tree — `effect(op)`, `pure(a)`,
   * `m.flatMap(x => body)` / `m.map(x => body)` with the continuation a
   * lambda literal — and that tree is walked here into the same binds
   * a block of marks would emit: an operation becomes `stage(op)`, a
   * `pure` the carrier's `pure`, a bind a `bind` whose continuation
   * walks the lambda's body with its parameter re-bound. `None` where
   * a node is not one of those — a def call, `defer`, a continuation
   * that is a value — and the caller refuses, naming it.
   */
  def stageProgram(t: Term, row: TypeRepr): Option[Term] =
    def wrap(bs: List[Statement], x: Term): Term = if bs.isEmpty then x else Block(bs, x)
    def renamed(t: Term, from: Symbol, to: Term): Term =
      val r = new TreeMap:
        override def transformTerm(tree: Term)(o: Symbol): Term = tree match
          case id: Ident if id.symbol == from => to
          case _ => super.transformTerm(tree)(o)
      r.transformTerm(t)(Symbol.spliceOwner)
    def opLeaf(bs: List[Statement], op: Term, elem: TypeRepr): Option[Term] =
      if op.tpe.widen <:< row.appliedTo(elem) then Some(wrap(bs, liftOp(op, elem, row))) else None
    def walk(t: Term): Option[Term] =
      val (bs, core) = unwrap(t)
      core match
        case Apply(TypeApply(f, targs), List(op)) if f.symbol == effectSym =>
          opLeaf(bs, op, targs.last.tpe.widen)
        case ref: Ref if sharedNodes.contains(sharedKey(ref.symbol)) =>
          elemOf(t).flatMap(elem => opLeaf(bs, sharedNodes(sharedKey(ref.symbol))(elem.asType), elem))
        case leaf if leafOp(leaf).isDefined =>
          val (op, elem) = leafOp(leaf).get
          opLeaf(bs, op, elem)
        case Apply(TypeApply(f, targs), List(a)) if f.symbol == pureSym =>
          Some(wrap(bs, pureF(Typed(a, Inferred(targs.head.tpe.widen)))))
        // `Run.at[A, Es](m)[Es2](using w)`: the same program, more effects
        case Apply(TypeApply(Apply(TypeApply(f, _), List(m)), _), _) if f.symbol == runAt => walk(m).map(wrap(bs, _))
        // `Run.map[A, Es](m)[B](x => body)`: a bind into a pure
        case Apply(TypeApply(Apply(TypeApply(f, targs1), List(m)), targs2), List(k)) if f.symbol == runMap =>
          val aT = targs1.head.tpe.widen
          val bT = targs2.head.tpe.widen
          stripped(k) match
            case Lambda(List(param), body) =>
              walk(m).map(ms => wrap(bs, bind(ms, aT, bT)(v => pureF(Typed(renamed(body, param.symbol, v), Inferred(bT))))))
            case _ => None
        // `Run.flatMap[A, Es](m)[B, Es3](x => body)(using j)`
        case Apply(Apply(TypeApply(Apply(TypeApply(f, targs1), List(m)), targs2), List(k)), _) if f.symbol == runFlatMap =>
          val aT = targs1.head.tpe.widen
          val bT = targs2.head.tpe.widen
          stripped(k) match
            case Lambda(List(param), body) =>
              // the body is walked FIRST, against a fresh name for the
              // parameter, and the bind is emitted only if it walks —
              // a failed walk inside the bind's quote would be a cast
              // exception, not a refusal
              val fresh = Symbol.newVal(Symbol.spliceOwner, param.name, aT, Flags.EmptyFlags, Symbol.noSymbol)
              for
                ms <- walk(m)
                bodyStaged <- walk(renamed(body, param.symbol, Ref(fresh)))
              yield wrap(bs, bind(ms, aT, bT)(v => renamed(bodyStaged, fresh, v)))
            case _ => None
        // `_ <- m` in a for-comprehension lands as `() match { case () =>
        // rest }`, and a `case x =>` is a rename: a match with ONE
        // irrefutable case is its right-hand side
        case Match(scrut, List(CaseDef(pat, None, rhs))) =>
          pat match
            case Wildcard() => walk(rhs)
            case Literal(UnitConstant()) if scrut.tpe.widen =:= TypeRepr.of[Unit] => walk(rhs)
            case Bind(_, Wildcard()) => walk(renamed(rhs, pat.symbol, scrut))
            case _ => None
        case _ => None
    walk(proxyFree(t, row))

  /**
   * ONE mark, dispatched by type: an F[elem] of this block IS
   * already the term to bind; an operation of this block's row is
   * made a program of the block's effects first. Anything else is
   * refused with both possibilities named.
   */
  def markTerm(m0: Term, elem: TypeRepr, at: Position): Term =
    val m = unwrapGen(m0)
    val fT = TypeRepr.of[F].appliedTo(elem.widen)
    if m.tpe <:< fT then m
    else anyRow match
      case Some(row) if effectOfOp(m.tpe, elem).isDefined =>
        liftOp(m, elem, row)
      // a staged block: a program of the row is walked into binds
      // (stageProgram, v2) — a leaf is one `stage(op)`, a compound one
      // its binds; what cannot be walked is a program built at run
      // time, and a staged block's cost is one arm per operation with
      // no interpreter inside, so it is refused, naming the shape
      case Some(row) if stage.isDefined && m.tpe.widen.dealias.derivesFrom(freeClass) =>
        stageProgram(m, row).getOrElse(
          report.errorAndAbort(
            s"a staged block stages programs it can READ: the marked value ${m.tpe.show} is built at " +
              "run time (a def call, `defer`, a continuation that is not a lambda literal, a " +
              "program held in a val) — inline it, or perform its operations in the block", at))
      // a program needing FEWER effects: `!Reader.ask[Db]` inside a block
      // at `Writer % String + Reader % Db + State % Long` (direct-narrow-row,
      // 2026-09-16): the std's combinators answer at their OWN effects, and
      // the block's are more — `p.at[Es]`, each effect found among them at
      // compile time (the root's `Within`); a foreign monad's value, with its
      // `ForeignEffect` in scope (specs/direct-foreign-mark.md), the same
      case Some(row) =>
        foreignTerm(m) match
          case Some(lifted) => narrowRow(lifted, elem, row, at)
          case None => narrowRow(m, elem, row, at)
      case _ =>
        report.errorAndAbort(
          s"the marked value has type ${m.tpe.show} — neither this block's ${fT.show}" +
            anyRow.fold("")(r => s" nor an operation of its row ${r.show}"), at)

  lazy val foreignClass = Symbol.requiredClass("okay.ForeignEffect")

  /**
   * `ev.lift(m)` when `m: M[X]` is not a program and a
   * `ForeignEffect[M]` is in the implicit scope at the mark
   * (specs/direct-foreign-mark.md): a Future, a ZIO, a cats IO as an
   * `X ! G`, which `narrowRow` then widens into this block's effects.
   * For a type of several arguments the candidate constructor
   * fixes all but the LAST — `ZIO[R, E, A]` asks for `ZIO[R, E, _]`, as
   * `discardedMonadic` does. `None` when nothing is found, and the mark
   * falls to the refusal it always had.
   */
  def foreignTerm(m: Term): Option[Term] =
    val mt = m.tpe.widen.dealias
    if mt.derivesFrom(freeClass) then None
    else
      val candidates = mt.baseClasses.iterator.flatMap(c => mt.baseType(c) match
        case AppliedType(g, args) if args.nonEmpty =>
          val lam =
            if args.lengthIs == 1 then g
            else TypeLambda(List("X"), _ => List(TypeBounds.empty),
              tl => g.appliedTo(args.init :+ tl.param(0)))
          Some((lam, args.last))
        case _ => None)
      candidates.map((lam, x) =>
        Implicits.search(foreignClass.typeRef.appliedTo(lam)) match
          case found: ImplicitSearchSuccess =>
            val lift = foreignClass.methodMember("lift").head
            Some(found.tree.select(lift).appliedToType(x).appliedTo(m))
          case _ => None
      ).collectFirst { case Some(t) => t }

  /** `Evidence.All`'s constructor: every effect of one sum among another's */
  private lazy val allOf: TypeRepr = TypeRepr.of[okay.Evidence.All[Pure, Pure]] match
    case AppliedType(tc, _) => tc
    case other => report.errorAndAbort(s"direct: Evidence.All is ${other.show} (macro bug)")

  /** `Direct.lifted[elem, r, row](m)` — `m.at[row]` — when m is a
   * program whose effects are all among this block's */
  def narrowRow(m: Term, elem: TypeRepr, row: TypeRepr, at: Position): Term =
    def refuse: Nothing = report.errorAndAbort(
      s"the marked value has type ${m.tpe.show} — neither this block's " +
        s"${TypeRepr.of[F].appliedTo(elem.widen).show} nor an operation of its row ${row.show}", at)
    // the element by SUBTYPING, not equality: `Run[Es, +A]`, so a
    // `Nothing ! r` IS a `Unit ! r`
    programOf(m.tpe) match
      case Some((r, e)) if e.widen <:< elem.widen =>
        if r =:= row then m
        else Implicits.search(allOf.appliedTo(List(r, row))) match
          case _: ImplicitSearchSuccess =>
            Apply(TypeApply(Ref(liftedSym), List(Inferred(elem.widen), Inferred(r), Inferred(row))), List(m))
          case _ => refuse
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
   * type last, so tried first) and, for programs, from the `Run`
   * base type; the <:< check makes the guesses safe.
   */
  def runnableElem(p: Term): Option[TypeRepr] = runnableElemT(p.tpe)

  def runnableElemT(tpe0: TypeRepr): Option[TypeRepr] =
    // a bare `Gen[W]` statement in a generator block runs (it is Unit ! Row[W])
    if genRow && genOf(tpe0).isDefined then return Some(TypeRepr.of[Unit])
    val w = tpe0.widen.dealias
    val fromFree = programOf(w).map(_._2).toList
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
      tpe0 <:< TypeRepr.of[F].appliedTo(t.widen) || effectOfOp(tpe0, t).isDefined
    }
