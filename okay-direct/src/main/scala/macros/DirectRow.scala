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
 * `Row.into`).
 */
private[okay] trait DirectRow[F[_]] extends DirectPhase[F]:
  import q.reflect.*

  /** the tree's class: `Freer` since freer-base-step-extractor (2026-09-29),
   * `A ! Row` being `Freer[Lift[Row], Unit, Unit, A]` */
  lazy val freeClass = Symbol.requiredClass("okay.Freer")
  private lazy val liftedClass = TypeRepr.of[okay.Freer.Lifted[Option]].typeSymbol
  private lazy val liftAlias = TypeRepr.of[okay.Freer.Lift[Option]].typeSymbol

  /**
   * A PROGRAM TYPE, TAKEN APART: `(row, elem)` of an `A ! Row`, or None.
   * One place for the shape, because the base is indexed now:
   * `Freer[G, S, R, A]` with the value LAST and the row inside `G`,
   * which arrives in three spellings depending on how far the compiler
   * has dealiased it — `Lift[Row]` (the alias applied),
   * `Lifted[Row]#L` (the projection it stands for), or the lambda
   * `[S, R, X] =>> Row[X]` the projection reduces to. The last is
   * eta-expanded back to a unary row, so `=:=` against a block's row
   * holds either way.
   */
  def programOf(tpe: TypeRepr): Option[(TypeRepr, TypeRepr)] =
    def rowOfSig(g: TypeRepr): Option[TypeRepr] = g match
      case AppliedType(l, List(row)) if l.typeSymbol == liftAlias => Some(row)
      case TypeRef(AppliedType(l, List(row)), _) if l.typeSymbol == liftedClass => Some(row)
      case tl: TypeLambda if tl.paramNames.length == 3 =>
        val unit = TypeRepr.of[scala.Unit]
        Some(TypeLambda(List("A"), _ => List(TypeBounds.empty), l => tl.appliedTo(List(unit, unit, l.param(0)))))
      case other =>
        val d = other.dealias
        if d == other then None else rowOfSig(d)
    tpe.widen.dealias match
      case AppliedType(f, List(g, _, _, elem)) if f.typeSymbol == freeClass => rowOfSig(g).map(row => (row, elem))
      case _ => None

  /** the block's effect row, if its F is the program monad A ! Row */
  lazy val rowOf: Option[TypeRepr] =
    programOf(TypeRepr.of[F].appliedTo(TypeRepr.of[scala.Unit])).map(_._1)

  lazy val stagedType: Symbol = Symbol.requiredModule("okay.Handled").typeMember("Handled")

  /** the block's row when its F is `Handled[Row, R, *]` (specs/direct-
   * staged.md) — opaque, so it does not dealias to the function it is.
   * Kept apart from `rowOf`: the defer pre-pass and the Once cells are
   * Free's, and a staged block has neither */
  lazy val stagedRow: Option[TypeRepr] =
    TypeRepr.of[F].appliedTo(TypeRepr.of[scala.Unit]).dealias match
      case AppliedType(f, List(row, _, _)) if f.typeSymbol == stagedType => Some(row)
      case _ => None

  /** the row an operation must belong to, whichever carrier holds it */
  lazy val anyRow: Option[TypeRepr] = rowOf.orElse(stagedRow)

  /** a GENERATOR block (specs/generators.md): the row names `Stop`,
   * `Gen`'s own effect — and then a for-yield in statement or final
   * position emits each value instead of collecting them */
  lazy val genRow: Boolean =
    rowOf.exists(r => TypeRepr.of[Stop[Unit]] <:< r.appliedTo(TypeRepr.of[Unit]))

  /** the generator's element type: the `Writer % W` member's W */
  lazy val genW: Option[TypeRepr] =
    def members(t: TypeRepr): List[TypeRepr] = t.dealias match
      case OrType(l, r) => members(l) ++ members(r)
      case m => List(m)
    rowOf.flatMap { r =>
      members(r.appliedTo(TypeRepr.of[Any])).collectFirst {
        case AppliedType(w, List(wt, _)) if w.typeSymbol == Symbol.requiredClass("okay.Writer") => wt
      }
    }

  lazy val genType: Symbol = Symbol.requiredClass("okay.Gen")

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
    rowOf.exists(r => TypeRepr.of[Once[Unit]] <:< r.appliedTo(TypeRepr.of[Unit]))

  lazy val injectApply: Symbol =
    Symbol.requiredModule("okay.Free.Inject").methodMember("apply").head

  /**
   * SHARED OPERATION NODES (specs/effect-op-cost.md D2). `State.get`
   * and `Reader.ask` answer ONE shared `Inject` of a fieldless
   * operation instead of a fresh one, so after inlining they are a
   * reference to a val, not a `Free.Inject(op)` call. This table reads
   * each such val as the operation it holds, typed at the element the
   * reference was cast to, so a staged block stages it exactly as the
   * fresh node it replaced.
   */
  lazy val sharedNodes: Map[String, Type[?] => Term] = Map(
    // `new`, not `Get()`: the companion's `apply` is a method call the
    // inliner keeps for its effects, so the stage's inline match left a
    // live `Get$.apply()` per block — 1 600 B a run on StagedBenchmark,
    // read in its bytecode; a constructor it drops when nothing uses it
    "okay.SharedOps.getNode" -> { case '[e] => '{ new okay.State.Get[e, e]() }.asTerm },
    "okay.SharedOps.askNode" -> { case '[e] => '{ new okay.Reader.Ask[e, e]() }.asTerm })

  /** a symbol's key in that table: its full name, the module's `$` off
   * (a reference from inside the object and one from outside name the
   * same val by different owners' spellings) */
  def sharedKey(sym: Symbol): String = sym.fullName.replace("$", "")

  /** the element a program term answers: `A` of `A ! F` — the LAST
   * argument of the tree; any other two-argument carrier its second */
  def elemOf(t: Term): Option[TypeRepr] = t.tpe.widen.dealias match
    case AppliedType(f, args) if f.typeSymbol == freeClass && args.nonEmpty => Some(args.last)
    case AppliedType(_, List(_, a)) => Some(a)
    case _ => None

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

  lazy val pureApply: Symbol =
    Symbol.requiredModule("okay.Free.Return").methodMember("apply").head
  lazy val bindApply: Symbol =
    Symbol.requiredModule("okay.Free.Bind").methodMember("apply").head
  /** THE SAME NODES BY THEIR OTHER NAMES (freer-base-step-extractor,
   * 2026-09-29): `Free.Inject`/`Return`/`Bind` are wrapper objects over
   * the enum `Freer`, and an INLINE door — `effect`, `pure`,
   * `flatMap`, `map` — expands to the enum case's own `apply`, so a
   * staged program reaches the reader as `Freer.Inject.apply[G, S, R,
   * A](op)` as often as `Free.Inject.apply[F, A](op)`. Both spell one
   * node; the element is the LAST type argument in either */
  lazy val injectApplies: Set[Symbol] = Set(injectApply, Symbol.requiredModule("okay.Freer.Inject").methodMember("apply").head)
  lazy val pureApplies: Set[Symbol] = Set(pureApply, Symbol.requiredModule("okay.Freer.Return").methodMember("apply").head)
  lazy val bindApplies: Set[Symbol] = Set(bindApply, Symbol.requiredModule("okay.Freer.Bind").methodMember("apply").head)
  /** `map`'s continuation (one-bind-hot-steps): `new Freer.Mapped(f)` runs
   * as `a => Free.Return(f(a))`, and the stager reads it as that */
  lazy val mappedClass: Symbol = Symbol.requiredClass("okay.Freer.Mapped")

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
        case Apply(TypeApply(f, _), _) if injectApplies(f.symbol) || pureApplies(f.symbol) || bindApplies(f.symbol) => true
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

  /**
   * A PROGRAM of the row, staged (specs/direct-staged.md v2): after
   * inlining, `State.modify(f)`, a for-comprehension over the row and
   * a hand-written chain are all one tree — `Free.Inject(op)`,
   * `Free.Return(a)`, `Free.Bind(m, x => body)` with the continuation a
   * lambda literal — and that tree is walked here into the same binds
   * a block of marks would emit: an operation becomes `stage(op)`, a
   * `Pure` the carrier's `pure`, a `Bind` a `bind` whose continuation
   * walks the lambda's body with its parameter re-bound. `None` where
   * a node is not one of those — a def call, `Free.delay`, a
   * continuation that is a value — and the caller refuses, naming it.
   */
  def stageProgram(t: Term, row: TypeRepr): Option[Term] =
    def wrap(bs: List[Statement], x: Term): Term = if bs.isEmpty then x else Block(bs, x)
    def walk(t: Term): Option[Term] =
      val (bs, core) = unwrap(t)
      core match
        case Apply(TypeApply(f, targs), List(op)) if injectApplies(f.symbol) =>
          val elem = targs.last.tpe.widen
          if stripped(op).tpe.widen <:< row.appliedTo(elem) then Some(wrap(bs, liftOp(op, elem, row))) else None
        case ref: Ref if sharedNodes.contains(sharedKey(ref.symbol)) =>
          elemOf(t).flatMap { elem =>
            val op = sharedNodes(sharedKey(ref.symbol))(elem.asType)
            if op.tpe.widen <:< row.appliedTo(elem) then Some(wrap(bs, liftOp(op, elem, row))) else None
          }
        case Apply(TypeApply(f, targs), List(a)) if pureApplies(f.symbol) =>
          Some(wrap(bs, pureF(Typed(a, Inferred(targs.last.tpe.widen)))))
        case Apply(TypeApply(f, targs), List(m, k)) if bindApplies(f.symbol) =>
          // `Free.Bind[F, A, B]` or `Freer.Bind[G, S, T, R, A, B]`: A and B
          // are the last two either way
          val aT = targs(targs.length - 2).tpe.widen
          val bT = targs.last.tpe.widen
          // `map`'s continuation, `new Free.Mapped(x => body)`: a bind whose
          // continuation is the pure `x => Return(body)` (one-bind-hot-steps)
          def mappedFn(t: Term): Option[Term] = t match
            case Apply(TypeApply(Select(New(tpt), _), _), List(fn)) if tpt.tpe.typeSymbol == mappedClass => Some(fn)
            case Apply(Select(New(tpt), _), List(fn)) if tpt.tpe.typeSymbol == mappedClass => Some(fn)
            case _ => None
          stripped(k) match
            case kk if mappedFn(kk).isDefined =>
              stripped(mappedFn(kk).get) match
                case Lambda(List(param), body) =>
                  def renamedTo(to: Term): Term =
                    val r = new TreeMap:
                      override def transformTerm(tree: Term)(o: Symbol): Term = tree match
                        case id: Ident if id.symbol == param.symbol => to
                        case _ => super.transformTerm(tree)(o)
                    r.transformTerm(body)(Symbol.spliceOwner)
                  walk(m).map(ms => wrap(bs, bind(ms, aT, bT)(v => pureF(Typed(renamedTo(v), Inferred(bT))))))
                case _ => None
            case Lambda(List(param), body) =>
              // the body is walked FIRST, against a fresh name for the
              // parameter, and the bind is emitted only if it walks —
              // a failed walk inside the bind's quote would be a cast
              // exception, not a refusal
              val fresh = Symbol.newVal(Symbol.spliceOwner, param.name, aT, Flags.EmptyFlags, Symbol.noSymbol)
              def renamed(t: Term, from: Symbol, to: Term): Term =
                val r = new TreeMap:
                  override def transformTerm(tree: Term)(o: Symbol): Term = tree match
                    case id: Ident if id.symbol == from => to
                    case _ => super.transformTerm(tree)(o)
                r.transformTerm(t)(Symbol.spliceOwner)
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
            case Bind(_, Wildcard()) =>
              val sym = pat.symbol
              val r = new TreeMap:
                override def transformTerm(tree: Term)(o: Symbol): Term = tree match
                  case id: Ident if id.symbol == sym => scrut
                  case _ => super.transformTerm(tree)(o)
              walk(r.transformTerm(rhs)(Symbol.spliceOwner))
            case _ => None
        case _ => None
    walk(proxyFree(t, row))

  /**
   * ONE mark, dispatched by type: an F[elem] of this block IS
   * already the term to bind; an operation of this block's row is
   * injected into the row program first. Anything else is refused
   * with both possibilities named.
   */
  def markTerm(m0: Term, elem: TypeRepr, at: Position): Term =
    val m = unwrapGen(m0)
    val fT = TypeRepr.of[F].appliedTo(elem.widen)
    if m.tpe <:< fT then m
    else anyRow match
      case Some(row) if m.tpe <:< row.appliedTo(elem.widen) =>
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
              "run time (a def call, `Free.delay`, a continuation that is not a lambda literal, a " +
              "program held in a val) — inline it, or perform its operations in the block", at))
      // a program of a NARROWER row: `!Reader.ask[Db]` inside a block at
      // `Writer % String + Reader % Db + State % Long` (direct-narrow-row,
      // 2026-09-16). The row's own combinators — `State.modify`,
      // `Reader.ask`, `Writer.tell` — all answer at their OWN row, so
      // without this every one of them needs a hand-written `.plus[...]`
      // naming the other members, which is what made the test harness
      // in docs/direct-style.md unreadable. The coercion is Row's,
      // and its side condition is Row's too: an `In[F2, row]`
      // summoned HERE, so the compiler proves the membership and the
      // macro emits no cast of its own.
      // a FOREIGN monad's value, with its `Layered` layer in scope
      // (direct-layers-instances): `list.?` inside `reify[List, …]` is
      // `Layered.reflect(list)`, a `X ! Shift % ?`, which `narrowRow` then
      // widens to this block's row by Row's own proof
      case Some(row) =>
        layerTerm(m) match
          case Some(reflected) =>
            if !(TypeRepr.of[[A] =>> okay.Shift[?, A]].appliedTo(elem.widen) <:< row.appliedTo(elem.widen)) then
              report.errorAndAbort(
                s"the marked value ${m.tpe.show} has a Layered layer in scope, and reflecting into it " +
                  s"is a capture (Shift % ?), but this block's row ${row.show} has no Shift % ?: " +
                  "write the block at a row with Shift % ? (the layer's reify needs one anyway)", at)
            narrowRow(reflected, elem, row, at)
          case None =>
            foreignTerm(m) match
              case Some(lifted) => narrowRow(lifted, elem, row, at)
              case None => narrowRow(m, elem, row, at)
      case _ =>
        report.errorAndAbort(
          s"the marked value has type ${m.tpe.show} — neither this block's ${fT.show}" +
            anyRow.fold("")(r => s" nor an operation of its row ${r.show}"), at)

  lazy val layeredModule = Symbol.requiredModule("okay.Layered")
  lazy val reflectCapability = Symbol.requiredClass("okay.Layered.Reflect")

  /**
   * `Layered.reflect(m)[R, Pure]` when `m: M[X]` is not a program and a
   * `Layered.Reflect[M, R]` is in the implicit scope at the mark: the
   * layer's own reflect, found by the same search a hand-written
   * `m.reflect` would make. `None` when there is no layer, and the mark
   * then falls to the refusal it always had.
   */
  def layerTerm(m: Term): Option[Term] =
    val mt = m.tpe.widen.dealias
    if mt.derivesFrom(freeClass) then None
    else
      // the value's own class first, then its parents, one type
      // parameter each: `Some[Int]` and `None | Some[Int]` (an `if`'s
      // type) reach `Option`'s layer, `List[Int]` reaches `List`'s
      val candidates = mt.baseClasses.iterator.flatMap(c => mt.baseType(c) match
        case AppliedType(tycon, List(x)) => Some((tycon, x))
        case _ => None)
      candidates.map((tycon, x) =>
        Implicits.search(reflectCapability.typeRef.appliedTo(List(tycon, TypeBounds.empty))) match
          case found: ImplicitSearchSuccess =>
            (found.tree.tpe.widen.dealias.baseType(reflectCapability), Implicits.search(TypeRepr.of[okay.At])) match
              case (AppliedType(_, List(_, r)), at: ImplicitSearchSuccess) =>
                val reflect = layeredModule.methodMember("reflect").head
                Some(Ref(layeredModule).select(reflect).appliedToTypes(List(tycon, x)).appliedTo(m)
                  .appliedToTypes(List(r, TypeRepr.of[okay.Pure])).appliedTo(found.tree, at.tree))
              case _ => None
          case _ => None
      ).collectFirst { case Some(t) => t }

  lazy val foreignClass = Symbol.requiredClass("okay.ForeignEffect")

  /**
   * `ev.lift(m)` when `m: M[X]` is not a program and a
   * `ForeignEffect[M]` is in the implicit scope at the mark
   * (specs/direct-foreign-mark.md): a Future, a ZIO, a cats IO as an
   * `X ! G`, which `narrowRow` then widens into this block's row by
   * subtyping. For a type of several arguments the candidate constructor
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

  /** `m.at[row]`, when m is a program of a row this block's row CONTAINS */
  def narrowRow(m: Term, elem: TypeRepr, row: TypeRepr, at: Position): Term =
    def refuse: Nothing = report.errorAndAbort(
      s"the marked value has type ${m.tpe.show} — neither this block's " +
        s"${TypeRepr.of[F].appliedTo(elem.widen).show} nor an operation of its row ${row.show}", at)
    // the element by SUBTYPING, not equality (free-answer-variance,
    // 2026-09-23): `Free[F, +A]`, so an inlined `raise[E, Unit](e)` is
    // an `Inject[Throws % E, Nothing]`, and `Nothing ! r` IS a `Unit ! r`
    val narrow: Option[TypeRepr] = programOf(m.tpe.widen.dealias.baseType(freeClass)) match
      case Some((r, e)) if e.widen <:< elem.widen => Some(r)
      case _ => None
    narrow match
      case None => refuse
      // membership by SUBTYPING, which is what it means for a union:
      // `Reader % E <:< (Writer % W + Reader % E + State % S)` holds
      // pointwise, while an `In` search on the reduced row does not
      // (see Row.into)
      case Some(r) if r <:< row =>
        val intoSym = TypeRepr.of[Row.type].typeSymbol.methodMember("into").head
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
    // a bare `Gen[W]` statement in a generator block runs (it is Unit ! Row[W])
    if genRow && genOf(tpe0).isDefined then return Some(TypeRepr.of[Unit])
    val w = tpe0.widen.dealias
    val fromFree = w.baseType(freeClass) match
      case AppliedType(_, args) if args.nonEmpty => List(args.last)
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
