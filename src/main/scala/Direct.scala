package okay

import scala.quoted.*
import scala.language.implicitConversions
import scala.annotation.implicitNotFound

/**
 * The flat block (specs/direct-macro.md): `direct[F] { ... m.reflect ... }`
 * rewrites a plain block into its monad's own flatMap binds, so
 * monadic values read as plain values with no for-comprehension.
 * The macro adds SYNTAX only — every emitted program is one the
 * user could write with flatMap by hand; multi-shot, short-circuit
 * and the stack discipline of the monad are inherited, not
 * re-implemented. (The first cut compiled to the Cont binds of
 * Monadic reflection instead; bench-direct priced that layer at
 * 3.3x over the hand-written chain and the target retired —
 * direct-flatmap-emission in the spec's Decisions.) The block is
 * scoped: marks under a lambda (other than the whitelisted loop
 * combinators) or a by-name argument, and a `finally` around marks,
 * are compile errors with the workaround named — refusing that
 * corner is the entire difference between these few hundred lines
 * and a general CPS transformer.
 */
object Direct:

  extension [F[_], A](m: F[A])
    /**
     * The mark: typechecks as A so the block typechecks BEFORE the
     * macro expands; the macro rewrites every call, so this body
     * only runs when a mark escapes outside a direct block — and
     * then it fails loudly rather than compiling to nothing.
     * The mark is a NAME, deliberately: of the symbols tried, .!
     * shadows object ! and .? is ambiguous with the Throws row-? —
     * the retirements are recorded in specs/direct-macro.md; .!?
     * and prefix ! survive as the symbolic spellings below.
     */
    def reflect: A = throw new IllegalStateException(
      "Direct.reflect outside a direct block — wrap the code in direct[F] { ... }")
    /**
     * THE SYMBOLIC SPELLING of the same mark, and since unwrap-glyph
     * it is the one the glyph itself points at.
     *
     * The history is three strikes and a return. `.!` shadowed the
     * object `!` for every file importing Direct.*, and went. `.?`
     * was retired because two other things answered it on a program
     * — `Throws.?`, which through the `into` conversion answered it
     * on ANY value and did nothing, and the row peek. `.!?` survived
     * as the symbol that collided with nothing.
     *
     * Both collisions are now gone: the Throws glyphs moved into
     * their type's companion, where a converted receiver cannot
     * reach them, and the peek took the word `peek`, which is what a
     * method that RUNS operations through a Handler should have been
     * called. So `.?` is below, and this stays — it is written in
     * the repository and in its docs, and a mark with two spellings
     * costs nothing (specs/unwrap-glyph.md).
     */
    def !? : A = throw new IllegalStateException(
      "Direct.!? outside a direct block — wrap the code in direct[F] { ... }")

    /**
     * The glyph, back where it was meant to be: `val x = m.?` inside
     * a `direct` block binds the program, exactly as `.reflect` and
     * `.!?` do — one mark, three spellings, one meaning, and the
     * meaning is the one `.?` already had on `A throws E`: give me
     * the value, the context deals with what was around it.
     *
     * Outside a block it throws like every other mark, and the
     * message says where it belongs.
     */
    def ? : A = throw new IllegalStateException(
      "Direct.? outside a direct block — wrap the code in direct[F] { ... }")

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

  /**
   * `w.tell`: inside a direct block, the mark on the Writer operation
   * `Writer(w)`, typed Unit so it reads as a STATEMENT; outside one,
   * the program `Writer.tell(w)`, `Unit ! Writer % W` (direct-tell,
   * 2026-09-16). One name, decided at the call site by whether the
   * block's capability `DirectCtx` is in scope — the same gate the
   * auto-colouring conversions stand behind — and `transparent`, so
   * each site gets its own type.
   *
   * Why a mark inside rather than the operation: a bare `Writer(w)`
   * on its own line runs too, by do-notation, but under `-Wall` the
   * typer flags it before the macro sees it (E176, an unused non-Unit
   * value), which is what the `: Unit` ascriptions in the tests were
   * for. This is that mark with the ascription built in. Inline, so
   * the macro sees the mark through the call; for an argument the
   * inliner cannot substitute it arrives under `Inlined` with a proxy
   * binding, which `compile` reads as a block.
   */
  extension [W](w: W)
    transparent inline def tell: Any =
      scala.compiletime.summonFrom:
        case _: DirectCtx[?] => Writer(w).reflect
        case _ => Writer.tell(w)

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

  /**
   * How a `direct` block treats a call at its own program type. It
   * reaches the macro as an ordinary `using` argument with a DEFAULT,
   * not as a summoned marker, and that is deliberate: an import whose
   * only reader is a macro is an "unused import" to the compiler, and
   * the warning would land in every user's build. As a parameter the
   * typer passes it, so the import that provides it counts as used.
   */
  sealed trait Deferral
  object Deferral:
    /** the default: defer every call at the block's program type */
    case object All extends Deferral
    /** `import Direct.eagerCalls.given`: build the call where it stands */
    case object Eager extends Deferral
    /** the default lives in the COMPANION, i.e. in implicit scope, and
     * the opt-out in an object you import, i.e. in lexical scope, which
     * wins without ambiguity (verified by running both, 2026-09-16).
     * A default ARGUMENT would have been the obvious spelling and is
     * wrong: on an `inline def` whose earlier parameter is the inline
     * block, `apply$default$N` takes that block again, so the whole
     * body is duplicated into the default's call — and a nested
     * `direct` block inside it then crashes `TreePickler`.
     *
     * Both givens are declared at their SINGLETON type, not at
     * `Deferral`: the macro reads the mode off the argument's TYPE,
     * and a given declared `given Deferral = Eager` hands it the type
     * `Deferral`, which says nothing. Written the wrong way first, and
     * caught by dumping the expansion: the import compiled, resolved,
     * and changed nothing. */
    given All.type = All

  /**
   * OPT OUT of deferring calls, by an import:
   *
   *     import okay.Direct.eagerCalls.given
   *
   * By DEFAULT a `direct` block defers every call at its own program
   * type — `f(x)` becomes `Free.delay(() => f(x))` — so that recursion
   * of any shape, self or mutual, in any position, trampolines through
   * the tree instead of the JVM stack. That default is safety: without
   * it a recursive block COMPILES, answers at small inputs and
   * overflows the stack at depth, which is the one failure mode a type
   * system cannot catch and a small test does not reach.
   *
   * It is not free, and the number is published rather than hidden:
   * one `Delay` and its thunk, 64 bytes, per call the block marks —
   * `compare/DirectBenchmark`'s `okayDirect` marks ten thousand of
   * them per invocation and pays 106.8 → 167.2 µs and 1 598 113 →
   * 2 238 113 B for a call (`step`) that never recurses. Where a block
   * is hot and provably not recursive, this import buys that back.
   *
   * What stays on with it: a call to the ENCLOSING def is still
   * deferred anywhere in the block, and a call to another def is still
   * deferred in TAIL position — both were measured free (allocation
   * identical to the byte). What you take on: a mutual call OUTSIDE
   * tail position is then built where it stands, and needs the word —
   * `!.tailcall(other(n))`, which is a deferral. `!`, `.reflect` and
   * `.!?` are NOT substitutes: they are marks ("bind this program"),
   * and a marked call is still built when the block is built.
   */
  object eagerCalls:
    given Deferral.Eager.type = Deferral.Eager

  /**
   * Whether a `direct` block may run INDEPENDENT binds together
   * (specs/applicative-static.md, stage 3).
   *
   * The same shape as `Deferral` above, for the same reasons: a
   * `using` parameter with the default given in this companion and
   * the opt-in in an object you import, both declared at their
   * SINGLETON type so the macro can read the mode off the argument's
   * type. A default argument would duplicate the block.
   */
  sealed trait Binds
  object Binds:
    /** the default: one bind after another, in the order written */
    case object Sequential extends Binds
    /** `import Direct.parallelBinds.given`: a run of independent
     * Async binds is spawned together and joined in order */
    case object Parallel extends Binds
    given Sequential.type = Sequential

  /**
   * OPT IN to running a block's INDEPENDENT binds at once:
   *
   *     import okay.Direct.parallelBinds.given
   *
   * Under it, a maximal run of two or more consecutive
   * `val x = m.?` statements whose right-hand sides do not mention a
   * name bound earlier in the same run is emitted as spawn-all-then-
   * join-all: N fibers started, then N joins in the order written.
   * A leaf qualifies when its OWN type is `X ! Async` — exactly
   * Async, read BEFORE the mark narrows it into this block's row — so
   * a block over `Async + Throws` parallelises its Async leaves too,
   * and anything else simply ends the run. (v1 could not: it decided
   * on the COMPILED leaf, by which time `RowLift.into` had lifted it,
   * and the import quietly did nothing in a wider row.
   * direct-parallel-wider-rows fixed that.)
   *
   * WHY THE FLAT SHAPE AND NOT THE APPLICATIVE ONE. `Par`
   * (specs/applicative-static.md, stage 1) joins leaves PAIRWISE, and
   * that was measured at about 5x a flat `parAll` at eight leaves: N
   * leaves become N joins and 2N fibers. A macro holds the whole
   * group at once, so it is the one position that never has to be
   * pairwise. Emitting `Par.app` chains would have taught the
   * compiler to write the expensive form.
   *
   * WHAT YOU TAKE ON, which is `parAll`'s bargain and not a new one:
   * the leaves interleave, so an effect one of them performs may now
   * be observed beside another's; and a failure surfaces where its
   * JOIN is reached, with the healthy siblings left to finish rather
   * than cancelled. A block whose binds must not interleave simply
   * does not import this.
   */
  object parallelBinds:
    given Binds.Parallel.type = Binds.Parallel

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

  /** rewrite the block: marks become flatMap binds, the result is
   * F[A]. `direct[F] { block }` names only the monad (the partial
   * type application trick); with an expected type both infer:
   * `val p: Int ! W = direct { ... }`. The block is a context
   * function so DirectCtx is ambient in it (plain blocks adapt);
   * the context lambda is stripped by the macro, never called. */
  inline def direct[F[_]]: DirectApply[F] = DirectApply[F]()

  final class DirectApply[F[_]](private val unit: Unit = ()) extends AnyVal:
    inline def apply[A](inline block: DirectCtx[F] ?=> A)
                       (using inline M: Monad[F], inline d: Deferral,
                        inline b: Binds): F[A] =
      ${ directImpl[F, A]('block, 'M, 'd, 'b) }

  /** a term with its inlining and ascription wrappers taken off */
  private def stripped(using q: Quotes)(t: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    t match
      case Inlined(_, Nil, inner) => stripped(inner)
      case Typed(inner, _) => stripped(inner)
      case _ => t

  @scala.annotation.publicInBinary
  private[okay] def directImpl[F[_] : Type, A: Type](block: Expr[DirectCtx[F] ?=> A],
                                               M: Expr[Monad[F]],
                                               d: Expr[Deferral],
                                               b: Expr[Binds])
                                              (using Quotes): Expr[F[A]] =
    import quotes.reflect.*
    // the block arrives as a context lambda; take its body — the
    // lambda is never called (see the entry note below)
    val topBody: Term = stripped(block.asTerm) match
      case Block(List(dd: DefDef), _: Closure) =>
        dd.rhs.getOrElse(report.errorAndAbort("empty direct block"))
          .changeOwner(Symbol.spliceOwner)
      case other => report.errorAndAbort(
        "a Direct mark as a non-literal block (a stored context-function value) " +
          "cannot be rewritten by direct's v1", other.pos)
    pipeline[F, A](topBody, M, d.asTerm.tpe <:< quotes.reflect.TypeRepr.of[Deferral.Eager.type],
      b.asTerm.tpe <:< quotes.reflect.TypeRepr.of[Binds.Parallel.type])

  /** the compilation pipeline at ONE monad — recursive for try
   * bodies (direct-try): a try's body is its own sub-block, compiled
   * at the try's type, then bound as one mark. The emission target
   * is plain `F[T]` terms (direct-flatmap-emission): a bind is a
   * Monad.flatMap call, the pure tail is M.pure — exactly the
   * program a careful hand would write, with no Cont layer between
   * the block and its monad. */
  /** the compiler's own evidence that V is a T, summoned at macro
   * time and spliced: the generated code upcasts through it, so no
   * `asInstanceOf` is ever emitted — the macro checked V <:< T on
   * the TypeReprs before asking, and a refusal here would be its bug */
  private def upcast[V: Type, T: Type](using q: Quotes): Expr[V <:< T] =
    Expr.summon[V <:< T].getOrElse(
      q.reflect.report.errorAndAbort(s"direct: ${Type.show[V]} is not a ${Type.show[T]} (macro bug)"))

  private def pipeline[F[_] : Type, A: Type](using q: Quotes)(topLevelBody: q.reflect.Term,
                                             M0: Expr[Monad[F]],
                                             eager: Boolean,
                                             parallel: Boolean): Expr[F[A]] =
    import q.reflect.*
    // ONE instance for the whole block: the summoned Monad
    // expression is hoisted to a val, so every emitted bind shares
    // it — the given for Free is a parameterized class the splice
    // would otherwise re-evaluate per bind. Built by hand (Symbol/
    // Block, not a quote) so the term stays in THIS Quotes context.
    val mmSym = Symbol.newVal(Symbol.spliceOwner, "mm$direct",
      TypeRepr.of[Monad[F]], Flags.EmptyFlags, Symbol.noSymbol)
    val mmVal = ValDef(mmSym, Some(M0.asTerm.changeOwner(mmSym)))
    val body = compileAll[F, A](topLevelBody, Ref(mmSym).asExprOf[Monad[F]], eager, parallel)
    Block(List(mmVal), body.asTerm).asExprOf[F[A]]

  private def compileAll[F[_] : Type, A: Type](using q: Quotes)(topLevelBody0: q.reflect.Term,
                                               M: Expr[Monad[F]],
                                               eager: Boolean,
                                               parallel: Boolean): Expr[F[A]] =
    import q.reflect.*
    val topLevelBody = topLevelBody0.changeOwner(Symbol.spliceOwner)

    val directSym = TypeRepr.of[Direct.type].typeSymbol
    val markSyms = (directSym.methodMember("reflect") ++ directSym.methodMember("!?")
      ++ directSym.methodMember("?") ++ directSym.methodMember("unary_!")).toSet
    val colorSyms = (directSym.methodMember("selfColor") ++
      directSym.methodMember("opColor") ++
      Symbol.requiredModule("okay.Free").methodMember("directColor")).toSet

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
        case AppliedType(f, List(row, _)) if f.typeSymbol == freeClass => Some(row)
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

    /**
     * Compile a term to either a pure term (no marks) or an F[elem]
     * term. Eff CARRIES its element type: F is invariant in general,
     * so the value type travels with the term instead of being
     * parsed back out of it. Pure children that precede an effectful
     * child are NOT reordered: the ANF hoisting below binds children
     * left to right.
     */
    enum Out:
      case Pure(t: Term)
      case Eff(f: Term, elem: TypeRepr) // f : F[elem]

    /** run f with the TypeRepr as a Type given */
    def tpe2[R](tpe: TypeRepr)(f: [T] => Type[T] ?=> R): R =
      tpe.asType match
        case '[t] => f[t]
      end match

    /** M.pure(t) at t's type */
    def pureF(t: Term): Term =
      tpe2(t.tpe.widen) { [T] => (tT: Type[T]) ?=>
        '{ $M.pure[T](${ t.asExprOf[T] }) }.asTerm
      }

    /**
     * ONE mark, dispatched by type: an F[elem] of this block IS
     * already the term to bind; an operation of this block's row is
     * injected into the row program first. Anything else is refused
     * with both possibilities named.
     */
    def markTerm(m: Term, elem: TypeRepr, at: Position): Term =
      val fT = TypeRepr.of[F].appliedTo(elem.widen)
      if m.tpe <:< fT then m
      else rowOf match
        case Some(row) if m.tpe <:< row.appliedTo(elem.widen) =>
          injectTerm(m, elem, row)
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
              rowOf.fold("")(r => s" nor an operation of its row ${r.show}"), at)

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

    /** fa.flatMap(v => body(v)) — body built from a reference to v,
     * returning an F[resTpe] term */
    def bind(fa: Term, vTpe: TypeRepr, resTpe: TypeRepr)(body: Term => Term): Term =
      tpe2(vTpe.widen) { [T] => (tT: Type[T]) ?=>
        tpe2(resTpe.widen) { [B] => (tB: Type[B]) ?=>
          val fa2 = fa.asExprOf[F[T]]
          '{
            $M.flatMap[T]($fa2)[B]((v: T) =>
              ${ body('v.asTerm).changeOwner(Symbol.spliceOwner)
                   .asExprOf[F[B]] })
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

    /** the compiled term at exactly F[tpe] — a no-op when the types
     * already agree; a real fmap when a branch narrows (F need not
     * be covariant, so ascription cannot widen it) */
    def asFAt(o: Out, tpe: TypeRepr): Term =
      tpe2(tpe.widen) { [T] => (tT: Type[T]) ?=>
        o match
          case Out.Pure(p) => '{ $M.pure[T](${ p.asExprOf[T] }) }.asTerm
          case Out.Eff(f, e) =>
            if e.widen =:= tpe.widen then f
            else tpe2(e.widen) { [V] => (tV: Type[V]) ?=>
              // a STATEMENT's value is discarded (Scala's own rule for a
              // Unit position) — said so, not cast; anything else is an
              // upcast the compiler vouches for
              if TypeRepr.of[T] =:= TypeRepr.of[Unit] then
                '{ $M.fmap[V, Unit](${ f.asExprOf[F[V]] }, (_: V) => ()) }.asTerm
              else
                val ev = upcast[V, T]
                '{ $M.fmap[V, T](${ f.asExprOf[F[V]] }, (x: V) => $ev(x)) }.asTerm
            }
      }

    def asF(o: Out): Term = o match
      case Out.Eff(f, _) => f
      case Out.Pure(p) => pureF(p)

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

    /** replace references to `sym` with `ref` */
    def subst(t: Term, sym: Symbol, ref: Term): Term =
      val m = new TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
          case id: Ident if id.symbol == sym => ref
          case _ => super.transformTerm(tree)(owner)
      m.transformTerm(t)(Symbol.spliceOwner)

    /** the block's row names Once — the by-need cells have a handler */
    lazy val onceInRow: Boolean =
      rowOf.exists(r => TypeRepr.of[Once[Unit]] <:< r.appliedTo(TypeRepr.of[Unit]))

    lazy val reflectMark: Symbol = directSym.methodMember("reflect").head

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

    /** replace every use of `sym` in the statements and the result with a
     * fresh `ref()` — INCLUDING a use wrapped in a colouring conversion,
     * which is how a colourless val of the block's program type is read
     * (direct-colourless-val): the conversion goes with the reference,
     * since `ref()` already stands at the element type */
    def substUses(stats: List[Statement], expr: Term, sym: Symbol, ref: () => Term): (List[Statement], Term) =
      substUsesBy(stats, expr, sym, ref, ref)

    /** the same, with the COLOURED use and the BARE use replaced by
     * different terms — a nested program def needs that: read as a value
     * it becomes a mark, read as a program it becomes the program */
    def substUsesBy(stats: List[Statement], expr: Term, sym: Symbol,
                    refColoured: () => Term, refBare: () => Term): (List[Statement], Term) =
      val m = new TreeMap:
        override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
          case Apply(Select(conv, "apply"), List(id: Ident))
            if colorSyms(calleeRoot(conv)) && id.symbol == sym => refColoured()
          case id: Ident if id.symbol == sym => refBare()
          case _ => super.transformTerm(tree)(owner)
      (stats.map(st => m.transformStatement(st)(Symbol.spliceOwner)), m.transformTerm(expr)(Symbol.spliceOwner))

    /**
     * How a local of the block's PROGRAM type is read in what follows:
     * COLOURED (the conversion applied to the bare reference — read as a
     * value, `x + 1`) or BARE (read as a program — marked, passed on,
     * `!.once(p)`).
     */
    def useKinds(stats: List[Statement], expr: Term, sym: Symbol): (Int, Int) =
      var coloured = 0
      var bare = 0
      val probe = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
          case Apply(Select(conv, "apply"), List(id: Ident))
            if colorSyms(calleeRoot(conv)) && id.symbol == sym => coloured += 1
          case id: Ident if id.symbol == sym => bare += 1
          case _ => super.traverseTree(tree)(owner)
      (stats :+ expr).foreach(t => probe.traverseTree(t)(Symbol.spliceOwner))
      (coloured, bare)

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
              if e.widen =:= elem.widen then c
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
        case Some(m) =>
          compile(m) match
            case Out.Pure(pm) => Out.Eff(markTerm(pm, t.tpe, t.pos), t.tpe.widen)
            case Out.Eff(cm, ce) => // marks inside the marked value: bind, then mark
              Out.Eff(bind(cm, ce, t.tpe)(v => markTerm(v, t.tpe, t.pos)), t.tpe.widen)
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
      case HofCall(xs, nm @ ("foreach" | "map"), param, lbody) if hasMark(lbody) =>
        hofLoop(t, xs, nm, param, lbody)

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
            val raw = pipeline[F, B](b.changeOwner(Symbol.spliceOwner), M, eager, parallel)
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
                val hp = pipeline[F, H](c.rhs.changeOwner(Symbol.spliceOwner), M, eager, parallel)
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
    def trivial(t: Term): Boolean = t match
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

    /**
     * A leaf this block may SPAWN — decided on the COMPILED leaf, not
     * on the syntax.
     *
     * The first cut matched the mark itself and found nothing: by the
     * time the macro sees `async(1).?` the inline expansion has
     * wrapped it in `Inlined` nodes carrying `$proxy` bindings, which
     * `stripped` does not go through, so `asMark` answered None on
     * every leaf and the whole feature was silently off (caught by a
     * fork COUNT of 0, which is why that assertion exists).
     *
     * `compile` already knows how to get through all of it, and what
     * it hands back is the program this block will bind. If that
     * program's type is `X ! Async` then it can be spawned, and
     * asking the type is both simpler and more honest than asking the
     * syntax.
     *
     * THE LIMIT THAT PUT ON v1 IS GONE (direct-parallel-wider-rows):
     * for a block over a WIDER row the compiled leaf has already been
     * lifted by `RowLift.into`, so its type is `X ! (Async + …)` and
     * it is not spawnable — the import used to do nothing there,
     * quietly. `markedProgram` below reads the mark's own argument
     * first, which is the program the author wrote, and only falls
     * back to the compiled leaf. The compiled leaf remains the road
     * for a mark shape the walk cannot take apart.
     */
    def spawnableLeaf(rhs: Term): Option[(Term, TypeRepr)] =
      if !hasMark(rhs) then None
      else
        // the mark's OWN argument first (a wider row still has Async
        // leaves), and the compiled leaf as the fallback
        markedProgram(rhs).orElse(
          compile(rhs) match
            case Out.Eff(c, e) => Some((c, e.widen))
            case Out.Pure(_) => None
        ).filter((c, e) => isAsyncProgram(c, e))

    /** is this term a program of EXACTLY the Async row? */
    def isAsyncProgram(c: Term, e: TypeRepr): Boolean =
      tpe2(e.widen) { [X] => (tX: Type[X]) ?=> c.tpe.widen <:< TypeRepr.of[X ! Async] }

    /**
     * THE MARK'S OWN ARGUMENT, before `markTerm` narrows it into this
     * block's row (direct-parallel-wider-rows).
     *
     * `compile` hands back a leaf already lifted by `RowLift.into`,
     * so in a block over `Async + Throws` its type is
     * `X ! (Async + Throws)` and `Async.spawn` will not take it — the
     * import did nothing there, quietly, and v1 said so. The program
     * the author WROTE is still `X ! Async`, and it is reachable: the
     * obstacle was never the narrowing, it was that inline expansion
     * wraps a leaf in `Inlined` nodes carrying `$proxy` bindings,
     * which `stripped` does not remove.
     *
     * `compile` already goes through them, by turning such an
     * `Inlined` into a `Block`. The same walk here KEEPS the bindings
     * around the mark's argument — `Block(bindings, program)` is a
     * term of the program's own type — so the extracted leaf is
     * self-contained and can be spawned where it stands.
     */
    def markedProgram(rhs: Term): Option[(Term, TypeRepr)] =
      def go(t: Term): Option[Term] = stripped(t) match
        case Inlined(_, bindings, inner) if bindings.nonEmpty =>
          go(Block(bindings, inner))
        case Block(stats, expr) =>
          go(expr).map(m => if stats.isEmpty then m else Block(stats, m))
        case other => asMark(other).map(stripped)
      go(rhs).flatMap { m =>
        m.tpe.widen.dealias.baseType(freeClass) match
          case AppliedType(_, List(_, e)) => Some((m, e.widen))
          case _ => None
      }

    /** does this tree mention any of these symbols? */
    def mentionsAny(t: Tree, syms: Set[Symbol]): Boolean =
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

    /** the maximal leading run of vals that may be spawned together */
    def independentRun(stats: List[Statement]): List[(ValDef, Term, TypeRepr)] =
      def go(rest: List[Statement], bound: Set[Symbol],
             acc: List[(ValDef, Term, TypeRepr)]): List[(ValDef, Term, TypeRepr)] =
        rest match
          case (vd @ ValDef(_, _, Some(rhs))) :: tail
            if !vd.symbol.flags.is(Flags.Lazy) && !vd.symbol.flags.is(Flags.Mutable) =>
            spawnableLeaf(rhs) match
              case Some((m, e)) if !mentionsAny(rhs, bound) =>
                go(tail, bound + vd.symbol, (vd, m, e) :: acc)
              case _ => acc.reverse
          case _ => acc.reverse
      go(stats, Set.empty, Nil)

    /** `async(Async.spawn(m))`, with its Fiber element type */
    def spawnOf(m: Term, e: TypeRepr, sched: Expr[Scheduler]): (Term, TypeRepr) =
      tpe2(e) { [X] => (tX: Type[X]) ?=>
        ('{ okay.async(okay.Async.spawn[X](${ m.asExprOf[X ! Async] })(using $sched)) }.asTerm,
          TypeRepr.of[Fiber[X]])
      }

    /** `fiber.joinAsync` */
    def joinOf(f: Term, e: TypeRepr): Term =
      tpe2(e) { [X] => (tX: Type[X]) ?=>
        '{ ${ f.asExprOf[Fiber[X]] }.joinAsync }.asTerm
      }

    /**
     * N spawns, then N joins in the order written — the FLAT shape,
     * which is `parAll`'s and not `Par`'s. The applicative spine was
     * measured at ~5x this at eight leaves because `app` is pairwise;
     * a macro holds the whole group, so it never has to be.
     *
     * Each fiber keeps its own element type, so nothing here casts.
     * The val keeps its symbol, re-bound to the join's value, exactly
     * as the sequential road does — a later def or assignment still
     * refers to it.
     */
    def parallelGroup(run: List[(ValDef, Term, TypeRepr)],
                      rest: List[Statement], expr: Term): Out =
      val sched = Expr.summon[Scheduler].getOrElse(report.errorAndAbort(
        "direct: `import Direct.parallelBinds.given` needs a Scheduler in scope — " +
          "it starts a fiber per independent bind (an `Async.spawn`), and there is no " +
          "given Scheduler here", run.head._1.pos))
      val resTpe = expr.tpe

      def joins(pairs: List[((ValDef, Term, TypeRepr), Term)]): Term =
        pairs match
          case Nil => asFAt(compileBlock(rest, expr), resTpe)
          case ((vd, _, e), fib) :: tail =>
            bind(markTerm(joinOf(fib, e), e, vd.pos), e, resTpe) { v =>
              Block(List(ValDef.copy(vd)(vd.name, vd.tpt, Some(v))), joins(tail))
            }

      def spawns(todo: List[(ValDef, Term, TypeRepr)],
                 done: List[((ValDef, Term, TypeRepr), Term)]): Term =
        todo match
          case Nil => joins(done.reverse)
          case (leaf @ (vd, m, e)) :: tail =>
            val (sp, fibTpe) = spawnOf(m, e, sched)
            bind(markTerm(sp, fibTpe, vd.pos), fibTpe, resTpe) { f =>
              spawns(tail, (leaf, f) :: done)
            }

      Out.Eff(spawns(run, Nil), resTpe.widen)

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
        // an `import` rides along, see stmtsTail
        case (im: Import) :: rest => wrapStat(im, rest, expr)
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

    /** keep a pure statement in front of the compiled rest */
    def wrapStat(s: Statement, rest: List[Statement], expr: Term): Out =
      compileBlock(rest, expr) match
        case Out.Pure(p) => Out.Pure(Block(List(s), p))
        case Out.Eff(c, e) => Out.Eff(Block(List(s), c), e)

    def wrapPure(vd: ValDef, rhs: Term, rest: List[Statement], expr: Term): Out =
      wrapStat(ValDef.copy(vd)(vd.name, vd.tpt, Some(rhs)), rest, expr)

    /**
     * DEEP RECURSION (deep-recursive-direct, specs/direct-macro.md): a
     * call to the def this block is the body of, at this block's own
     * program type, is deferred wherever it is marked or auto-coloured
     * — `fib(n - 1)` under `.reflect` or under `selfColor` becomes
     * `Free.delay(() => fib(n - 1))` under the same mark — so a direct
     * block may recurse a million deep and the recursion trampolines
     * through the tree instead of the JVM stack. With `import
     * Direct.given` and `scala.language.implicitConversions` in scope
     * the self-call needs no annotation at all:
     *
     *     def fib(n: Int): Long ! Pure = direct:
     *       if n < 2 then n.toLong else fib(n - 1) + fib(n - 2)
     *
     * which is the rewrite the deepRecursive macro of "Deep recursion
     * in Scala 3" (Kozak) performs on `TailRec`, at the one place it
     * matters: the DEFERRAL, since a self-call evaluated at
     * construction is the native recursion the block was written to
     * avoid. The rest — binds for `a + b`, branches, blocks — is the
     * lowering below, and her `TailRec` is this tree (tailcall =
     * `Delay`, flatMap = `Bind`, done = `Pure`, `.result` = `!.run`).
     *
     * Only a call to the ENCLOSING def, only at the block's program
     * type, only under a mark or a colouring conversion, and never
     * under a lambda (v1 does not look there): a self-call used as a
     * VALUE — passed along, stored — is left as it is.
     *
     * MUTUAL recursion needs no word either, by a second rule: a call
     * in the block's TAIL POSITION at the block's program type is
     * deferred whoever it calls (direct-tail-defer, 2026-09-16). The
     * macro expanding `isEven` cannot know that `isOdd` calls back —
     * a cycle spans files and the other def may not be typed yet — so
     * the enclosing-symbol test cannot see it; the tail position can,
     * and is exactly where a node costs one allocation and saves a
     * frame. Without it `else isOdd(n - 1)` COMPILED, answered at
     * small n and overflowed the stack at depth, which is the worst
     * failure mode there is.
     *
     * Two restrictions keep it honest. A call already wrapped in
     * `!.tailcall`/`Free.delay`/`Free.defer` is left alone, so the
     * explicit spelling does not pay for two nodes. And the rule is
     * only for a call (an `Apply`): a tail-position program VALUE is
     * not deferred, since nothing is built by naming it.
     */
    def deferSelfCalls(t: Term): Term =
      val self: Symbol =
        var o = Symbol.spliceOwner
        while o != Symbol.noSymbol && !o.isDefDef do o = o.owner
        o
      /** the block's lazy vals: a use of one whose rhs is effectful
       * becomes a MARK at compile time (direct-once), and a mark cannot
       * live under the thunk a deferral would build — so a call whose
       * arguments mention one is built where it stands (direct-tell,
       * 2026-09-16: `lazy val plan = !loadPlan(user.planId)` with `user`
       * lazy was "a mark under a lambda", the thunk's) */
      val lazySyms: Set[Symbol] =
        var acc = Set.empty[Symbol]
        val probe = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case vd: ValDef if vd.symbol.flags.is(Flags.Lazy) => acc += vd.symbol
              case _ => ()
            super.traverseTree(tree)(owner)
        probe.traverseTree(t)(Symbol.spliceOwner)
        acc
      def mentionsLazy(app: Term): Boolean =
        lazySyms.nonEmpty && {
          var found = false
          val probe = new TreeTraverser:
            override def traverseTree(tree: Tree)(owner: Symbol): Unit =
              if !found then tree match
                case id: Ident if lazySyms(id.symbol) => found = true
                case _ => super.traverseTree(tree)(owner)
          probe.traverseTree(app)(Symbol.spliceOwner)
          found
        }
      rowOf match
        case Some(row) if self != Symbol.noSymbol =>
          /** the element type of a self-call at this block's program type */
          def selfProgram(app: Term): Option[TypeRepr] =
            if calleeRoot(app) != self || mentionsLazy(app) || hasMark(app) then None
            else app.tpe.widen.dealias match
              case AppliedType(f, List(r, elem)) if f.typeSymbol == freeClass && r =:= row => Some(elem)
              case _ => None
          lazy val delayApply = Symbol.requiredModule("okay.Free").methodMember("delay").head
          lazy val reflectSym = directSym.methodMember("reflect").head
          /**
           * `Free.delay[row, elem](() => app)`, the thunk built as a term
           * under the owner it is being placed under — `at`, not
           * `Symbol.spliceOwner`. The difference is invisible while the
           * rewrite only fires at the top of a block, and fatal once it
           * fires inside one: a lambda owned by the splice while it sits
           * under a local definition pickles to `assertion failed: method
           * $anonfun`, reproducibly (direct-defer-default, 2026-09-16).
           */
          def delayed(app: Term, elem: TypeRepr, at: Symbol): Term =
            val thunk = Lambda(at, MethodType(Nil)(_ => Nil, _ => app.tpe.widen),
              (owner, _) => app.changeOwner(owner))
            Apply(TypeApply(Ref(delayApply), List(Inferred(row), Inferred(elem.widen))), List(thunk))
          /** Direct.reflect[F, elem](m) — a mark the pipeline recognises */
          def marked(m: Term, elem: TypeRepr): Term =
            Apply(TypeApply(Ref(reflectSym), List(Inferred(TypeRepr.of[F]), Inferred(elem.widen))), List(m))
          /**
           * Does this term ALREADY build a deferring node? `!.tailcall(p)`
           * is inline and reaches the macro as `Free.delay(…)` wrapped in
           * an `Inlined` WITH BINDINGS (the thunk proxy), which `stripped`
           * and `calleeRoot` both leave alone — so the first cut of this
           * check read no name and wrapped the node twice, measured in the
           * expansion. This peels whatever stands between, bindings and
           * all, and asks the name at the bottom.
           */
          def alreadyDefers(t: Term): Boolean =
            def root(x: Term): String = x match
              case Inlined(_, _, inner) => root(inner)
              case Typed(inner, _) => root(inner)
              case Block(_, expr) => root(expr)
              case Apply(f, _) => root(f)
              case TypeApply(f, _) => root(f)
              case other => if other.symbol == Symbol.noSymbol then "" else other.symbol.name
            val n = root(t)
            n == "delay" || n == "defer" || n == "tailcall"

          /** a program-typed call, whoever it calls — the tail rule's test */
          /** a CALL, under whatever the typer wrapped it in — a program
           * VALUE is not deferred, since naming one builds nothing */
          def isCall(t: Term): Boolean = t match
            case Inlined(_, _, inner) => isCall(inner)
            case Typed(inner, _) => isCall(inner)
            case Block(_, expr) => isCall(expr)
            case _: Apply => true
            case _ => false

          /**
           * Does the term carry definitions of its own — a lambda, a
           * local val or def, a nested `direct` block's context
           * function? Such a tree cannot simply be moved under a new
           * thunk: its symbols are owned where they stand, and the
           * owner surgery that would move them is not what this rule
           * is for. Measured rather than guessed: without this test,
           * `TestConditionDirect`'s `Condition.frame("skip")(ctx ?=>
           * …)` — a call whose argument is a nested block — crashed
           * the compiler in `TreePickler` with `assertion failed:
           * method $anonfun`, reproducibly. Such a call is left where
           * it stands; if it also recurses, `!.tailcall` is the word.
           */
          def carriesDefinitions(t: Term): Boolean =
            var found = false
            val probe = new TreeTraverser:
              override def traverseTree(tree: Tree)(owner: Symbol): Unit =
                if !found then tree match
                  case _: DefDef | _: ValDef | _: ClassDef => found = true
                  case Lambda(_, _) => found = true
                  case _ => super.traverseTree(tree)(owner)
            probe.traverseTree(t)(Symbol.spliceOwner)
            found

          /**
           * A call whose ARGUMENTS carry marks is not deferred
           * (direct-marked-args, 2026-09-16). The deferral wraps the
           * call in `Free.delay(() => …)`, a thunk this pass builds
           * before anything is compiled — so a mark in an argument
           * would land under a LAMBDA, and the general refusal fired
           * with a message naming a lambda the user never wrote
           * (`!k(!k(5))`, the natural spelling of a continuation
           * invoked twice). The arguments bind first and the call is
           * built inside the continuation, where there is nothing left
           * to defer; deep recursion through such a call is
           * `!.tailcall`'s job, as it is under `eagerCalls`.
           */
          def anyProgram(app: Term): Option[TypeRepr] =
            if !isCall(app) || carriesDefinitions(app) || mentionsLazy(app) || hasMark(app) then None
            else app.tpe.widen.dealias match
              case AppliedType(f, List(r, elem))
                if f.typeSymbol == freeClass && r =:= row && !alreadyDefers(app) => Some(elem)
              case _ => None

          /** the block's tail positions: what it finally hands back. A
           * call there is the last thing the block does, so deferring
           * it costs one node and spares the frame. */
          def tails(t: Term): Term = t match
            case Inlined(c, b, inner) => Inlined(c, b, tails(inner))
            case Typed(inner, tpt) => Typed(tails(inner), tpt)
            case Block(stats, expr) => Block(stats, tails(expr))
            case If(c, th, el) => If(c, tails(th), tails(el))
            case Match(sel, cases) =>
              Match(sel, cases.map(cd => CaseDef.copy(cd)(cd.pattern, cd.guard, tails(cd.rhs))))
            case Apply(TypeApply(fun, targs), List(m)) if markSyms(fun.symbol) =>
              anyProgram(stripped(m)) match
                case Some(elem) => Apply(TypeApply(fun, targs), List(delayed(stripped(m), elem, Symbol.spliceOwner)))
                case None => t
            case Apply(sel @ Select(conv, "apply"), List(m)) if colorSyms(calleeRoot(conv)) =>
              anyProgram(stripped(m)) match
                case Some(elem) => Apply(sel, List(delayed(stripped(m), elem, Symbol.spliceOwner)))
                case None => t
            case _ => t

          val walk = new TreeMap:
            override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
              // v1 does not look under lambdas: a self-call there is a value —
              // and a thunk the tail rule already built is one, so this is
              // also what stops the two rules deferring the same call twice
              case Lambda(_, _) => tree
              // already marked: defer the call, keep the one mark
              case Apply(TypeApply(fun, targs), List(m)) if markSyms(fun.symbol) =>
                (if eager then selfProgram(stripped(m)) else anyProgram(stripped(m))) match
                  case Some(elem) =>
                    val inner = super.transformTerm(stripped(m))(owner)
                    Apply(TypeApply(fun, targs), List(delayed(inner, elem, owner)))
                  case None => super.transformTerm(tree)(owner)
              // auto-coloured (`val x: Long = fib(n - 1)`): the same, under the conversion
              case Apply(sel @ Select(conv, "apply"), List(m)) if colorSyms(calleeRoot(conv)) =>
                (if eager then selfProgram(stripped(m)) else anyProgram(stripped(m))) match
                  case Some(elem) =>
                    val inner = super.transformTerm(stripped(m))(owner)
                    Apply(sel, List(delayed(inner, elem, owner)))
                  case None => super.transformTerm(tree)(owner)
              case app: Apply =>
                selfProgram(app) match
                  case Some(elem) =>
                    val inner = super.transformTerm(app)(owner)   // arguments first
                    marked(delayed(inner, elem, owner), elem)
                  case None => super.transformTerm(tree)(owner)
              case _ => super.transformTerm(tree)(owner)
          walk.transformTerm(tails(t))(Symbol.spliceOwner)
        case _ => t

    asFAt(compile(deferSelfCalls(topLevelBody)), TypeRepr.of[A]).asExprOf[F[A]]
