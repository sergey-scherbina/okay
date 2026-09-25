package okay

import scala.quoted.*
import scala.language.implicitConversions
import scala.annotation.tailrec

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

  /** Compatibility aliases for the small core-level direct support
   * surface. The DSL itself, including all macros, lives in this
   * optional module. */
  type DirectCtx[F[_]] = okay.DirectCtx[F]

  /** Marker used by the optional DSL for auto-colouring operations.
   * It refines the core's lightweight capability so core `Effect`
   * never needs a dependency back on this module. */
  @scala.annotation.implicitNotFound("no Direct.Effect[${G}]: auto-coloring is OPT-IN per signature.\nRegister the effect once — `given Direct.Effect[${G}] with {}` — or use the explicit marks\n(.reflect / .!? / !prog), which need no marker.")
  trait Effect[G[_]] extends okay.DirectEffect[G]

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
   * on the COMPILED leaf, by which time `Row.into` had lifted it,
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
  /** the block's own monadic values auto-color: F[A] as A — a
   * phantom, the macro rewrites every call */
  given selfColor[F[_], A](using DirectCtx[F]): Conversion[F[A], A] =
    _ => throw new IllegalStateException(
      "Direct auto-coloring escaped macro rewriting — this call belongs inside direct[F] { ... }")

  /** marked operations auto-color: G[A] as A, row membership checked
   * by the macro exactly as for .!? */
  given opColor[F[_], G[_], A](using DirectCtx[F], okay.DirectEffect[G]): Conversion[G[A], A] =
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
                       (using inline M: Applicative[F], inline d: Deferral,
                        inline b: Binds): F[A] =
      ${ directImpl[F, A]('block, 'M, 'd, 'b) }

  /**
   * The marks on a GENERATOR value (specs/generators.md): `Gen[W]` is
   * a value class over `Unit ! Gen.Row[W]`, so the generic mark would
   * unify it as `F[A]` with `A = W` and answer a `W` that never was —
   * these say what running a generator answers: `Unit`. More specific
   * than the generic extension, so they win; the macro reads them as
   * marks (their symbols carry the same names) and takes `.program`.
   */
  /**
   * `for x <- src do body` over a SOURCE, inside a block only
   * (specs/direct-loops.md v3): a `Pull[A, G]` has no `foreach` of its
   * own, because the loop is a PROGRAM and a program in statement
   * position is the discarded-program error build.sbt escalates —
   * rightly, since outside a block nothing would run it. This
   * extension needs the block's ambient `DirectCtx`, so it exists
   * only where the macro will rewrite it, is typed Unit there, and
   * is never called: the macro replaces it with the loop as a
   * program. Outside a block write `src.loop(f)`, the program by name.
   */
  extension [A, G[+_]](src: Pull[A, G])
    def foreach[F[_]](f: A => Unit)(using DirectCtx[F]): Unit = throw new IllegalStateException(
      "a source loop is rewritten by the direct macro and never called; outside a block use src.loop(f)")

  extension [W](g: Gen[W])
    def reflect: Unit = throw new IllegalStateException(
      "Direct.reflect outside a direct block — wrap the code in direct[F] { ... }")
    def !? : Unit = g.reflect
    def unary_! : Unit = g.reflect

  /**
   * A GENERATOR block (specs/generators.md): the row is `Gen[W]`'s —
   * `Writer % W + Stop` — so `Gen.emit(w).!?`, a bare `Writer(w)`
   * statement and `Gen.stop.!?` are its words, `while`/`if`/recursion
   * work as in any block, and `for x <- xs yield e` in statement or
   * final position EMITS each `e` (the only place `yield` means that:
   * the block says it is a generator). The block's own value is
   * dropped; what it produces is read lazily through `Gen`'s readers.
   */
  inline def generator[W](inline block: DirectCtx[[A] =>> A ! Gen.Row[W]] ?=> Any): Gen[W] =
    // DELAYED: a block's program is built when the block is evaluated,
    // and building it runs every statement before the first yield and
    // initialises every block-local `var` — once. Python runs nothing
    // before `next()`, and a second read starts fresh; the thunk gives
    // both (a generator is re-runnable BECAUSE its vars are re-made)
    Gen.fromProgram(Free.delay(() => direct[[A] =>> A ! Gen.Row[W]](block).map(_ => ())))

  /**
   * The block with its handler known at the call site
   * (specs/direct-staged.md): over `Handled[Sig, R, *]`, every marked
   * operation of the row — and every marked LEAF program, which is
   * what `State.get`, `Writer.tell`, `Reader.ask` inline to — is
   * emitted as `st.stage(op)`, whose inline match the compiler
   * reduces on the operation as written. No `split`, no tree: the
   * block is a function of its continuation. `st` is an INLINE
   * parameter so that its own type — the object's, where `stage` is
   * a concrete inline member — is what the macro sees; the macro
   * hoists it to one val for the block. Calls are never deferred
   * (`Func` has no `delay`): a staged block is not stack-safe on a
   * left-nested chain, and says so in its spec.
   */
  inline def staged[Sig[+_], R](inline st: Stager[Sig, R])[A]
                               (inline block: DirectCtx[Handled[Sig, R, *]] ?=> A)
                               (using inline b: Binds): Handled[Sig, R, A] =
    ${ stagedImpl[Sig, R, A]('st, 'block, 'b) }

  @scala.annotation.publicInBinary
  private[okay] def stagedImpl[Sig[+_] : Type, R: Type, A: Type](st: Expr[Stager[Sig, R]],
                                                                 block: Expr[DirectCtx[Handled[Sig, R, *]] ?=> A],
                                                                 b: Expr[Binds])
                                                                (using Quotes): Expr[Handled[Sig, R, A]] =
    import quotes.reflect.*
    type F[X] = Handled[Sig, R, X]
    val topBody: Term = blockBody[F, A](block).asTerm
    val m = Expr.summon[Monad[F]].getOrElse(
      report.errorAndAbort("direct.staged: no Monad[Handled[Sig, R, *]] (macro bug)"))
    macros.DirectCompiler.pipeline[F, A](topBody, m,
      eager = true,
      parallel = b.asTerm.tpe <:< TypeRepr.of[Binds.Parallel.type],
      stage0 = Some(st.asTerm))

  /** a term with its inlining and ascription wrappers taken off */
  @tailrec private[okay] def stripped(using q: Quotes)(t: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    t match
      case Inlined(_, Nil, inner) => stripped(inner)
      case Typed(inner, _) => stripped(inner)
      case _ => t

  /** the block arrives as a context lambda; this is its body — the
   * lambda is never called (see the entry note below). An `Expr`, so
   * the phase probes (src/test/scala/DirectProbe.scala) can take the
   * same body into a compiler of their own Quotes path. */
  private[okay] def blockBody[F[_] : Type, A: Type](block: Expr[DirectCtx[F] ?=> A])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    stripped(block.asTerm) match
      case Block(List(dd: DefDef), _: Closure) =>
        dd.rhs.getOrElse(report.errorAndAbort("empty direct block"))
          .changeOwner(Symbol.spliceOwner).asExpr
      case other => report.errorAndAbort(
        "a Direct mark as a non-literal block (a stored context-function value) " +
          "cannot be rewritten by direct's v1", other.pos)

  @scala.annotation.publicInBinary
  private[okay] def directImpl[F[_] : Type, A: Type](block: Expr[DirectCtx[F] ?=> A],
                                               M: Expr[Applicative[F]],
                                               d: Expr[Deferral],
                                               b: Expr[Binds])
                                              (using Quotes): Expr[F[A]] =
    import quotes.reflect.*
    val topBody: Term = blockBody[F, A](block).asTerm
    Expr.summon[Monad[F]] match
      // a monad: the road every existing block takes, unchanged
      case Some(m) =>
        macros.DirectCompiler.pipeline[F, A](topBody, m,
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
