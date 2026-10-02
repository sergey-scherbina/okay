package okay

import scala.quoted.*
import scala.language.implicitConversions

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
  @scala.annotation.implicitNotFound("no Direct.Effect[${G}]: auto-coloring is OPT-IN per signature.\nRegister the effect once — `given Direct.Effect[${G}] with {}` — or use the explicit marks\n(.reflect / .? / !prog), which need no marker.")
  trait Effect[G[_]] extends okay.DirectEffect[G]

  extension [F[_], A](m: F[A])
    /**
     * The mark: typechecks as A so the block typechecks BEFORE the
     * macro expands; the macro rewrites every call, so this body
     * only runs when a mark escapes outside a direct block — and
     * then it fails loudly rather than compiling to nothing.
     * The word is the spelling that works in every scope; `.?` below
     * is the glyph, and prefix `!prog` the gesture for rows.
     */
    def reflect: A = throw new IllegalStateException(
      "Direct.reflect outside a direct block — wrap the code in direct[F] { ... }")

    /**
     * THE GLYPH: `val x = m.?` inside a `direct` block binds the
     * program, exactly as `.reflect` does — and the meaning is the one
     * `.?` already has on `A throws E`: give me the value, the context
     * deals with what was around it.
     *
     * The history is three strikes and a return (specs/unwrap-glyph.md).
     * A postfix `.!` shadowed the object `!` for every file importing
     * Direct.*, and went. `.?` was retired while two other things
     * answered it on a program — `Throws.?`, which through the `into`
     * conversion answered it on ANY value and did nothing, and the row
     * peek — and `.!?`, the one symbol that collided with nothing,
     * stood in for it. Both collisions are gone (the Throws glyphs live
     * in their type's companion, the peek is `peek`), so the glyph came
     * back, and `.!?` retired for good (mark-glyph-only, 2026-09-25):
     * two postfix symbols for one mark was a question every reader had
     * to ask and nobody needed answered.
     *
     * Outside a block it throws like every other mark, and the
     * message says where it belongs.
     */
    def ? : A = throw new IllegalStateException(
      "Direct.? outside a direct block — wrap the code in direct[F] { ... }")

    /**
     * The one-glyph mark for the rows, PREFIX: `!prog` — a program
     * of type `A ! F` collapses under its own type's symbol, and reads
     * as "perform" where `.reflect` is a word. A POSTFIX `.!` was
     * tried and refuted the same hour: the method name `!` shadows the object `!` for
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
  // (the name, every scope), .? (the postfix glyph — the one `?`
  // means on `A throws E` too), and prefix !prog (unary_!, the
  // one-glyph gesture for rows). An
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
   * `.?` are NOT substitutes: they are marks ("bind this program"),
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
   * by the macro exactly as for .? */
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
      ${ okay.macros.DirectMacros.directImpl[F, A]('block, 'M, 'd, 'b) }

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
    def ? : Unit = g.reflect
    def unary_! : Unit = g.reflect

  /**
   * A GENERATOR block (specs/generators.md): the row is `Gen[W]`'s —
   * `Writer % W + Stop` — so `Gen.emit(w).?`, a bare `Writer(w)`
   * statement and `Gen.stop.?` are its words, `while`/`if`/recursion
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
    ${ okay.macros.DirectMacros.stagedImpl[Sig, R, A]('st, 'block, 'b) }
