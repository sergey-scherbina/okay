package okay

import scala.annotation.tailrec

/**
 * The indexed freer monad: ONE data type under the whole library
 * (specs/freer-base.md).
 *
 * `Freer[G, A, S, R]` means `(A => S) => R` — a computation of `A`
 * which, given a continuation into `S`, answers `R`. The two indexes
 * are Atkey's parameterised-monad indexes and read two ways from the
 * same pair, depending on the leaf `G`:
 *
 *  - `Cont` (Cont.scala) is `Freer[Shift, …]`, whose leaf is a
 *    FUNCTION of the continuation, so the indexes are the answer type
 *    and its modification (Danvy–Filinski) — which is what `PState`
 *    and `Loop` already use;
 *  - `Free` (Free.scala) is `Freer[Lift[F], …]`, whose leaf is an
 *    OPERATION whose answer the handler chooses later, so the indexes
 *    are free to carry a protocol state instead.
 *
 * One sentence holds it: Free is Cont whose shift body is chosen by
 * the handler, not by the program. `Op` is a suspended shift.
 *
 * Nothing here knows what a leaf is. `resume` is the rotation that
 * used to be copied FIVE times (`Cont./`, `Free.fold`, `runFree`,
 * `!.resume`, Async's own loop), and every edit to `Defer` used to go
 * to all five.
 */
enum Freer[G[_, _, _], A, S, R] {

  /** a finished value; the indexes meet, which is the GADT equality
   * every eliminator's `Pure` branch relies on (and the reason these
   * parameters are invariant — see specs/freer-base.md Decisions) */
  case Pure[G[_, _, _], A, R](a: A) extends Freer[G, A, R, R]

  /** one operation of the leaf signature: a shift whose body the
   * instantiation supplies — a function for `Cont`, an effect for
   * `Free` */
  case Op[G[_, _, _], A, S, R](g: G[A, S, R]) extends Freer[G, A, S, R]

  /**
   * sequencing, as data: nothing runs until an eliminator walks it.
   *
   * PUBLIC, as `Free.Bind` has always been, because it guards no
   * invariant: a hand-built `Bind(Bind(a, f), g)` is a left-nested
   * tree `resume` normalizes by the associativity law, and a
   * hand-built `Bind(Op(s), f)` on the `Cont` side is the unabsorbed
   * form — one rotation slower, and correct. The tree is for tools.
   */
  case Bind[G[_, _, _], A, B, S, T, R](a: Freer[G, A, T, R],
                                       f: A => Freer[G, B, S, T]) extends Freer[G, B, S, R]

  /**
   * a bind whose LEFT side is deferred. It guards an invariant, unlike
   * `Bind`: the thunk is forced only inside a runner's own loop, one
   * hop per iteration, which is what lets two mutually recursive
   * functions call each other in tail position without a JVM frame per
   * call (`tailcall`, `!.tailcall`) and what makes `Eff` stack-safe on
   * a left-nested bind (specs/eff-stack-safety.md). `Freer.defer` is
   * the only way to BUILD one.
   *
   * `private[okay]` rather than `private`, for the reason `Free.Defer`
   * was public: the runners live across files (`Shift.run` in
   * Cont.scala, and in stage 1 `runFree`, `!.resume`, Async's loop),
   * and each of them may inline the rotation for speed — measured
   * necessary, see specs/freer-base.md Results. Outside the library it
   * stays invisible.
   */
  private[okay] case Defer[G[_, _, _], A, B, S, T, R](thunk: () => Freer[G, A, T, R],
                                                      f: A => Freer[G, B, S, T]) extends Freer[G, B, S, R]

  /**
   * THE rotation, and the only one: normalizes to a head form —
   * `Pure(a)`, `Op(g)`, or `Bind(Op(g), k)` — in constant stack.
   *
   * Sound by the monad associativity law, and linear-time amortized
   * for programs built by `foldLeft`. It also answers the "reflection
   * without remorse" concern (van der Ploeg–Kiselyov 2014): stepping a
   * program one operation at a time measures within ~8% of running it
   * in bulk here (HandlerBenchmark), so the type-aligned queue of that
   * paper is not needed.
   *
   * It composes continuations as `Bind` and NEVER fuses. That is a
   * measured decision, not an oversight: a fused chain is n nested
   * closure calls per run, and `statePara` — the one lane whose
   * segment reached `Cont`'s old depth budget of 128 — ran 12% FASTER
   * once the budget became one step (history.tsv `fuse1-statePara`,
   * 2026-09-15). Absorption belongs to the leaf, once, at
   * construction; see `Shift.bind`.
   *
   * THE INVARIANT IT ESTABLISHES, and why every match over it is
   * written `(x.resume: @unchecked) match`: by construction the result
   * is one of exactly those three shapes, because the cases below
   * normalize the other two away. The TYPE cannot say so — it is still
   * `Freer[G, A, S, R]`, whose cases include the ones that cannot
   * occur — so a correct three-case match reads as inexhaustive and
   * did so at forty-two sites, enough to bury every warning worth
   * reading. A three-case view ADT would let the compiler check it, at
   * one allocation per step on the hottest path in the library;
   * explicit impossible branches would cost one more type test per
   * step. `@unchecked` costs nothing and marks exactly the claim being
   * made, at the place it is made — so that is what is used, and this
   * is the one place that says what the claim is.
   */
  @tailrec final def resume: Freer[G, A, S, R] = this match
    case Bind(Bind(a, f), g) => Bind(a, x => Bind(f(x), g)).resume
    case Bind(Pure(a), f) => f(a).resume
    // the deferred left side is forced HERE, in the loop, and its own
    // binds then rotate through the cases above — constant stack
    case Defer(t, f) => Bind(t(), f).resume
    case Bind(Defer(t, f), g) => Defer(t, x => Bind(f(x), g)).resume
    case a => a

}

object Freer {

  /**
   * a bind whose left side is a thunk the eliminator's own loop
   * forces. The smart constructor is public and the case private, so
   * that the representation stays the runner's; not `inline`, for the
   * ComonadHandler reason — an inline body reaching a private
   * constructor makes the compiler synthesize an accessor with an
   * unstable name, and a downstream JAR breaks on recompilation.
   */
  def defer[G[_, _, _], A, B, S, T, R](thunk: () => Freer[G, A, T, R])
                                      (f: A => Freer[G, B, S, T]): Freer[G, B, S, R] =
    Defer(thunk, f)

}
