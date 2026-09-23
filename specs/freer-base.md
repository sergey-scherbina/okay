# Freer base — one indexed freer monad under Cont and Free

## Overview

`Cont` (Cont.scala) and `Free` (Free.scala) are the same data type
with one case renamed. Both are `Pure | leaf | Bind | Defer`, both
rebalance left-nested binds by the same tail-recursive rotation, both
force `Defer` in that loop. The leaf differs: `Cont` has
`Shift(f: (A => S) => R)`, a function of the continuation that
carries the answer types; `Free` has `Inject(e: F[A])`, an operation
whose answer is chosen later by the handler. Everything else is
copied — the rotation exists FIVE times today (`Cont./`, `Free.fold`,
`runFree`, `!.resume`, and Async.scala's own loop at lines 210–231),
and every edit to `Defer` went to all five.

This spec extracts the shared part as one enum, `Freer`, indexed by
Atkey's parameterised-monad indexes, and makes `Cont` and `Free`
two instantiations of it. The indexes read two ways from the same
pair of parameters: for `Cont` they are the answer type
(Danvy–Filinski answer-type modification, which `PState` and `Loop`
already use); for `Free` they are a protocol state (typestate), which
nothing uses yet and stage 2 opens. One sentence carries the design:
**Free is Cont whose shift body is chosen by the handler, not by the
program.**

Three measurement lanes on 2026-09-15 (fuse-bench f4da381e,
fuse-depth e12b36fd, fuse-consumers 1c236e0a; rows `fuse0-*` and
`fuse1-*` in src/jmh/history.tsv, paragraphs in
specs/interpreter-optimization.md Results) settled the one thing that
resisted a shared `flatMap`, Cont's closure fusion:

- fusion pays: `fuse=0` is 1.12–1.23x slower on every Fib lane;
- ONE step is the whole win: `fuse=1` equals `fuse=128` on fib10/50/
  100/1000 and on both `Monadic.reflect` lanes, inside ±1–3% bars;
- the 128 budget COSTS on the one shape that reaches it: `statePara`
  (PState, 1 000 left-nested ops) is 12% faster at `fuse=1`.

Why, in one line: `Op(s).flatMap(g).flatMap(h)` without fusion is
`Bind(Bind(Op(s), g), h)` and the runner pays a rotation (a Bind and a
closure) per element; absorbing `g` into the leaf makes the second
bind `Bind(Op(s'), h)`, already the head-normal form, and the rotation
never happens. Deeper absorption only nests closure calls on the
run-time stack. Only a function leaf can absorb a continuation; a data
leaf `F[A]` cannot — which is also why `Free` has no fusion and the
only alternative for it, the type-aligned queue, was measured
unneeded (HandlerBenchmark: stepping within 8% of bulk).

## Interface

```scala
/** (A => S) => R: a computation of A that, given a continuation into
 *  S, answers R. Read as a transition from index R to index S. */
enum Freer[G[_, _, _], A, S, R]:
  case Pure[G[_,_,_], A, R](a: A)                                                            extends Freer[G, A, R, R]
  case Op[G[_,_,_], A, S, R](g: G[A, S, R])                                                   extends Freer[G, A, S, R]
  /** public, as Free's is today: the tree is for tools, and an
   *  interpreter anywhere matches `Bind(Op(e), k)` after `resume` */
  case Bind[G[_,_,_], A, B, S, T, R](a: Freer[G, A, T, R], f: A => Freer[G, B, S, T])          extends Freer[G, B, S, R]
  /** the runner's alone: `resume` forces it before anyone sees the tree */
  case Defer[G[_,_,_], A, B, S, T, R](thunk: () => Freer[G, A, T, R], f: A => Freer[G, B, S, T]) extends Freer[G, B, S, R]

  /** ONE rotation, leaf-agnostic: normalizes to Pure | Op | Bind(Op, k).
   *  Composes continuations as Bind directly — the runner must never
   *  fuse (see Decisions), so this needs no flatMap of any kind. */
  @tailrec final def resume: Freer[G, A, S, R] = this match
    case Bind(Bind(a, f), g)  => Bind(a, x => Bind(f(x), g)).resume
    case Bind(Pure(a), f)     => f(a).resume
    case Defer(t, f)          => Bind(t(), f).resume
    case Bind(Defer(t, f), g) => Defer(t, x => Bind(f(x), g)).resume
    case a                    => a
```

`Freer` has NO `flatMap`/`map` members. Each instantiation owns its
own, as extensions in the leaf type's companion — that companion is
in the implicit scope of `Freer[Leaf, A, S, R]`, so `c.flatMap(f)`
resolves by the receiver's leaf with nothing imported, and there is
no member to shadow it:

```scala
/** the Cont leaf: a function of the continuation, answer types in its type */
opaque type Shift[A, S, R] = (A => S) => R
object Shift:
  /** a shift that has absorbed exactly one continuation — the fusion
   *  bit is the class, there is no depth field */
  private final class Absorbed[A, B, S, T, R](s: (A => T) => R, g: A => Cont[B, S, T])
      extends ((B => S) => R):
    def apply(k: B => S): R = s(g(_) / k)

  extension [A, S, R](c: Cont[A, S, R])
    def flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R] = c match
      case Op(s: Absorbed[?, ?, ?, ?, ?]) => Bind(c, f)          // already absorbed one: a node
      case Op(s)                         => Op(Absorbed(s, f))   // absorb the first
      case _                             => Bind(c, f)
    def map[B](f: A => B): Cont[B, S, R] = flatMap(a => Pure(f(a)))
    @tailrec infix def /(k: A => S): R = c.resume match
      case Pure(a)           => k(a)
      case Op(s)             => s(k)
      case Bind(Op(s), f)    => s(f(_) / k)

type Cont[A, S, R] = Freer[Shift, A, S, R]
object Cont:                       // the factories the 211 `Cont.Pure` sites and `Cont.defer` need
  inline def Pure[A, R](a: A): Cont[A, R, R] = Freer.Pure(a)
  inline def Shift[A, S, R](f: (A => S) => R): Cont[A, S, R] = Freer.Op(f)
  def defer[A, B, S, T, R](t: () => Cont[A, T, R])(f: A => Cont[B, S, T]): Cont[B, S, R] = Freer.Defer(t, f)

/** the Free leaf: an operation of a UNARY signature; the index rides on the node */
opaque type Lift[F[+_]] = [A, S, R] =>> F[A]
object Lift:
  extension [F[+_], A, S, R](p: Freer[Lift[F], A, S, R])
    def flatMap[B, S2](f: A => Freer[Lift[F], B, S2, S]): Freer[Lift[F], B, S2, R] = Bind(p, f)
    def map[B](f: A => B): Freer[Lift[F], B, S, R] = Bind(p, a => Pure(f(a)))

/** stage 1: a closed diagonal program, the phantom index pinned */
infix type ![A, F[+_]] = Freer[Lift[F], A, Unit, Unit]
object !:
  export Freer.{Pure, Bind}
  val Effect = Freer.Op                                   // the extractor, `case Bind(Effect(e), k)` unchanged
  def defer[F[+_], A, B](t: () => A ! F)(f: A => B ! F): B ! F = Freer.Defer(t, f)
  inline def effect[F[+_], A](e: F[A]): A ! F = Freer.Op(e)   // the factory, index pinned at Unit
  // resume, next, ?, tailcall, widen, translate, relay, interpret, tracing: as today, over Freer.resume

/** stage 2 (2026-09-23): an INDEXED program — the same Free tree
 *  behind an opaque facade with two phantom indexes, `S` before and
 *  `R` after. Ordinary programs enter DIAGONALLY (`diag`, at any
 *  index, moving nothing); only a smart constructor may claim a move
 *  (`Prog.transition`), and the module that writes one keeps it
 *  private. Never matched, so the existential leak that refuted the
 *  indexed enum (stage 1) cannot reach it. */
opaque type Prog[F[+_], A, S, R] = Free[F, A]
object Prog:
  inline def diag[S, F[+_], A](p: A ! F): Prog[F, A, S, S]          // any program, index unmoved
  inline def pure[F[+_], A, S](a: A): Prog[F, A, S, S]
  inline def effect[F[+_], A, S](e: F[A]): Prog[F, A, S, S]
  inline def transition[S, R, F[+_], A](p: A ! F): Prog[F, A, S, R]  // THE claim; a module's private tool
  extension [F[+_], A, S, R](m: Prog[F, A, S, R])
    inline def flatMap[B, T](f: A => Prog[F, B, R, T]): Prog[F, B, S, T]
    inline def map[B](f: A => B): Prog[F, B, S, R]
  extension [F[+_], A, S](m: Prog[F, A, S, S])
    inline def free: A ! F                                            // unlift — DIAGONAL only

/** the Delim half: the prompt stack in the index, as a lexical given
 *  (the probe's shape, 4af08745, over the real machine) */
object Delim.Stacked:   // spelled `Delim.Stacked` in Delim.scala
  final class Stack[S0 <: Tuple] { type S = S0 }
  final class In[R, S <: Tuple](val p: Prompt[R]) { given stack: Stack[p.type *: S] }
  sealed trait Has[S <: Tuple, P]        // "p is on the stack" — the compile error's home
  type Under[F[+_], A, S <: Tuple] = Prog[Delim + F, A, S, S]
  def delimited[R, F[+_]](body: (s: In[R, EmptyTuple]) => Under[F, R, s.p.type *: EmptyTuple])
                         (using OneMachine[F], At): R ! F              // the root: installs AND runs
  def reset[R, F[+_]](using st: Stack[?])(body: (s: In[R, st.S]) => Under[F, R, s.p.type *: st.S])
                     (using At): Under[F, R, st.S]                     // nested: installs only
  def shift[R, A, F[+_]](p: Prompt[R])(using st: Stack[?], ev: Has[st.S, p.type])
                        (f: (A => Under[F, R, st.S]) => Under[F, R, st.S])(using At): Under[F, A, st.S]
  // shift0 / control / control0 / abort: the same signatures over their Delim twins
```

Unchanged by this spec: `Control[M]` and its `Cont`/`Func` instances,
`/>`, `^`, `Loop`, `answer`, `tailcall`; `Effects[M]` with its three
instances — `Eff` is a function into `Cont` and `Eager` is a union
`A | (A ! F)` over the alias, neither touches the tree; `Handler`,
`TypeableK`, `<|>`, `split`, the row algebra `+`/`%`; every handler
signature in the library (they take `A ! F`, which is still one alias).

## Behavior

Stage 0 — the enum, and `Cont` on it:
- [ ] `Freer` compiles on JVM, JS and Native with exactly the four
      cases above and no `flatMap` member; `resume` is the only
      rotation in the file.
- [ ] `Cont` is the alias; Cont.scala keeps `Shift`, `Absorbed`, `/`,
      the `Cont` factories and `Control[Cont]` — nothing else. Every
      `Cont.Pure`/`Cont.Shift`/`Cont.defer` call site compiles
      unchanged (211 / 2 / n today).
- [ ] `TestCont`'s fusion-budget spill stress is REWRITTEN for one-step
      absorption: a left-nested chain of 1 000 000 binds runs on the
      default stack, a right-nested one too, and a chain of 1 000
      shifts each followed by two binds runs — the second bind of each
      must land in a `Bind` node, asserted by an `Absorbed`-count probe
      or by the B/op of the lane, not by reading a depth.
- [ ] `PState`, `Loop`, `Monadic.reflect/reify`, `Delim` (its handlers
      are `F !> S`, Cont-valued) and the `Eff` instance are green
      without source changes beyond imports.
- [ ] MEASURED, same session, alternating, per-lane minimum of 3:
      fib10/50/100/1000 and `statePara` at or below the `fuse1-*` rows
      (175.9 / 937.0 / 1888.1 / 27017.1 ns, 27.8 µs); `cont24` and
      `stateEffect` unchanged; `okayDirect`/`okayDirectRec` unchanged.
      Prediction: shift-heavy lanes gain up to 17% in B/op (40 vs 48 B
      per shift: `Op` 16 + `Absorbed` 24 against `Shift` 24 + lambda
      24) and time inside bars; `statePara` keeps its 12%.

Stage 1 — `Free` on it:
- [ ] `A ! F` is the alias at `Unit`; Free.scala keeps `Lift`, `fold`,
      `run` (both), and `Free.defer`. `object !` exports the cases and
      binds `Effect` to `Op`; the 89 `(x.resume: @unchecked) match`
      sites and the 20 files outside Cont/Free/Effects that match
      `Bind`/`Defer`/`Effect` compile UNCHANGED — the count of touched
      call sites is a result to record, and more than the imports is a
      finding against the design.
- [ ] `runFree`, `!.resume`, `!.next`, `!.?`, `relay`, `translate`, the
      Delim machine, `Pipe`, `Stm`, `Sim`, `Chunks`, `State.handle`,
      `Writer.fold` run over `Freer.resume` or match the cases; Async's
      own loop (the fifth rotation, the only place outside the runners
      that CONSTRUCTS `Bind` and matches `Defer`) is deleted in favour
      of `resume`. Nothing outside Freer.scala names `Defer` afterwards
      — grep is the check. `Free.fold` and `runFree` may keep their own
      inlined rotation ONLY if the law below says they must, and then
      they are the two places that see `Defer`.
- [ ] LAW (new test, all three platforms): for every encoding and every
      bind-tree shape TestLowering already enumerates, eliminating
      after `resume` equals eliminating with the eliminator's own
      inlined rotation, on the answer AND the effect trace. This is
      what lets an eliminator inline the four lines for speed without
      a second source of truth.
- [ ] MEASURED: FusionBenchmark `fusedSWr` at or below 13.7 µs /
      122 641 B/op; CompareBenchmark `okayFree`/`okayCont`;
      HandlerBenchmark `relayForward`/`handleForward`/`stepBulk`/
      `stepOneByOne`; WidenBenchmark and PerElementStepBenchmark
      unchanged within bars. Any lane over its bar names the eliminator
      that must inline the rotation (see Decisions).
- [ ] The `Monad[Free[F, *]]`, `Effects[Free]` and `Effects[Eff]`
      instances, `fromFree`, `toEff`, `convert`, `reify`, `reflect`
      and TestReflect's round trip are green.
- [ ] `TestFree`-level laziness contract holds unchanged: `def forever
      = pure(()).flatMap(_ => forever)` does not diverge at
      construction (no Pure-fusion in `Lift.flatMap`, ever).

Stage 2 — the index as typestate, Delim first (lane freer-base-stage2, 2026-09-23):
- [ ] `Prog[F, A, S, R]` exists as an opaque facade over `Free[F, A]`
      with `diag`/`pure`/`effect` at the diagonal, `flatMap` composing
      indexes end to end, `map` keeping them, `free` unlifting a
      DIAGONAL program only; `A ! F` is untouched (see Decisions: the
      diagonal is a conversion, not a definition).
- [ ] Delim carries its prompt stack in the index: `Delim.Stacked.
      reset` installs `s.p.type` on the stack for its body, `shift(p)`
      requires `Has[stack, p.type]`, and the three `NoPrompt` shapes
      the probe named — a shift with NO reset, a shift to a FOREIGN
      prompt of the same answer type, and a prompt that ESCAPES its
      reset into a `var` and is shifted to afterwards — are
      `compileErrors` in TestProg, with the message naming the stack.
- [ ] The five positive shapes of the probe run through the REAL
      machine and answer the shift/reset laws' values: bare, a
      for-comprehension, an ordinary effect in head position, nesting
      with a shift to the OUTER prompt, a reset as a step of a larger
      program. TestDelim is untouched (the facade is additive).
- [ ] The spec's own first caveat is a test: a `Throws` abort inside a
      block promising a transition drops the continuation and the
      transition does NOT happen — asserted, so nobody reads the type
      as a run-time guarantee.
- [ ] One module protocol typed: okay-sql's transaction as a `Prog`
      over any `Sql` (`Tx.begin: Idle -> Open`, `commit`/`rollback`:
      `Open -> Idle`, `query`/`update`/`batch` at any index, `Tx.run`
      accepting only `Idle -> Idle`) — the nested `begin` that
      `PgSql.begin` refuses with an `IllegalStateException`, a `commit`
      with no `begin`, and a program that ends inside a transaction
      are `compileErrors`; a well-bracketed program runs against a
      recording fake `Sql` in the order the type promised.
- [ ] Zero bytes and zero time: the facade is an opaque alias and
      every constructor is `inline`, so a `Prog` program IS the `Free`
      tree it wraps — asserted structurally (`free` is identity; the
      same nodes, `eq`) rather than benchmarked, since no node changes.

## Out of scope

- **A generic `flatMap` over `G`** — no code is polymorphic in the
  leaf; `Effects[M]` abstracts over the encoding, not the leaf.
- **`Delim.Segs` as a `Freer`** — the machine's continuation stack is
  the Bind spine turned inside out (a zipper: `K`/`Mark`/`Done`); it
  could be a third instantiation, but the machine reifies segments back
  into programs anyway, and nothing measured asks for it.
- **Three-ary effect signatures** (`Tx[A, S, R]` as a GADT the compiler
  checks) — that needs a three-ary row algebra beside the unary one.
  Stage 2's transitions are sealed by smart constructors, the Delim
  discipline; the GADT tier is a later spec if the soft one leaks.
- **`Eff` and `Eager`** — untouched by construction (see Interface).
- **Variance on the indexes** — refused, see Decisions.
- **Lowering `Cont.Fuse`** as its own lane — BACKLOG `cont-fuse-one-
  step` is absorbed by stage 0: with `Absorbed` there is no budget.

## Decisions

- **Stage 2 is an ADDITIVE facade, and `A ! F` is not redefined**
  (2026-09-23). The interface once read "`A ! F` is `Prog[F, A, Unit,
  Unit]`"; with `Prog` an opaque alias that cannot be — one type
  cannot be both the alias and its own facade — and it need not be:
  `Prog.diag` is an inline identity, so any program enters at any
  index for nothing, and `free` leaves at the diagonal. The
  PState/Delim policy (operator, 2026-09-01: additive where apt,
  primary only where necessary) decides the rest: `Delim.reset`,
  `shift` and their callers in okay-ui/okay-agent/okay-llm keep their
  spelling; `Delim.Stacked` is the typed door beside them.
- **The stack is a lexical GIVEN with a type member, exactly the
  probe's shape** (4af08745) — not an inferred parameter (a
  for-comprehension head has no expected type), not a curried
  dependent context function (refused by the compiler), not a stack
  carried through a non-curried one (crashes dotty). The escape case
  falls out of the same design with no region system: after a
  `reset` returns, the stack in force is the OUTER given, which has
  no `p.type` in it, so a shift to the leaked prompt has no `Has`.
  specs/delim-safety.md's region tag (`[S] => Prompted[R, S] ?=>`)
  was the other road; it needs `S` in the program's type for the
  tag to bind, and therefore on every `Delim` signature and the four
  inline doors — the given stack costs one `import s.given` per
  reset instead.
- **`transition` is public and named as the claim it is.** A module
  typing its protocol must make its own transitions, so the tool
  cannot be `private[okay]`; the discipline is the module's — it
  calls `transition` inside its private smart constructors and
  exposes only those, which is how `Tx` in okay-sql is written. A
  `transition` at a call site is the `asInstanceOf` of this design and
  reviewed as one.
- **`free` unlifts the diagonal only.** A `Prog[F, A, S, R]` with `S
  != R` is a program that promises a move; letting it out as a plain
  `A ! F` would run the move without the bracket that closes it
  (`begin` without `commit`). A module's runner takes the closed shape
  (`Tx.run: Idle -> Idle`) and unlifts inside.
- **Enum with a closed `Op(g: G[A, S, R])`, not a sealed trait with
  an open leaf.** Chosen for exhaustiveness: every match in the
  library is checked. The wrapper's cost was the objection — `Op(g)`
  is one object more than a leaf that IS the node — and it is answered
  per instantiation: for `Free`, `Op(e)` is byte for byte today's
  `Inject(e)`; for `Cont`, `Op(f)` holds the raw function, and the
  fusion state is the function's CLASS (`Absorbed`), so a shift is 40 B
  against today's 48. Rejected: `type Shift = ((A => S) => R, Int)`
  (the operator's first proposal) — a `Tuple2` beside `Op` is 64 B per
  shift and a boxed `Integer` at the budget; and a depth field on the
  generic `Op` — 8 B on every Free operation for a number that is 0
  or 1.
- **One-step absorption, and the runner never fuses.** fuse-depth and
  fuse-consumers: one step is the whole win, the budget costs on the
  shape it was built for. `resume` composes continuations as `Bind`
  because a rotation that fused would re-nest closure calls, which is
  exactly what the 128 budget was found doing. Predicted, to be
  refuted by stage 0's numbers: rotation losing the chance to absorb
  inside `f(_).flatMap(g)` is worth nothing measurable — `Bind(Op(s'),
  g)` and `Op(Absorbed(s', g))` are the same bytes and both head-
  normal.
- **`flatMap` lives in the leaf's companion, not in a `Sig[G]`
  typeclass.** Same effect, no plumbing: the implicit scope of
  `Freer[Shift, …]` includes `Shift`'s companion, and there is no
  member to shadow the extension. The typeclass is the fallback if
  extension resolution through an opaque leaf misbehaves — the one
  known trap is the repository's own: same-name extensions in
  DIFFERENT files of one package are not overloads, so `Shift.flatMap`
  and `Lift.flatMap` MUST sit in companions, never at package level.
- **Eager right-nesting at construction — REFUTED by construction, do
  not retry.** `Bind(x, g).flatMap(h) = Bind(x, a => g(a).flatMap(h))`
  would spare the runner every rotation, for `Free` too. It nests the
  EARLIER continuation inside the later one, so invoking the composed
  closure calls `g`, which calls its own inner `g₀`, n deep on a
  foldLeft chain — the eff-stack-safety overflow in a new coat. Lazy
  rotation composes the other way round (`f(_).flatMap(g)`: `f` runs,
  returns a node, the runner calls the next) and stays flat. A thunk
  would fix it at +32 B per bind, which is the price already refused
  for Eff's fast path.
- **No Pure-fusion in `Lift.flatMap`.** Unchanged from
  interpreter-optimization.md: `pure(()).flatMap(_ => forever)` must
  not diverge at construction; cats-effect and ZIO give the same
  contract. `Shift.flatMap` on a `Pure` receiver builds `Bind` for the
  same reason.
- **`Free` at a pinned `Unit` index in stage 1; polymorphic factories
  only in stage 2.** A diagonal program at a fixed index is not
  state-agnostic (the enum is invariant), so "indexes everywhere" means
  two phantom type parameters on every handler — ~539 mentions of the
  program type in the 16k-line core, 17 files in the handler layer.
  Stage 1 therefore pins `Unit` at every Free factory and changes no
  signature — REFUTED 2026-09-15, see Results: pinning the factories
  does not pin a pattern match, because `Bind` carries the left side's
  index and a match makes it existential; stage 2 adds `Prog` and the diagonal `effect[F, A, R]`
  beside it, one effect at a time. Type-inference risk is confined to
  stage 2 and has a precedent: `PState.get[S, R]`/`set[S, S2, R]` are
  exactly this shape and infer through for-comprehensions with no
  annotations.
- **Variance is out.** `(A => S) => R` would be `[+A, -S, +R]`, and a
  state-agnostic program could then upcast into any index. But `Cont`'s
  runner types `k(a): R` from the GADT equality `S = R` that matching
  `Pure` provides; with variance that equality is gone and the line
  becomes a cast. No casts without necessity (operator, 2026-09-02).
- **`resume` is the single rotation; an eliminator may inline the
  four lines only under the law.** Free's `runFree` was measured
  within 8% of stepping through `resume` (HandlerBenchmark
  stepOneByOne vs stepBulk); 8% is over the bars on some lanes. The
  law in stage 1 makes an inlined copy a verified optimisation rather
  than a second definition — the JIT will likely inline the small
  final `resume` anyway, and the numbers decide.
- **Index as typestate is a protocol of the TEXT.** An aborting
  handler (`Throws`) drops the continuation and a promised transition
  never runs; a multi-shot handler (`Choice`) runs it twice. Haskell's
  indexed monads do not carry this caveat because they have no
  handlers that capture the continuation; this library does. So the
  index complements the finalizer discipline of `Resource.run`, never
  replaces it — and stage 2 asserts it in a test.
- **Every case public; only `Shift.Absorbed`/`Shift.Mapped` private.**
  Settled with the operator on 2026-09-15 across three questions: must
  `Bind` and `Defer` be public, does a public `Bind` cost anything,
  and — the one that finished it — what does hiding `Defer` buy when
  `Freer.defer` is a public factory that builds exactly one? Nothing:
  hiding a case whose constructor is public does not stop anyone
  MAKING the node, only MATCHING it, and matching is the half a
  stepper, a rewriter or an outside interpreter needs. So the rule
  came out simpler than it went in — privacy guards invariants, and
  the only invariant a case could guard here is "a leaf absorbs at
  most once", which lives in the leaf's own classes inside `Shift`'s
  companion, not in the tree. `Freer.defer` stays the constructor
  because the thunk shape reads better at a call site than
  `Defer(() => …, f)`, not because it is the only way in.
  `Bind` guards nothing either: a hand-built `Bind(Bind(a, f), g)` is a
  left-nested tree the rotation normalizes by the associativity law,
  and a hand-built `Bind(Op(s), f)` is the unabsorbed form — one
  rotation slower, correct. Public is also what the library says of
  itself ("the tree is for tools": step it, inspect it, relay it —
  Kiselyov's freer exposes `Impure(op, k)` for the same reason), what
  `Free` already is in v0.1.1, and what an interpreter outside package
  `okay` needs, which `okay-cats`/`okay-kyo` would be if they were
  third-party. `Cont`'s old reason for a private `Bind` — "the
  representation stays the runner's" — is answered by `resume` being
  the public normal form. Recorded and refused: `private[okay]`
  (hides the node from library users, keeps the 89 in-repo match sites
  in 15 packages compiling — but buys only the freedom to add a case,
  which breaks the `@unchecked` matchers in-repo just the same, and
  needs a `val` alias in `object !` because an export would go public);
  and a fully private `Bind` behind a public trait `Next` matched by
  type-test patterns with bound type variables (Delim's `case n:
  Next[F, a, R]` idiom, allocation-free, compiler-checked, but the 89
  sites become `n.op`/`n.k`). A case-class view returned by `resume`
  was refused earlier for one allocation per step (Effects.scala's
  `resume` comment). The normal form after `resume` — `Pure | Op |
  Bind(Op, k)` — is documented ONCE, on `resume`.
- **Names.** `Freer` after Kiselyov–Ishii; `Op` for the leaf (not
  `Inject`, which named the effect side only); `Lift` for the unary
  signature's leaf; `Shift.Absorbed` for the fused function, nested
  because `okay.Fused` (handler-fusion loops) and `okay.Fuse` (optics)
  are taken; the `!` alias and `Effect` extractor keep their names so
  the 89 match sites do not move.

## Results

### Stage 0 — implemented, green, and at parity or better

**The state to judge it by** (branch tip, four alternating rounds on a
quiet box, every core JMH lane, `-prof gc`, history.tsv `once-*`):

| lane | time | B/op vs master |
|---|---|---|
| `statePara` | **0.860** | −40 132 |
| `fib10` | **0.884** | −96 |
| `handleForward` | 0.974 | −158 400 |
| `stepBulk` | 0.974 | −80 016 |
| `stepOneByOne` | 0.984 | −80 016 |
| `relayPrebuilt` | 0.986 | identical |
| `effCont24` | 0.989 | identical |
| `effInline24`, `func24`, `fib1000`, `relayForward`, `stateEffect` | 0.99–1.00 | identical or lower |
| `buildOnly`, `fib100`, `cont24` | 1.00 | identical or lower |
| `effFunc24` | 1.015 | identical |
| `fib50` | 1.024 | −416 |

Nothing is more than 2.4% slower, eight lanes are faster, and
allocation is at or below master on every lane. Three things got it
there, in this order, and each is written up below: `map` reaching the
absorbing path, `relay`'s loop getting back under the JIT's inlining
threshold (landed separately on master, 25102517), and `Once` becoming
an enum.

### Stage 0 — how it read before those three

The code exists on `feature/freer-base-stage0`: `Freer.scala` (the enum
and `resume`), `Cont.scala` rewritten as the alias plus the `Shift`
leaf, `TestFreer.scala` (the law), `TestCont` rewritten for absorption.
**The full matrix is GREEN on the branch**: `scripts/gate.sh` reports
4 419 test results across JVM, JS and Native, 182 module compiles, no
failures and no warnings. Two call sites outside the base changed,
both named below. It is NOT merged, because it does not pass the
PERFORMANCE gate this spec set for it: three Fib lanes and
`relayForward` are 3.6–8.2% slower.

**The final A/B**, master against stage 0, FOUR alternating rounds in
one session on a quiet box, per-lane minimum, `-prof gc`, every core
JMH lane (history.tsv `freer0-*`):

| lane | time | B/op vs master |
|---|---|---|
| `statePara` | **0.851** | −40 096 |
| `fib10` | **0.903** | −96 |
| `stepOneByOne` | 0.954 | −80 016 |
| `effCont24` | 0.979 | identical |
| `handleForward` | 0.985 | −158 400 |
| `stateEffect`, `effInline24`, `func24` | 0.99 | identical |
| `effFunc24`, `cont24` | 1.00–1.01 | identical |
| `stepBulk` | 1.022 | −80 016 |
| `fib100` | 1.029 | −816 |
| `fib1000` | 1.040 | −8 015 |
| `fib50` | 1.053 | −416 |
| `relayForward` | **1.103** | **identical** |

Allocation is at or below master on every lane. Eight lanes are
faster, five are slower, and the slower ones are led by
`relayForward`, whose bytes match master to the digit.

### The second round of speed work, and what it closed

The operator asked for the residual to be chased. Three more
candidates were named and each was measured; all three are REFUTED,
and the exercise is recorded because the refutations are the durable
part.

- **"The `Op` wrapper costs a pointer chase per leaf touch."** NO — it
  is the opposite. A per-construct benchmark (`LeafBenchmark`, one
  lane per node kind) reads `leafOnly` **0.972**, `leafAbsorbed`
  0.970, `leafSpilled` 0.971, each allocating 8–16 B LESS per leaf.
  The leaf is the part that got faster.
- **"One-step absorption is too shallow; the old budget was buying
  something."** NO, and the sweep is worth keeping: depth 1 / 4 / 16 /
  128 against master, three rounds. The Fib lanes do not care (1.047
  at depth 1, 1.043 at depth 128, allocation identical at every
  depth); `statePara` cares enormously and in ONE direction — 0.861 at
  depth 1 against 1.188 / 1.146 / 1.170 at 4 / 16 / 128. Depth 1 is
  settled by measurement, the constant is a literal, and the switch
  that swept it is gone.
- **"The runner should delegate to `Freer.resume` after all."**
  Indistinguishable. Asked again with the `map` defect fixed, four
  rounds: `relayForward` 1.093 against 1.094, and the Fib lanes
  disagree in DIRECTION (fib10 0.911 vs 1.246, fib100 1.039 vs 0.891)
  on a box at load 4–5 — a JIT inlining-shape lottery, not an effect.
  The interleaved loop is kept because it is the shape the old runner
  had and because it wins `statePara` (0.845 vs 0.900).

**`statePara` earned a second job out of this**: a thousand left-nested
operations, and the only lane in the suite with a sharp, monotone
response to absorption depth. Anything that accidentally deepens
absorption shows up there immediately. Price future leaf changes
against it.

**What is left is not explained.** The residual is invariant to
absorption depth, to runner shape and to allocation, and
`relayForward` makes that unmistakable: 100 of its 10 000 operations
touch `Cont` at all (the other 9 900 forward through `Free`, which
stage 0 does not change), its handler returns a `Pure`, its bytes
match master to the digit — and it is 10% slower. No structural
hypothesis available on this machine survives those four facts
together. The next instrument is a disassembling profiler, and
`-prof perfasm` needs Linux `perf`; on macOS that means `dtraceasm`
under root, which is the operator's call and not a thing to take.

### `Once` as an enum — the operator's proposal, and what closed the gap

After four structural theories had been refuted, the residual on the
Fib lanes was still 3–5% and unexplained. The operator proposed making
`Once` an enum with cases `Absorbed` and `Mapped`, keeping `Shift`
itself the raw opaque function. Measured on the branch, three rounds:
**fib10 0.968, fib100 0.968, fib1000 0.955** — and that is the whole
residual.

The mechanism, and it is worth stating precisely because the obvious
reading is wrong. `apply` written ONCE in the enum body instead of
overridden in each of two classes gives both cases the same vtable
entry, so the runner's `s(k)` has a single call TARGET and the JIT
inlines it. It is not a cure for megamorphism: in the same session,
splitting that call site so it tests `Once` first — a bimorphic site
for the absorbed forms, a megamorphic one for user lambdas — did
NOTHING, 0.996 to 1.017 across every lane. The number of receiver
types was never the problem; the number of call targets was.

Allocation is unchanged, by construction: an enum case is a case class
with the same two fields, and `Shift` stays the raw function so an
unabsorbed leaf is still `Op` plus the user's lambda. The one measured
price is synthetic: `leafMixed`, which alternates the two cases at one
site, allocates +14 B/op (225 760 against 211 792) where a single body
appears to lose escape analysis that two bodies kept. No real lane
shows it — `statePara` 1.002, every Fib lane faster.

### What was refuted on the way, with numbers

Four experiments, each its own A/B in its own session; none is worth
retrying blind.

1. **"The absorption rule costs something."** No. A three-arm run —
   master at a budget of 128, master at a budget of 1, stage 0 —
   measured master@1 allocating **byte for byte** what master@128
   allocates on all four Fib lanes (2 080 / 11 616 / 20 752 / 298 970,
   delta zero) and within 0.4% on time. The whole regression was
   structural, and this is what made that certain.
2. **"The cost is splitting rotation from elimination."** Refuted:
   copying `resume`'s four lines into the runner made it WORSE —
   fib10 1.08→1.18, fib50 1.15→1.38, fib100 1.09→1.18 against the same
   baseline in the same session.
3. **"The rotation must compose through the leaf's own `flatMap`, so
   the rotated continuation can be absorbed."** True in principle
   (`Free`'s `flatMap` IS `Bind`, so `resume`'s raw composition is
   already optimal for it, while `Cont`'s can absorb) — and worth
   nothing measurable here: B/op did not move by a single byte on any
   Fib lane. The generator's programs are right-nested, so that
   rotation case almost never fires. The interleaved loop was kept
   anyway, since it is the shape the old runner had.
4. **The actual defect, found by decomposing allocation per construct**
   (`getCurrentThreadAllocatedBytes`, the technique
   exact-bytes-beat-jmh-alloc-norm records; the JMH bars were wider
   than the effect): `s.map(f)` cost **96 B** against master's 48,
   while calling the absorbing path directly cost **40**. So `map` was
   not reaching it. Cause: `Cont` has no members any more, and a
   top-level `given` is in the package's LEXICAL scope, which beats
   `object Shift` in the receiver's implicit scope — so `c.map(f)`
   resolved to `ParaMonad.map`, whose default is `flatMap(x =>
   pure(f(x)))`, a `Pure` per element. `flatMap` never had the problem
   because both roads lead to `Shift.bind`. `ParaMonad.map` was
   `inline`, hence final, hence unoverridable: the fix is one word
   removed there plus an override in each `Control` instance, and it
   moved fib10 from 1.07 to 0.908 and `shift+map+map` from 192 B to
   136.

### Findings that outlived their experiment

- **No `apply` extension on `Cont` is possible inside package `okay`.**
  `c(k)` was a member of the old enum, where members win; as an
  extension it loses to Generate.scala's seed-side `apply` (`a(f)` for
  a `Loop` body) in lexical scope. Two call sites in `relay`
  (Effects.scala) became `g(e) / k`, which was always the meaning.
- **The eager-shift shape got BETTER.** A chain whose shift bodies
  re-enter their continuation immediately overflows the stack in both
  trees — it is direct style's own cost — but master dies at 1 000
  elements where stage 0 survives them, because the old budget nested
  128 fused closure calls where one-step absorption nests one.
- **`Cont`'s cases are matched nowhere outside Cont.scala.** Every
  `case Pure(a)` in the repository (80-odd sites, 15 packages) is
  `Free`'s. The rewrite touched two call sites in total.

### The decision this leaves

Landing is the operator's call, and the trade is: one enum and one
rotation concept instead of five copies, `statePara` 14% faster,
allocation down everywhere — against 3.6–8.2% on four lanes whose
cause is dispatch shape rather than anything this spec can name. The
next experiment, if it is wanted, is `-prof perfasm` on `relayForward`
(identical bytes, 8.2%, the cleanest signal) before any further
redesign.

### Stage 1 — REFUTED AS SPECIFIED (2026-09-15, branch `feature/freer-base-stage1`)

`Free` cannot be `Freer[Lift[F], A, Unit, Unit]` while the library's
match sites stay as they are. The obstacle is exact, and the compiler
said it rather than an argument:

`Bind[G, A, B, S, T, R](a: Freer[G, A, T, R], f: A => Freer[G, B, S, T])`
carries the LEFT side's answer index `T`. Matching a `Free[F, A]`
gives back a continuation at `Freer[Lift[F], A, Unit, T]` for an
EXISTENTIAL `T`, while all 89 `(x.resume: @unchecked) match` sites
want `A ! F`, which is `T = Unit`. They are the same value at run time
— `Lift` ignores both indexes and every factory pins them — but no
type says so.

Three ways out were tried or costed:

1. **An existential outer index**, `type Free[F, A] = Freer[Lift[F],
   A, Unit, ?]`. Fixes elimination, breaks CONSTRUCTION symmetrically:
   `Bind(a, f)` can no longer unify the inner index with `f`'s result.
2. **A pinning extractor** in `object !` — `def unapply[F, X, A](p:
   Free[F, A]): Option[(Free[F, X], X => Free[F, A])]` with the claim
   made once. REFUTED BY THE COMPILER: `X` is unconstrained by the
   scrutinee, and Scala 3 infers it as `Nothing` rather than
   skolemizing, so `case Bind(Effect(e), k)` yields `k: Nothing =>
   Free[F, A]` and the link between the operation's answer type and
   the continuation's argument is gone. This is the load-bearing
   negative result: the trick that makes GADT extractors work
   elsewhere does not apply to a type parameter that appears only in
   the RESULT of the `unapply`.
3. **A uniform-index bind case in the base** — `case Seq[G, A, B,
   R](a: Freer[G, A, R, R], f: A => Freer[G, B, R, R]) extends
   Freer[G, B, R, R]`. This WOULD work: matching a `Free[F, A]` gives
   `f: x => Free[F, A]` with the link intact, because there is no
   inner index to leak. NOT TAKEN without a decision, because of what
   it costs: `Cont` needs the non-uniform `Bind` (`PState` changes the
   answer type — that is the point of the paramonad), so the base
   would carry BOTH, and `resume` would rotate both. A tree is built
   entirely by one instantiation, so the two never mix and the cases
   cannot combine — but "one rotation instead of five" becomes "one
   method holding two rotations", which is most of what stage 1 was
   for.

**What stage 0 still delivers, and it is not nothing:** `Cont` on the
shared base, at parity or better, with the fusion budget gone. What
stage 1 was to add — `Free` on it, so the rotation exists once rather
than four times — needs either option 3 above or a different base
shape, and that is a decision, not a task.

The branch holds the attempt, WIP and not mergeable, so the next
person does not re-derive the leak.

### The turn after the refutation: indexes on facades, `Freer` deleted (landed)

The refutation said where types may NOT live: on the nodes, because a
match makes a node's index existential. The operator drew the
conclusion the other way round — keep `Free` as the base, delete
`Freer`, make `Cont` a facade — and that is what landed
(`feature/cont-on-free`, rows `cof-*`).

```scala
enum Free[F[+_], A]              // the base, unchanged: Pure | Inject | Bind | Defer
type Cont[A, S, R] = Cont.Rep[A, S, R]
object Cont:
  opaque type Rep[A, S, R] = Free[Shift, A]         // S, R phantom to the tree
private type Shift[+X] = (X => Nothing) => Any      // every (X => S) => R conforms: an upcast
```

Danvy–Filinski's answer-type modification lives ONLY in the
companion's signatures. The tree is the same `Pure | Inject | Bind |
Defer` every effect program is made of, and `Free` needs no alias, no
`Lift`, no phantom `Unit`: the 89 match sites are untouched because
nothing about `Free` changed. **Measured against master: every core
lane within ±1%, allocation identical to the byte on all of them** —
the nodes are the same objects, the cast is erased.

Two `asInstanceOf` lines, one invariant ("the facade typed it when
it built it, and nothing else can build one"): `typed`, applying a
leaf to a continuation, and `pinned`, the `Pure` branch of the runner
— the GADT equality `S = R` that `Pure[A, R] extends …[A, R, R]` used
to carry does not exist on an unindexed tree. That is the price of
ATM on a shared homogeneous tree, and it is the operator's rule
satisfied to the letter: one function each, one comment, sound by a
sealed-module claim.

The principle this settles, and it carries into stage 2: **the tree
is syntax; an index is a claim about syntax; claims live on facades;
facades are never pattern-matched.** Typestate for an effect program
is one more facade over `Free[F, A]`, and the leak that killed stage 1
cannot happen to it.

Three spellings the operator proposed were each tried by compiling,
because the session's rule is that an argument is not evidence:

- **`Free[[X] =>> (X => S) => R, A]`, the precise leaf.** Types only
  the diagonal. One `Bind` relates THREE leaf types — left
  `(·=>S)=>R`, continuation results `(·=>S2)=>S`, whole `(·=>S2)=>R` —
  and `Free.Bind[F, A, B]` has one `F`: the node's intermediate answer
  type has no home. `PState.set` changes the answer type, so the
  diagonal is not enough.
- **`(X => ?) => R`.** Fails twice: at `shift`, because a wildcard is a
  SUPERtype and a function's argument slot needs a SUBtype of every
  `A => S`; and at `bind`, for the same reason as the precise leaf.
- **`(X => ?) => ?`.** `bind` now passes — the two wildcards do unify
  the three positions — and only `shift` fails, on the variance point
  above. The one legal spelling of "unknown in the argument slot" is
  `Nothing`, of "unknown in the result slot" `Any`; so this is `Shift`
  before the variance check, and it needs the same cast at run.

**A trap worth its own line: a top-level `opaque type` is transparent
to its whole PACKAGE, not its file.** Declared at top level, `Cont`
was plainly `Free[Shift, A]` in Generate.scala, `Stream`'s
program-carrier `map` captured the for-comprehension, and — as a plain
alias — `Free`'s MEMBERS beat `Cont`'s extensions and switched
absorption off. The representation had to move inside `object Cont`.

- Stage 1: REFUTED as specified; superseded by the facade above, which
  gets the whole of its goal (one base, `Free` untouched) by the
  opposite move.
- Stage 2: the LANGUAGE question is answered, the lane is not started.
  See below.

### Stage 2, the Delim half — the identity question, answered by compiling

Stage 2's first behavior item is that `NoPrompt` — the exception
`Delim.scala:223` throws when a shift names a prompt that is not
installed — becomes a compile error. Everything in that item rests on
one question nobody had asked the compiler: a `Prompt[R]` is made at
RUN time by `reset`, so can its IDENTITY, not merely its answer type,
reach the type level?

**It can.** `scripts/stage2-prompt-identity-probe.scala` is the whole
answer in one runnable file: five positives compile and three
negatives are refused, including the case that matters most — a prompt
that ESCAPES its `reset` and is shifted to afterwards, which is
precisely today's throw.

This was asked FIRST, before any lane was claimed, because stage 1
died on exactly this class of question after the implementation was
written. The two questions are cousins and their answers are
opposite: stage 1 needed the compiler to SKOLEMIZE a free parameter in
an `unapply` and it inferred `Nothing` instead; stage 2 needs a
singleton `p.type` of a term parameter in a dependent signature, which
the language supports outright.

The shape that works, and each piece of it is scar tissue:

```scala
final class Stack[S0 <: Tuple]:      type S = S0   // the stack in force
final class In[R, S <: Tuple](val p: Prompt[R]):
  given stack: Stack[p.type *: S] = new Stack      // published, imported
def reset[R](using st: Stack[?])(
  body: (s: In[R, st.S]) => Prog[R, s.p.type *: st.S, s.p.type *: st.S]
): Prog[R, st.S, st.S]
def shift[R, A](p: Prompt[R])(using st: Stack[?], ev: Has[st.S, p.type])(
  f: (A => Prog[R, st.S, st.S]) => Prog[R, st.S, st.S]
): Prog[A, st.S, st.S]
```

**Four compiler facts were paid for to arrive at it**, each by a round
that failed, and they are why the obvious spellings are absent:

1. **An expected type is not enough.** It fixes the indexes for
   `val x: Top[Int] = reset { … }` and for nesting, but the HEAD of a
   for-comprehension has no expected type — `x.flatMap(…)` types `x`
   first — so the stack index fell back to its bound `Tuple` and the
   `Has` search failed. Since TestDelim, okay-ui and okay-agent all
   write for-comprehensions, this alone decides that the stack must be
   a GIVEN rather than an inferred parameter.
2. **A curried dependent context function is refused outright**:
   `(p: Prompt[R]) => Stack[p.type *: S] ?=> Prog[…]` — the obvious way
   to hand the body both the prompt and the stack — answers
   "Implementation restriction … not yet supported".
3. **A non-curried one compiles**, and nested witnesses of the same
   shape resolve to the INNER one with no ambiguity, which the nested
   case needs. But carrying the stack through it CRASHES the compiler:
   `java.lang.AssertionError: wildApprox failed to remove
   uninstantiated R`, in implicit scope computation. That road is
   closed by dotty, not by the design.
4. **Clause ORDER decides inference.** A `using` clause after the
   continuation loses: the lambda is typed first and pins the stack to
   `Tuple`. It goes before — and the stack is a type MEMBER, so no
   call site ever spells it and no method carries a stack type
   parameter that inference can pin too early.

**What it costs at the call site**, which is the number that decides
whether the lane is worth taking: `reset { p => … }` becomes
`reset { s => import s.given; … }`, one line per reset, and the prompt
is `s.p`. `reset` in a generator position needs its answer type
(`reset[Int] { … }`). The type arguments on `shift` are NOT a new
cost: TestDelim writes `shift[Int, Int, okay.Pure](p)` at every call
today.

**What is still unpriced**, and what the lane must not assume:

- Delim's four capture variants (`shift`, `shift0`, `control`,
  `control0`) differ in whether the body CONSUMES the delimiter.
  `shift0` and `control0` pop it, so their index is
  `Prog[A, p.type *: S, S]` rather than the balanced shape — the
  probe only exercised the balanced one.
- `Delim.abort`, and the spec's own first caveat: an abort inside a
  block promising a transition drops the continuation, so the
  transition does not happen. The type says it did. That caveat is
  already a required test in Behavior and it stays required.
- The four files outside the core that name Delim — `Scope` and
  `Screen` in okay-ui, `Stepper` in okay-agent, `Cut` in okay-llm —
  are where the one-line-per-reset cost is actually paid, and none of
  them has been read for this.
- Nothing here is measured. The indexes are phantom and the facade
  erases, so the expectation is allocation identical to the byte, the
  way `cont-on-free` measured — but an expectation is not a number.
