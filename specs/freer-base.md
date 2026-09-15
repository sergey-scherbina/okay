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
  /** the library's, not the user's: every interpreter in the repository
   *  matches `Bind(Op(e), k)`, and all of them live under package okay */
  private[okay] case Bind[G[_,_,_], A, B, S, T, R](a: Freer[G, A, T, R], f: A => Freer[G, B, S, T])          extends Freer[G, B, S, R]
  /** the runner's alone: `resume` forces it before anyone sees the tree */
  private case Defer[G[_,_,_], A, B, S, T, R](thunk: () => Freer[G, A, T, R], f: A => Freer[G, B, S, T]) extends Freer[G, B, S, R]

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
  export Freer.Pure
  private[okay] val Bind = Freer.Bind                     // an export would make the alias public; a val keeps the modifier
  val Effect = Freer.Op                                   // the extractor, `case Bind(Effect(e), k)` unchanged
  def defer[F[+_], A, B](t: () => A ! F)(f: A => B ! F): B ! F = Freer.Defer(t, f)   // Defer itself is never seen
  inline def effect[F[+_], A](e: F[A]): A ! F = Freer.Op(e)   // the factory, index pinned at Unit
  // resume, next, ?, tailcall, widen, translate, relay, interpret, tracing: as today, over Freer.resume

/** stage 2: an indexed program; ordinary effects inject DIAGONALLY,
 *  transitions only through typed smart constructors */
type Prog[F[+_], A, S, R] = Freer[Lift[F], A, S, R]
def effect[F[+_], A, R](e: F[A]): Prog[F, A, R, R] = Freer.Op(e)
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

Stage 2 — the index as typestate, Delim first:
- [ ] `Prog[F, A, S, R]` and the diagonal `effect[F, A, R]` factory
      exist; `A ! F` is `Prog[F, A, Unit, Unit]`, so stage 1 code is
      untouched.
- [ ] Delim carries its prompt stack in the index: `push(p)` is
      `Prog[Delim + F, R, P :: St, St]`-shaped, `shift(p)` requires `p`
      in the stack, and `NoPrompt` (Delim.scala's thrown case) becomes
      a compile error in a `compileErrors` test. TestDelim green with
      no annotations inside for-comprehensions beyond what PState needs
      today (none).
- [ ] The spec's own first caveat is a test: a `Throws` abort inside a
      block promising a transition drops the continuation and the
      transition does NOT happen — asserted, so nobody reads the type
      as a run-time guarantee.

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
  signature; stage 2 adds `Prog` and the diagonal `effect[F, A, R]`
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
- **`Defer` private, `Bind` `private[okay]`, `Pure` and `Op` public.**
  The operator asked whether the two had to be public (2026-09-15);
  the code answers. `Defer` is matched outside Cont/Free only by
  `runFree`, `!.resume`, `!.?` and Async's loop — all runners, all
  replaced by `resume` — so it stays as private as it is in `Cont`
  today, at no cost; `Freer.defer`/`Cont.defer`/`!.defer` are the
  public factories. `Bind` is CONSTRUCTED outside the runners only by
  Async's loop, but MATCHED at 89 sites in 15 packages (`okay`,
  `okay.agent`, `okay.blob`, `okay.jdbc`, `okay.pg`, `okay.llm`, …),
  every one under `okay`: `private[okay]` hides the node from a user of
  the published library and costs the repository nothing. For that
  user the tree is `resume` + `next`/`?`/`fold`/`translate`/`relay`/
  `interpret`. It IS an API change for `Free`, whose `Bind` is public
  in v0.1.1 — acceptable at 0.x, said here so the release note says
  it. The ALLOCATION-FREE road to a fully private `Bind` exists and is
  recorded, not taken: the private node implements a public trait
  `Next[G, X, A, S, T, R]` (`op`, `k`), and interpreters match it by a
  type-test pattern with bound type variables, `case n: Next[G, x, A,
  S, t, R]` — Delim's own `case n: Next[F, a, R]` idiom, no object
  created, compiler-checked. It rewrites the 89 sites from `case
  Bind(Effect(e), k)` to `n.op`/`n.k` and reads worse; take it the day
  an external interpreter needs it. A case-class view returned by
  `resume` was refused earlier for one allocation per step on the
  hottest path (Effects.scala's `resume` comment).
- **Names.** `Freer` after Kiselyov–Ishii; `Op` for the leaf (not
  `Inject`, which named the effect side only); `Lift` for the unary
  signature's leaf; `Shift.Absorbed` for the fused function, nested
  because `okay.Fused` (handler-fusion loops) and `okay.Fuse` (optics)
  are taken; the `!` alias and `Effect` extractor keep their names so
  the 89 match sites do not move.

## Results

(to be filled per stage; every number with its history.tsv row)

- Stage 0:
- Stage 1:
- Stage 2:
