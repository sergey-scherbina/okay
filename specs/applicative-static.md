# Applicative and Selective: the static half of a program

## Overview

An `Applicative` program has a pure lambda term for its spine and
effects only at its leaves; a `Selective` program adds conditionals
whose BOTH branches are written down. Neither can bind the result of
one effect into the body another effect sees — and that limitation is
the asset: every effect such a program may perform is known BEFORE it
runs. A `Monad` program gives that up at its first `flatMap`, whose
continuation is an opaque function.

okay has the whole ladder in `src/main/scala/Monad.scala` — `Functor`,
`Applicative` (line 80), `Selective` (line 93), `Monad` — and uses one
rung of it. `Selective` has no consumer outside that file. `traverse`
and `sequence` (Monad.scala) take an `Applicative`, but the only
instance a program `A ! F` has is the one derived from its `Monad`
(Free.scala:76), so they sequence. `Async.par` (Async.scala:268) runs
two programs on two fibers and is not an `Applicative`, so `traverse`
cannot see it. Nothing lists a program's effects without running it.

This spec adds the missing consumers, staged, each with the test that
decides it: a parallel applicative over `Async` so `traverse` can be
parallel by choosing the instance (stage 1); a free selective over any
effect signature `F` that answers "which operations may this program
perform" before running and converts to `A ! F` to run (stage 2); and,
only if those two earn their keep, the dependency analysis in the
`direct` macro that emits applicative structure for independent binds
(stage 3). Stage 4 writes the chapter: the ladder as the staging
boundary, with the S/K/I reading of `Reader` and Turner's combinators
as the history of `Func` and `Fuse`.

The fact that started it, stated at its true size. For the function
applicative `Env => A`: `pure(a) = _ => a` is **K**, `f.app(x) = e =>
f(e)(x(e))` is **S**, and `identity` is **I** — which okay already
names: `Reader.ask` (Reader.scala) IS the I combinator, `pure` is K,
`app` is S, and Scala 3's `A ?=> B` with `provide` (Provide.scala) is
the same SK-applicative in the language's own syntax. That instance is
Turing-complete because SK is. For an ARBITRARY applicative the claim
is weaker and that weaker form is what this spec builds on: McBride &
Paterson's idiom bracket `pure(f) <*> fa <*> fb` (Monad.scala:86
spells it `<*>`) lifts a pure lambda term over effectful ARGUMENTS and
nothing else. The spine is the λ-calculus; the leaves are the effects;
the two never mix. Turner (1979) compiled λ-terms to S, K, I, B, C by
bracket abstraction — `[x]x = I`, `[x]y = K y`, `[x](a b) = S ([x]a)
([x]b)` — and the K-versus-S split there is exactly the question stage
3 asks of a `direct` block: does the bound variable occur free in what
follows? Miranda ran on that compilation for a decade; okay already
runs on its modern form — an inline program at the `Func` carrier
(Cont.scala:401, `pure = _(a)`) is compiled to closures by the Scala
inliner, and `Fuse` (Fuse.scala) β-reduces in a macro until no
combinator is left, which is Turner's optimisation goal by another
route. `Free.resume`'s `Bind(Bind(a, f), g) → Bind(a, f(_).flatMap(g))`
(Free.scala:138) is the B rule; `Bind(Pure(a), f) → f(a)` is β; `Delay`
is Y with a trampoline. None of that changes here; the chapter names it.

## Interface

### Stage 1 — `Par`: the parallel applicative over Async

```scala
// Par.scala (core, src/main/scala)
opaque type Par[A] = A ! Async
object Par:
  /** wrap: this program is one leaf of a parallel spine */
  def apply[A](p: A ! Async): Par[A]
  extension [A](p: Par[A])
    /** unwrap: the spine as an ordinary program; leaves already
     * joined by Async.par where the spine said app */
    def seq: A ! Async
  /** the instance: pure lifts, app FORKS both sides (Async.par),
   * fmap does not fork (one leaf, nothing to run beside it) */
  given (using Scheduler): Applicative[Par]

  /** the two derived doors, INSIDE the object — see the correction
   * below: the top-level names were taken */
  def traverse[A, B](xs: Seq[A])(f: A => B ! Async)(using Scheduler): Seq[B] ! Async
  def sequence[A](xs: Seq[A ! Async])(using Scheduler): Seq[A] ! Async
```

**CORRECTION, made while implementing (2026-09-17).** This spec was
written believing there was no parallel door. There is:
`parAll` and `parTraverse` (src/main/scala-jvm-native/Parallel.scala)
have been shipping a fiber per program, joined in order, and the
compiler said so — `parTraverse is already defined`. They stay, and
the new doors live inside `object Par` rather than beside them. The
three differences, all real: the old pair is JVM/Native only (a
blocking join needs `CanBlock`), it does not cancel the siblings of a
failed leaf, and it takes a flat SEQUENCE. What stage 1 actually adds
is therefore the INSTANCE — generic applicative code becoming
concurrent by instance choice — not the door. Measured consequence:
for a flat sequence `parAll` is the cheaper road and the Results below
say by how much.

`Par` is NOT a `Monad` and offers no `flatMap` — Haxl's point (Marlow
et al. 2014): a `flatMap` would make the spine sequential again and
the type would stop meaning "these are independent". `traverse` and
`sequence` keep their signatures and their sequential default.

### Stage 2 — `Static`: the free selective over a signature

```scala
// Static.scala (core)
enum Static[F[+_], A]:
  case Pure(a: A)
  case Op(fa: F[A])
  case Ap[F[+_], A, B](f: Static[F, A => B], a: Static[F, A]) extends Static[F, B]
  case Select[F[+_], A, B](e: Static[F, Either[A, B]], f: Static[F, A => B]) extends Static[F, B]

object Static:
  /** one operation as a leaf */
  def op[F[+_], A](fa: F[A]): Static[F, A]
  given [F[+_]]: Selective[Static[F, *]]
  extension [F[+_], A](s: Static[F, A])
    /** every operation this program MAY perform, in program order;
     * both branches of every Select — an over-approximation, and
     * exact for a spine with no Select */
    def leaves: Vector[F[?]]
    /** the same program as a monadic one: Ap is a right-nested Bind,
     * Select runs the scrutinee and then at most one branch */
    def toFree: A ! F
    /** interpret the spine into ANY applicative — the natural
     * transformation; a batching interpretation is written as one
     * of these, not as a method here */
    def foldMap[G[_]: Applicative](nt: F ==> G): G[A]
```

### Stage 3 — `direct` runs independent binds at once (DESIGN SETTLED)

```scala
object Direct:
  /** whether a direct block may run independent binds together */
  sealed trait Binds
  object Binds:
    case object Sequential extends Binds   // the default, in the companion
    case object Parallel extends Binds
    given Sequential.type = Sequential

  /** OPT IN: `import okay.Direct.parallelBinds.given` */
  object parallelBinds:
    given Binds.Parallel.type = Binds.Parallel
```

No new entry point and no new combinator. `direct[F] { ... }` takes
the mode the way it already takes `Deferral` — a `using` parameter
with a default given in the companion and an opt-in given in an object
you import, which is the shape this macro already uses and which was
already debugged (a default ARGUMENT duplicates the block; a given at
the base type says nothing).

Under `Binds.Parallel`, a MAXIMAL RUN of two or more consecutive
`val x = m.?` statements whose right-hand sides do not mention a name
bound earlier in the same run is emitted as spawn-all-then-join-all:

```scala
// written
val u = fetchUser(id).?
val o = fetchOrders(id).?
Profile(u, o)

// emitted
async(Async.spawn(fetchUser(id))).flatMap(fu =>
async(Async.spawn(fetchOrders(id))).flatMap(fo =>
fu.joinAsync.flatMap(u => fo.joinAsync.flatMap(o => pure(Profile(u, o))))))
```

A leaf qualifies when its OWN type is `X ! Async` — exactly Async,
before the mark narrows it into the row — so a block over
`Async + Throws` still parallelises its Async leaves. Anything else in
the run ends it, and the emission for the rest is unchanged.

**THE DESIGN QUESTION THE SPEC DEFERRED, ANSWERED BY THE RESULTS.**
The original text offered two instances to emit `app` against and did
not choose. Both are now refused, and the Results say why.

- **The row's own instance is a pure loss.** `Monad.app` is
  `f.flatMap(g => fmap(a, g))` (Monad.scala), so emitting `app` for
  `A ! F` builds the same binds plus one `map` node per join. Nothing
  is gained but a shape nobody reads at run time.
- **`Par` is the right semantics and the wrong SHAPE.** Stage 1
  measured the applicative spine at about 5x a flat `parAll` at eight
  leaves, because `app` is pairwise: N leaves are N joins and 2N
  fibers. A macro emits a whole GROUP at once, so it is in the one
  position that does not have to be pairwise — and the flat shape is
  the cheap one. Emitting `Par.app` chains would have taught the
  compiler to write the expensive form.

So stage 3 emits neither `app` nor `Par`: it emits the flat join
directly, out of `Async.spawn` and `joinAsync`, which are public,
typed per leaf (each fiber keeps its own element type, so nothing
casts) and already cross-platform. `Par` remains the door for code
that is generic over `Applicative`; the macro is the door for a block
whose independence a reader can see.

**What it inherits, stated rather than discovered later**: spawn-all-
then-join-in-order is `parAll`'s semantics, so a failure surfaces when
the join reaches it and the healthy siblings are not cancelled. Same
as the door that has been shipping; `par`'s own asymmetry
(`par-right-failure-waits`) is not in this road at all.

### Stage 4 — the chapter

`docs/theory/12-applicative-static.md` (numbering follows the index at
writing time), added to `docs/theory/index.md`: the ladder as how much
the compiler or an analysis can see; S/K/I as `Reader`; idiom brackets;
free applicatives and static analysis; Selective and build systems;
Turner's combinators and Miranda as the history of `Func` and `Fuse`.
Every okay claim by `file:line`, every theory claim cited, as the
textbook's contract (specs/theory-textbook.md) requires.

## Behavior

Stage 1:
- [x] `Par` is a lawful Applicative: identity, homomorphism,
      interchange, composition hold on results for programs that
      complete (TestPar).
- [x] Concurrency is proven WITHOUT a clock: N leaves each wait on a
      rendezvous that opens only when all N have arrived; under
      `parSequence` the program completes, under `sequence` the same
      leaves would deadlock, and the test asserts completion with a
      bounded timeout as the failure road — not a duration.
- [x] A failing leaf fails the spine and cancels its siblings, exactly
      as `Async.par` does today (inherited, asserted, not
      re-implemented).
- [x] `Par.traverse(xs)(f)` agrees with `traverse(xs)(f)` on results and
      order for every pure `f` (TestPar).
- [x] `traverse`/`sequence` over `A ! F` are byte-for-byte unchanged:
      no signature moves, the existing tests that use them pass
      untouched.
- [x] Cost, predicted before measuring: one `Par.app` costs one
      `Async.par` plus one closure and one tuple; a JMH lane of 8
      leaves at `parSequence` vs 8 hand-written nested `Async.par`
      calls is within 10%. If it is not, the wrapper is doing work it
      should not, and the Results say what.

Stage 2:
- [x] `leaves` of a program with two `Static.op` leaves under one `ifS`
      names three operations before anything runs; `toFree` of the
      same program runs exactly two (TestStatic).
- [x] `toFree` agrees with the hand-written monadic program on the
      answer and on the sequence of operations a recording handler sees
      (program order, at most one branch of each Select).
- [x] `Static[F, *]` satisfies the Selective laws (Mokhov et al. 2019
      §2.2: identity, distributivity, associativity) on `leaves` and on
      `toFree`'s answers.
- [x] Batching, shown end to end on a synthetic `Fetch % K` signature:
      a spine of N independent `Get(k)` leaves is interpreted by a
      `foldMap` into a batching applicative that issues ONE bulk call
      for all N keys; the test counts calls to the backing store.
- [x] `leaves` of a 10 000-leaf spine is linear and stack-safe (the
      spine is left-nested by `traverse`'s foldLeft; the walk must be
      an explicit loop, not structural recursion — cf.
      assertEquals-on-a-deep-tree).
- [x] Cost, predicted before measuring: running a Static program
      through `toFree` is within 1.3x of the hand-written monadic
      program at 1 000 leaves (each Ap becomes a right-nested Bind, the
      shape `Free.resume` is fastest on). **REFUTED: 1.72x** — see
      Results. Recorded in src/jmh/history.tsv.
- [x] All existing tests stay green.

Stage 3:
- [x] Without the import, NOTHING changes: every existing direct test
      passes untouched and the emitted tree is the same.
- [x] With it, two independent marked binds run CONCURRENTLY — proven by
      a rendezvous each leaf must reach, not by a clock, exactly as
      stage 1's proof is.
- [x] A DEPENDENT pair stays sequential under the same import: the
      second rhs mentions the first bound name, so the run ends. The
      test asserts it by having the second leaf need the first's
      answer, which cannot even be spawned early.
- [x] Answers and binding order are unchanged either way (the same
      block, both modes, same result).
- [x] A leaf that is not exactly `X ! Async` ends the run and the rest
      compiles as before (a State leaf between two Async ones).
- [x] A run of three or more emits ONE flat group, N spawns then N
      joins — asserted on the emitted shape, not inferred from timing:
      a counter in a Scheduler says how many fibers were forked.
- [x] Cost, predicted before measuring: at 8 independent leaves the
      parallel block is within 20% of `parAll` on the same leaves (it
      is the same shape), and the sequential block is unchanged to the
      byte against master.

Stage 4:
- [x] The chapter exists, is in the index, and every `file:line` it
      cites resolves on the commit that lands it — checked by a grep
      over the cited files, as the textbook's own chapters were.

## Out of scope

- Changing what `traverse`/`sequence` do for `A ! F`. Their default
  stays the monad-derived, sequential instance; parallelism is opted
  into by name (`parTraverse`) or by instance (`Par`).
- New nodes in `Free`. The tree is `Return | Inject | Bind | Delay`
  after core-cleanup and its hot loops sit at a measured inlining
  threshold (specs/core-cleanup.md; freer-base Results); `Static` is
  its own type and converts.
- Applicative parsers with static first-sets (Swierstra & Duponcheel
  1996). okay-parse is an instruction language over a total builder,
  not monadic combinators, so the pressure that motivates them is
  absent. Named so the next agent does not re-derive the fork.
- The general ApplicativeDo transform over arbitrary Scala (re-typing
  under lambdas, dotty-cps-async scale). Stage 3 is scoped to the
  flat block `direct` v1 already handles (specs/direct-macro.md).
- Distributed batching. That is okay-dataflow's plan
  (specs/dataflow.md); a `Static` spine is an input it could take, not
  a thing this spec ships.
- Selecting stage 2's first REAL consumer. Candidates, in the order
  their Results should be gathered: a program's capability list for
  okay-di (the "needs" of a module derived rather than declared —
  specs/di.md); batching `Sql` leaves in okay-sql; explain/dry-run for
  rule packs in the private okay-watch. Each is a lane with its own
  measurement; this spec proves the mechanism on a synthetic
  signature and stops.

## Design

**Why an opaque type over `A ! Async` and not an effect.** Parallelism
here is a property of the SPINE (how `app` joins two leaves), not an
operation a handler answers. Making it an effect `Par % ...` would put
it in every row that wants it and in front of every handler that does
not care. The opaque type costs nothing at run time, gives `traverse`
an instance to find, and leaves the row alone. `Async.par` already
carries the semantics (fork both, fail fast, cancel the sibling) and
is not re-implemented.

**Why `Static` is separate from `Free`.** Three reasons, each
measured elsewhere. The Free tree's node count is priced: `resume` and
`handle` sit near `FreqInlineSize` and a fifth arm re-decides the
inlining of every caller (inlining-threshold, four faces). A free
applicative's `Ap` needs a different fold than `Bind` (both sides are
programs; the walk is a tree, not a list), so it would be a second
interpreter inside the first. And the point of `Static` is the
GUARANTEE that no `Bind` is present, which a type that also has `Bind`
cannot give. Conversion is one direction, `toFree`, and it is cheap.

**Why Selective and not just Applicative.** Every real program has a
conditional. Without Select the author's choice is to run both
branches (wrong) or drop to `Monad` (opaque). Mokhov et al. show the
middle rung is enough for over-approximate static analysis — build
systems, Haxl's `if` — and okay already has the trait with `ifS`,
`whenS`, `branch`, `unlessS`. The `leaves` over-approximation is stated
as such in the doc comment and asserted in the tests: what MAY run.

**`foldMap` is the batching door, not a `batch` method.** A batching
interpretation is a natural transformation into an applicative whose
`app` accumulates requests and whose run answers them at once; that is
one more instance, written where the backing store is known. Keeping
the door generic keeps `Static` free of any store.

**Stage 3's instance question is real and deferred.** Emitting `app`
against the row's own instance changes performance not at all (the
derived `app` is `flatMap` + `map`) and only exposes shape; emitting
against `Par` changes SEMANTICS (interleaving of leaves' side effects),
which is exactly what a user must opt into by name. The right answer
depends on how stage 1 is actually used, which is why this spec does
not guess.

## Decisions

- **Stage the work; gate stage 3 on Results** — chosen because
  stages 1 and 2 are each under 200 lines with a decisive test, and
  stage 3 is a macro change whose value depends on how 1 and 2 are
  used. Rejected: one lane for all four (the macro would hold the spec
  hostage; useful-not-just-works says deliver call sites as pairs, and
  1 and 2 each have one).
- **`Par` does not extend `Monad`** — chosen because the type's
  meaning is independence. Rejected: `given Monad[Par]` for
  convenience (Haxl explicitly refuses this; a `flatMap` on `Par`
  would silently sequence and the parallelism would vanish where it
  was written most naturally).
- **Do not make `Free`'s Applicative parallel** — chosen because the
  monad–applicative consistency law (`app` equals the `flatMap`
  derivation) is what every existing `traverse` over `A ! F` relies on
  for ordering of effects. Rejected: "faster by default" (it would
  reorder Writer, State and every interleaving-sensitive handler
  behind the author's back).
- **`leaves` returns `Vector[F[?]]`, existential, no cast** — chosen
  because the analysis is over operations of unknown answer type and
  the caller pattern-matches on the operation enum, which is how every
  handler in okay already reads an `F[A]`. Rejected: a typed
  `leaves[K]` that needs a `TypeableK` (that machinery is `test` only
  after core-cleanup and the no-casts rule forbids the shortcut).
- **A synthetic `Fetch` signature for the batching proof, not okay-sql**
  — chosen because the mechanism and its measurement should not wait on
  a module's schedule; the real consumer is a lane of its own (Out of
  scope). Rejected: proving on `Sql` first (couples a core lane to a
  module's test database).

## Results

Stage 0 (this spec): written 2026-09-17. Predictions above are the
bars; each stage records its measured numbers here, with the JMH
lanes named, before it lands.

### Stage 1 — `Par`, landed 2026-09-17

**What the spec got wrong, found by the compiler.** `parTraverse` and
`parAll` already existed (src/main/scala-jvm-native/Parallel.scala) and
the build refused the duplicate name. The doors moved inside
`object Par`; see the CORRECTION under Interface. The instance, which
is what stage 1 is actually for, is unaffected.

**What the tests found, filed rather than fixed — and FIXED the next
day, by the pin.** `Async.par`'s doc said "a child failure fails the
pair and cancels the sibling". It did so on the LEFT only: measured
2026-09-17, `par(slow, failing)` failed after 3.017 s and
`par(failing, slow)` after 0.0007 s, because the two completions were
registered in a nest rather than side by side. Filed as
`par-right-failure-waits` (BUGS.md) with the reduced repro, and
TestPar pinned BOTH orders with a message telling whoever fixed it to
come back. That is what closed it: landing par-fail-fast (2026-09-18)
failed TestPar with "par-right-failure-waits is FIXED — strengthen
this assertion and close the BUGS.md entry", and the entry was closed
because a test said to. `Par`'s fail-fast is symmetric now.

**The numbers.** ParBenchmark, 8 trivial leaves, `-f 3 -prof gc`,
three rounds on one box (load 3–6.6):

| lane | µs/op (r1 / r2 / r3) | B/op |
|---|---|---|
| bracketPar8 (idiom bracket at `Par`, 7 joins) | — / 52.404 / 52.190 | 11 401 / 11 510 |
| handNested8 (7 hand-written `Async.par`) | 52.611 / 52.249 / 53.101 | 10 504 / 10 704 / 10 503 |
| parApplicative8 (`Par.sequence`, generic `traverse`) | 59.301 / 61.934 / 59.124 | 15 284 / 14 594 / 14 355 |
| parAllFlat8 (`parAll`, flat, one fiber per leaf) | 11.746 / 9.502 / 11.251 | 4 200 / 4 200 / 4 195 |
| sequential8 (`traverse`, no fibers) | 0.352 / 0.424 / 0.345 | 3 840 |

- **PREDICTION CONFIRMED** — "the wrapper within 10%": the matched
  pair (bracketPar8 vs handNested8, same seven joins, same leaves) is
  **1.003 and 0.983** across the two rounds that have both. The
  carrier is inside the noise. The load-proof residue is in the bytes:
  +700 to +1 000 B/op, ~100–140 B per join for the two closures.
- **The applicative SHAPE costs ~5x** against `parAll` (53.1 vs 11.25
  in one round), and that is the spine, not the implementation: `app`
  is pairwise, so N leaves are N joins and 2N fibers. A further +13%
  and +2 845 B/op from bracketPar8 to parApplicative8 is generic
  `traverse` building its Vector element by element.
- **The rule that follows**, and it is in the doc comment, the guide,
  the typepedia and theory ch. 12: flat sequence of same-typed
  programs on the JVM -> `parAll`. Spine with leaves of different
  types, or generic code that never heard of Async -> `Par`, where the
  alternative is not `parAll` but running sequentially (0.345 µs/op
  buys you nothing when the leaves are real work).

### static-foldmap-stack-safe — landed 2026-09-18, and the cast was not needed

`foldMap` was the one door of the free selective that recursed on the
host stack. The BACKLOG entry predicted it "needs the existential
reassembly cats does with internal casts, which the no-casts rule says
must be earned". **It does not.**

`Args[F, T, C]` is a TYPE-ALIGNED list of what is left to apply, and
its constructors carry the alignment: `Done` exists only at
`Args[F, C, C]`, and consing an argument of type `X` onto an
`Args[F, R, C]` yields an `Args[F, X => R, C]`. Matching refines the
types, so coming back up is ordinary typed code — two tail-recursive
loops, no cast.

**THE FIRST VERSION STILL OVERFLOWED, at exactly the old depth, and
the reason is the useful part.** Walking down an `Ap`'s FIRST
component is the obvious axis and it is the wrong one: `traverse`'s
`foldLeft` builds `Ap(Ap(Pure(g), acc), leaf)`, so two steps down
reach `Pure(g)` and the whole accumulator — the deep thing — is pushed
as an ARGUMENT, folded by an ordinary recursive call. The stack came
back by another road and a 50 000-leaf test said so, rather than a
guess.

The answer is a third `Args` case: `Ap(Pure(g), a)` is not an
application to walk past, it is "fold `a`, then map by `g`". Carrying
the pure function lets the walk continue INTO the accumulator. It is
sound precisely because the first component is `Pure` and performs
nothing, so running `a` first reorders no effects.

Two `@unchecked` type tests remain, and they are the same claim
`Free.resume`'s forty-two callers make: the enum has three cases, the
CLASS test is total, and the type arguments are the ones the
constructors guaranteed. The ascription is needed rather than a
constructor pattern because `x` and `r` must be NAMED — the match
refines `T` to `x => r` but `g` is still written `G[T]`, and the
Applicative's `app` cannot find its `F[A => B]` shape through the
alias. A helper method taking the pieces would have worked and would
have cost the `@tailrec`, which is the whole point.

### Stage 3 — `direct` runs independent binds at once, landed 2026-09-17

**BOTH PREDICTIONS ANSWERED, one confirmed and one measured the wrong
pair first.** DirectParallelBenchmark, eight trivial independent
leaves, `-f 3 -prof gc`:

| lane | µs/op | B/op |
|---|---|---|
| parallel8 (the block, with the import) | 11.161 ± 2.459 | 6 712 |
| parAllFlat8 (the same leaves through `parAll`) | 11.669 ± 3.763 | 4 848 |
| sequential8 (the same block, no import) | see the A/B below | 1 344.002 |
| handChain8 (the flatMap chain by hand) | 0.083 ± 0.004 | 856 |

- **THE MACRO EMITS THE FLAT SHAPE.** parallel8 / parAllFlat8 = 0.956,
  inside both error bars, against a predicted 20%. That is the claim
  stage 3's design rests on: a macro holds the whole group, so it
  never has to be pairwise, and `Par`'s ~5x pairwise spine is not what
  gets built. The residue is in the bytes — +1 864 B/op over the bare
  door, which is the block's own eight binds around the spawn/join.
- **"WITHOUT THE IMPORT NOTHING CHANGES" NEEDED A REAL A/B**, and the
  first attempt was the wrong pair: sequential8 against handChain8
  prices the direct macro against hand-written code, which was never
  equal and has nothing to do with this lane. The right pair is
  sequential8 HERE against sequential8 on master, and it was run in a
  master worktree with the same file: **1 344.002 B/op against
  1 344.001 B/op**, identical to the digit. Times were not readable —
  the lane's rounds fell at load 13-56 and sequential8 came back
  0.194 ± 0.071 and 0.266 ± 0.187 while the control handChain8 held at
  0.083 ± 0.004 on both sides — so the bytes are the verdict, which is
  this repository's standing rule for exactly this situation.

**What the implementation found.** `asMark` does not see through the
inline expansion: by the time the macro has a leaf like `async(1)`,
`Inlined` nodes carry `$proxy` bindings that `stripped` does not
remove, so matching the syntax found nothing and the feature was
silently off. The fork COUNT caught it — a test asserting three fibers
got zero — and the fix is to ask `compile`, which already knows how to
get through all of it, and then ask the TYPE whether the program it
hands back is `X ! Async`.

**The limit v1 keeps, stated rather than discovered later**: for a
block over a WIDER row the compiled leaf has already been narrowed
into the row, so it is not spawnable and the import does nothing,
quietly. Pinned by a test over `Reader % Int + Async` asserting zero
forks, and filed as BACKLOG `direct-parallel-wider-rows`.

### Stage 2 — `Static`, landed 2026-09-17

- `leaves` on `ifS(flag)(get a)(get b)` reports **3** operations
  before anything runs; `toFree` performs **2** (TestStatic). That
  pair is the whole type in one line.
- Batching: 50 leaves, **1** call to the store through `foldMap` into
  an accumulating carrier, against **50** for the same program run the
  ordinary way — counted, not asserted.
- `foldMap` needs `Selective[G]`, not `Applicative[G]`: an applicative
  carrier cannot run one side of a `Select` and not the other. A
  carrier that runs both says so with `selectA`, which is exactly the
  over-approximation `leaves` reports.
- **Stack, measured 2026-09-17 on a traverse-built spine, default JVM
  stack**: `leaves` (explicit stack) returns at 50 000; the same walk
  written recursively returns at 10 000 and overflows at 50 000;
  `toFree` (Free.defer) has no bound found; `foldMap` folded 5 000 and
  overflowed at 10 000. **`foldMap` IS STACK-SAFE SINCE
  static-foldmap-stack-safe (2026-09-18)** — 50 000 leaves fold, and
  the cast this entry expected to need was not needed. See that
  lane's Results below.
- `F[Any]`, not `F[?]`, in `leaves`: a wildcard application of a
  higher-kinded parameter is unreducible (E043, the wall
  specs/schema-fold.md hit), and covariance makes `F[X] <: F[Any]` an
  upcast the compiler performs itself. No cast.
**The cost prediction is REFUTED.** StaticBenchmark, 1 000 leaves,
`-f 3 -prof gc`, prebuilt against prebuilt (the matched pair — the
first cut of this file compared a PREBUILT spine against a monadic
lane that rebuilt itself every invocation, which flattered the spine
and is the mismatch benchmark-pairing-rule exists to catch):

| lane | µs/op | B/op |
|---|---|---|
| staticToFree (convert + run) | 80.655 | 843 049 |
| monadicPrebuilt (run) — THE PAIR | 46.764 | 475 088 |
| monadicBuildAndRun (build + run, context) | 57.982 | 641 158 |
| staticLeaves (READ, do not run) | 14.453 | 101 048 |

**1.72x, not the predicted 1.3x**, and the bytes agree (1.77x), which
is what makes it a verdict rather than a load artefact. The
prediction's error is named: it said "each Ap becomes a right-nested
Bind, the shape resume is fastest on" and treated the conversion as
free. It is not — `toFree` MATERIALISES A SECOND TREE, and the
remaining 368 B per leaf is that tree's `Bind`, `Delay`, thunk and
continuation.

**One optimisation was found by reading those bytes and it is in the
code.** The first `toFree` wrapped BOTH sides of an `Ap` in a
`Delay`; the right side is a leaf in every spine a fold builds, and a
leaf needs no trampoline. Earning the node (`Static.now`) took 84.251
-> 80.655 µs and 899 105 -> 843 049 B/op: exactly 56 B per leaf, one
`Delay` and its thunk. A 10 000-deep RIGHT-nested spine is now a test,
because that is the shape the fallback exists for.

**What the numbers say to a reader**, and it is in the doc comment and
theory ch. 12: `Static` is not a faster way to RUN. It is a way to
READ — listing 1 000 operations costs 14.5 µs against 46.8 µs to
perform them, a third of the price — and a way to BATCH (50 leaves,
1 call). A program you only want to run should be written monadic.
