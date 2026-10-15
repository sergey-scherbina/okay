# The platform on the kernel's words: `A ! F + G` resolved to a row where it is placed

Status: in progress, 2026-10-15 (steps 1–2 landed the same day: `RowEffects`' words are the facade's, the root's `+` the sum of effects). Follows the decision of 2026-10-14 (okay-core's docs/okay.md): the one monad
is the machine, `Effects[M]` is `RowEffects[M]`, okay-cont is gone, "the platform's body on okay-freer is the
phase after". This is that phase, done so that nothing of the platform's body is rewritten.

## Overview

The platform's programs are written `A ! Writer % String + Async`, built by `effect`, `pure`, `flatMap`, and run
by `handle`/`run` — okay-freer's words over the free monad `Free[F, A]` (`Freer`, Kiselyov–Ishii), whose row is a
UNION of effects, `F + G = [A] =>> F[A] | G[A]`. Since 2026-10-14 the captures of that tree run on okay-core's
`Cont` (the carrier), but the tree itself is interpreted by okay-freer's own loop (`Classic.handle`: `resume`,
`split` by `TypeableK` at every handler, `HandleFrames` for depth).

The kernel has the same words with the same spelling: `okay.core.!`, `+`, `%`, `perform`, `pure`, `handleIn`,
`run`, and a std of the same names (`reader`, `state`, `writer`, `throws`, `choose`, `collect`, `generate`,
`stream`). Its row is a LIST, `F +: G +: Pure`, and an operation carries a PATH to its effect in it, so the machine
answers in place by the path, no class test, no interpreter loop.

The proposal: the platform's words BECOME the kernel's. `A ! F + G` keeps its spelling and its meaning — a program
needing `F` and `G`, in any order, among any others — and is the kernel's program-in-a-place: the row is resolved
where the program is put under handlers, not where it is written. The platform's body (1131 files in 92 modules,
`import okay.freer`) is not rewritten; what changes is what the imported names mean, and the few places that hold
a tree as a value.

## Why not a union to a list at compile time

A union cannot be taken apart at the type level: `F[A] | G[A]` is normalised (commutative, idempotent), and a match
type has no "head of a union". The platform knows it — `Distinct` and `Reach` are macros over the written text of a
type. So there is no map `F + G ⟶ F +: G +: Pure` by a type function.

The kernel's `+` is not a union: `okay.core.+[F, G] = [X] =>> Both[F, G, X]`, a pair as an effect, and the evidence
`Paths[F + G, R]` is found by givens (`both`, `one`, `rest`), each effect's path on its own. A program
`A ! F + G` is `(c: Needs[F + G]) ?=> Eff[c.R, A]`: the row `c.R` is whatever row the place has, and `F + G`,
`G + F`, `F + G + H` all fit it. That is the union's meaning — order does not matter, more effects around do not
matter — with a path for every operation. The union is resolved, at compile time, where it is used.

## The mapping

| okay-freer | the kernel | note |
|---|---|---|
| `A ! F` | `okay.core.!` | a context function of `Needs[F]`, not a tree value |
| `F + G` | `okay.core.+` | `Both`, decomposed by `Paths` |
| `F % S` | `okay.core.%` | the same |
| `effect(e)` | `perform(e)`: an `Op[F, X]` | its path found at the bind that uses it |
| `pure`, `flatMap`, `map`, `delay` | the kernel's | `Cont` nodes |
| `handle(m)(h)` / `Handler.*` | `handleIn`, `handle` | the handler forms below |
| `run(m)` | `run` | `Machine.run` |
| `Row.at`, `!.widen`, `In` as a Unit | — | gone: the path is exact by construction |
| `TypeableK`, `Distinct` | — | gone: no class test, no duplicate to forbid (two `Writer % String` are two places) |
| `Cps[F, S, R, A]` | `Cont[R, S, R', A]` | answer types are the kernel's `I`, `O` |
| `Shift`, `Delimited`, `reset`/`shift`/`shift0` | `Shifts[S, T]`, `Atm`, `Dollar`/`Shift0` | control as an effect of the row, or the answer moved |
| `Indexed` (a parameterised monad) | `Atm`, `PState` | the state's type in the answer |

Handler forms, by what each needs of the machine:

| okay-freer | the kernel's form | how |
|---|---|---|
| `Handler.answer` / `Answers[F]` | `Answering` | `value(op) = answers.handle(op)`: in place, the `Found` cache |
| `Handler.state`, `Stepped` with no `Halt` | `Stateful` | `value = step._2`, `next = step._1`, `ret(s, a)` |
| `Stepped` with `Halt` | `Deep` | `Halt(s)`: `k` dropped, `halted(s)` |
| `Interpr` (`into`, clauses with `k`) | `Deep` | an answer → `k(x)`; a capture → its body with `k`: the kernel's `k` IS the rest, a program over the row outside |
| `Full.run` (a loop of its own over the tree) | `Step` | the loop drives the program by steps; see "walkers" |

## What goes, what stays

Goes, because the machine does it: `Classic.handle`'s loop and `resume` (the machine's frames take a left-nested
bind in O(1), no rotation), `HandleFrames` (the machine's stack is on the heap), `ShiftMachine` (it is the machine),
`Bang`/`Eager` as encodings of the same interface (the one encoding is `Cont`), the carrier
(`okay.Carrier`, `Control[Carrier]`: a program is a `Cont` already).

Stays, untouched — they never look at the tree: `Fold`, `Aggregate`, `Staged`, `Layered`, `Replayable`,
`ContReplay`, `direct` (okay-direct), the std's effects as enums, every module's programs.

Moves onto the kernel's nodes, one file at a time with its tests: `Cps` (answers are `I`, `O`), `Delimited` and
`Shift` (`Dollar`, `Shift0`, `Shifts`), `Indexed` (`Atm`/`PState`), `Static` (lowers a selective into a program:
builds `Cont` nodes; its `peek` is `Step`), `Lexical` (one look ahead: `Step`).

## A program as a value: what Free had, and what the kernel has

Free was a tree value: stored, passed, matched on its nodes, re-seen at a wider row by a cast (`Row.at`: the union
erases). The kernel has the same property, in three forms, and one more than Free had:

- `A ! Es` — a program polymorphic in its row, a value (a context function of `Needs[Es]`): stored and passed, not
  matched; fitted to any place holding its effects, in any order, by `Needs`, with no `View` and no cast.
- `Eff[R, A] = Cont[R, Nothing, Nothing, A]` — the program AT A ROW: a tree of nodes (`Return`, `Bind`, `Map`,
  `Defer`, `Perform`, `Handle*`, `Guarded`, `Resume`, …), a public enum — matched, walked, reified (`reify` and
  `reflect` are the identity: the tree is syntax). From `A ! Es` by `p.at[R]`, or under `Place.at[R]`.
- `Rest` and `Resume(rest, y)` — the REST of a program after an operation, as a node: what Free never had (after
  the first `Bind` its rest is a host closure, which is what `Static` exists to get around). A handler installed is a
  node too (`HandleK`), a guard a node (`Guarded`): a program with its handlers on, or a program suspended at an
  operation, is a value — stored, stepped (`Step`), handed to another machine (the protocol).

What differs is the row of a value. Free's `Free[F, A]` was a `Free[R, A]` for any `R ⊇ F` for free — the union
erases, `Row.at` is a cast. A `Cont` value has a concrete row, `Eff[F +: G +: Pure, A]`; put under handlers of
another order it is RE-SEEN, not cast: `widen`/`Removed` put one `View` node around it, and the paths of its
operations are relocated at the lookup (`Removed.relocate`). One node a re-seeing, not a rebuild — and not zero, as
a cast is. It is needed only where a value already at a row meets a place of another row; programs written as
`A ! Es` never need it. Those places are what the compiler lists at step 1.

## Two of one effect in a row: which one an operation uses

A `perform` never ADDS an effect to a row. The row is the place's — the handlers in force — and a program needing
`F` fits a place where `F` is present: its path is found by the givens of `In` (`here` first, then `there`), so an
operation goes to the NEAREST `F` from where the program is placed, innermost first. Two `Writer % String` in a row
are two places; an operation written against `Writer % String` reaches the nearer one. The same rule the platform
had at run time (`TypeableK` tested on the way out, the first handler that takes it), now decided at compile time
by the path — and visible in the type: `In.There(In.Here())` names the outer one.

What tells two effects apart is their TYPE: `Writer % String` and `Writer % Int` are two effects, two paths,
nothing to confuse. The same type twice is two places of one effect — the nearer one is the program's unless it
says otherwise: a path named by hand (`Path`, `In.There`), or an interceptor (`local`, `handleWith`,
`Removed.again`), a handler of `F` at the nearer place whose own clauses perform `F` to the outer one. A handler
takes its effect from a place of its own choosing too (`Removed`: the head by `handleIn`, any place by `handle`),
so "which `Writer` does this handler serve" is in its position, the same way.

## Stepping, where a walk is needed

`Freer.resume` normalised a tree to a head form — the next operation and the rest as a function — in constant
stack, running nothing. The kernel's `Step` is that: `p.step` runs the program to its first operation of the row
and gives `Yield(op, in, resume)`, the rest `k` as data, multi-shot, constant stack. On a program with no handler
installed inside, every operation comes out in order and nothing runs until it is resumed. A `Step` allocates a
`Yield` and a `Resume` (three closures) where `resume` rotated pointers; if the lanes say so, the kernel gets a
lighter `Head` beside `Step` — the operation and the machine's own rest, no closure — a change in the machine's
`Out` alone.

## Rows, by the kernel

A row is a list; an effect may appear twice (two `Writer % String` are two places, the nearer wins by path);
`Needs` fits a program to any row holding its effects; `Removed` takes a handler's effect from any place in the
row. What the platform's `Distinct` forbade — two members of one signature — is a feature here, not a misroute.

## Checks and measures

- The platform's suites are the specification: 6937 + 184, every module, every platform — each module's on the
  kernel's words once it has moved (two packages, above).
- okay-std's 60 JMH lanes, A/B against the carrier road (the last run: neutral within noise against okay-cont,
  `delimDollarResume` −20%, `handleCapture` +9%). Expected: `handle*` and `dispatch*` down (no `resume` loop, no
  `TypeableK` per level, no `HandleFrames`), `pure`/`bind` even (the same `Cont`), captures as the kernel's.
- okay-bench's `core` lane (223 µs on the mixed program) is the bar the platform's own program should meet on the
  new road; the `kernel` lane (286) its program over the protocol.

## `!` a value, `!!` the facade's (2026-10-15)

The platform calls methods on programs held as values — `p.handle(h)` (≈800), `p.runWith` (≈1800), `p.run`
(thousands), a program in a `val` handled later. The facade's `!` is a CONTEXT FUNCTION of the place: a method on it
outside a program applies it first, so none of those compile. So the root has two:

- `A ! Es` — the program as a VALUE (`Program.scala`, the default): one object a node (`Run`), a function of the
  paths of its effects in the place's row, at a place the kernel's `Eff`. okay-freer's words on it: `flatMap`,
  `map`, `>>`, `p.at[Es2]`, `p.handle(h)`, `p.run`, `pure`, `effect`, `defer`. The effects of a bind are the
  continuation's if it has the program's among them, else the two summed (`Join`); a program needing fewer effects
  fits where more are expected (`widen`, a conversion; each effect found by its `Req`); a handler is a value
  polymorphic in the body's value (`Handler[E, O]`, its kernel form at a row).
- `A !! Es` — the facade's form (`okay.core.!`): no object a node, about twice as fast on binds, no methods outside
  a program (`handle(h)(p)`, `run(p)`). `p.ctx` and `Program.of(q)` cross between them.

Measured (one loop, 1000 × ask + tell, µs / KB a run): the kernel at a row named 21 / 297; the facade's `!!`
17 / 281; okay-freer, classic, 32 / 384; the value `!` 34 / 497. The value is at the classic's speed on binds; the
gain on the core road comes from the kernel's handlers (captures, forwarding, cancellation). Where a hot path wants
more, `!!` is there.

## A runner: `Split`, not a shape

okay-freer's runners are `A ! E + F => B ! F`, `F` inferred by the union's subtyping from a program in any order.
The kernel's `+` is a tree (`Both`), and `F` by shape fails on `A + B + C` and on any order where `E` is not first.
So a runner takes `E` off wherever it is: `Split[Es, E] { type Rest }` (okay-core 7d48d40), found where the runner
is called from the program's own effects; over `E + F` with `F` abstract its `Rest` is `F` itself. A runner is
`def run[Es[_]](p: A ! Es)(using s: Split[Es, E]): B ! s.Rest`; `p.handle(h)` is one. A chain of runners infers
with nothing named (`TestRestRow`). The platform's ≈340 rest-polymorphic signatures move to this form.

## Known changes of behaviour, as the conversion goes

Recorded as met; behaviour regressions are accepted where the speed is the point (2026-10-15).

- `Logic.msplit` steps the search (the kernel's `Step`): its rest is the worklist of the search's `k`s, one value —
  split again at the same place it goes on from where it was (linear, as LogicT; the first conversion re-ran
  pending alternatives through a reflection layer per answer: `observe` over a binary tree grew ~8x a level).
  Run anywhere else its operations are performed at the place of the same effect, read by paths
  (`Stepping.relocate`); an effect of it missing at the new place throws — the union routed it.
- `Choose.all`, `msplit`: a forked branch point is abandoned through the kernel's `recover`, which an abort or a
  cancel passes (`Exit`); okay-freer's catch frame abandoned it for those too.
- `Maybe.option` captures `k` for `Some` (a `Deep`), where okay-freer answered in place: the same result, a capture
  more.
- Writer's streams (`uncons`, `foldUntil`, the iterators, Generate's instances) step the program (`Step`): after
  an element is handed over, the producer's next code runs at the next pull, not at the handover; a rest handed out
  is re-placed by reading paths, an effect missing at the new place throws. The effectful `uncons`, `Pull.toldIn`
  and the stream instances take `Writer % W + G` with `G` one effect.
- Writer's folds are the kernel's `Stateful` (answered in place); the `Fold.OfLong`/`OfInt` dispatch is gone.
- `Resource` is a kernel `Deep` handler whose rest runs under a guard (release once: at the end, on an error with
  a failing release suppressed on it, on an abort or a cancel as the finalizer). The guard is told of each operation
  passing out of it to a handler taking the rest (okay-core's `Guard.passing`, 2026-10-15), so the two cases a guard
  alone missed are okay-freer's again: a `Final` operation (`Maybe`'s `None`, `Chronicle`'s halt, a choice of none)
  releases before it goes; a `Choose` goes out with its branch point (`Forked`), the resource shared by the branches
  and released after the last, or when the rest is abandoned (logic-cut-releases; `TestSharedResource`,
  `TestResourceDropped`). As on okay-freer, a multi-shot handler of another effect is taken as resuming once (released
  at the end of its first resumption), and a handler that drops the rest of an operation that normally resumes does
  not release.
- `Resource.bracket` acquires when the program runs: a program run twice acquires twice.
- `Gen`'s `zip` steps both sides when the zipped program runs, not when `zip` is called.
- `Once.memo` calls its step twice an operation (the kernel's `Stateful` asks `value` and `next` apart): pure, so the
  same.
- okay-sql's `Tx` (the indexed tree `Idle -> Open -> Idle`, `TxOp`, `Conn[S]`) is okay-tx's brackets over the
  driver: `Tx.atomic` holds its body, a statement reads the bracket's connection, `Tx.interpret(db)` runs them, in
  the kernel's words (`Eff`, rows), since okay-tx's effect names its outer row. A failure inside now ROLLS BACK (the
  indexed tree dropped the commit and left the transaction open); a retryable `sqlState` re-runs the bracket within
  `retries`. A statement outside any transaction is not written with `Tx` (it is the driver's own). `Typed.transact`
  and `region` are unchanged; their `G` is `G[_]`, and `Async` passed as `G` is a second `Async` place now.
- okay-py's dialogue (`Fn.calling`, `ProgramOf.calling`) answers `A ! F + ForeignEval` as before, but the row no
  longer collapses: callbacks in `ForeignEval` itself give `ForeignEval + ForeignEval` (`.at[ForeignEval]` to say
  `A ! ForeignEval`), and callbacks of `pure(x)` alone name their row (`pure[X, Pure]`; okay-freer's union took
  `Nothing + ForeignEval` as `ForeignEval`). `Foreign.releasing` is a kernel `Stateful` handler of `Holding` (was
  `HandleFrames.stateRun`); `Callback`, `Callbacks`, `Jvm.calls` take `F[_]`.
- A dialogue is okay-workflow's `Wf` alone: the plain door is `Wf.Dialogue.Asks` (`w.pause`), `Wf.Dialogue.pause`,
  `start`, `replay(body)(journal)`, `drive`; the bare question `Wf.ask(q)` (okay-agent's `Stepper` uses it and
  `drive`). okay-persist's `Dialogue.Asks`/`pause`/`replay` (its local copy) are gone: a plain durable dialogue's
  body is `Wf.Dialogue.Asks[Q, A, R, F] ?=> R ! Wf.Talking[Q, A] + F`. The source position is back as okay-freer's
  `At` was: `Wf.At` (`File.scala:line`, a macro given at the call site) is taken by every door (`pause`, `perform`,
  `now`, ..., `Proc.program`, okay-foreign-workflow's `Call.apply`), carried by `Talk.Say(q, where)` and
  `Wf.Dialogue.Ask(q, resume, fail, at)` (a fourth field) and read by `Dialogue#where`, which fills persist's
  `Diagnosis.where`, `Retire.State.Asking.where` and the worker's `Statuses` row again.

- TWO OF ONE EFFECT ARE TWO PLACES, everywhere: okay-freer's union merged `Async + Async`, `State % H + State % H`,
  `Tables + Tables` into one; on the kernel they stay two and a caller folds them with `.at[...]` (`Provider.relay`,
  okay-py's dialogue rows, okay-sql's `transact` with `G = Async` — pass `Pure`). The interpreters that answer into an
  effect the program already has take it from the program (`translateInto`): `Structured.viaTables`,
  `Sort.viaTables`, `Streamed.viaTables` (their `Tables`), `FlowBulk.streamed`, `SparkFrames.structured`,
  `SparkBulk.sort`, `Tables.viaInto` (their heap) require it among the effects. `Tables.via` takes `Tables` wherever
  it is (`Split`) and adds a heap of its own; `viaInto` is the same translation into a heap the program has.
- No compile-time replay discipline: `Wf.replay`, persist's durable dialogues and okay-agent's Stepper no longer refuse
  a row holding a non-replayable effect (okay-freer's `Replayable` read the union); a body with `Async` in its row
  re-performs it on replay.
- okay-zio: a row of ZIO members has the union of the members' errors (`DbErr | LogErr`), not an inferred parent.
- okay-stream: `Chunks.dropWhile` keeps a chunk whose first element already fails the predicate (it dropped it);
  `Push`'s final await pauses as `Waiting` first and is `Done` at the next call.
- okay-workflow/okay-persist: a pause carries its source position again (`Wf.At`, as okay-freer's `At`); the plain
  dialogue door is `Wf.Dialogue` (persist's own copy gone), `Wf.Dialogue.Ask` has a fourth field, `at`.
- okay-sql's `Tx` is okay-tx's bracket: a failure rolls back (the indexed interpreter left the transaction open),
  a retryable `sqlState` re-runs; the indexed `Idle`/`Open`/`begin`/`commit` are gone.

## Two packages, not a flag

`okay.freer` is FROZEN: the classic road as it is, its own JMH lanes, nothing in it changed. The kernel's words
come from the root, `okay.*` — `export Effects.machine.{given, *}`, the interface's instance on the one monad —
and a module moves by changing what it imports: `import okay.freer.*` and `.given` (555 and 548 files) become
`import okay.*`; the few named imports (`{!, pure}`, `{+, %}`, `{!, Shift}`) the same. The classic road lives
until the last module has left it, then stays for the lanes' comparison alone, or goes with them.

Regressions of behaviour are the module's own suite: red on the kernel's words, green on the classic's, is the
list. Nothing more elaborate: a module is switched when its suite is green and its lanes are not worse (below).

## `Effects[M]`: kept, the words its members

The interface over encodings stays, and the kernel's words are its members, so every word a program is written in
names one encoding: the one monad with no import, any other by its instance — an analyser that runs nothing
(counts operations, proves a program never touches `Payments`), a selective (`Static`: the effects known before
the run), the reference (`TestReference`'s meaning as an `Effects[Norm]`), a journal, the protocol's `OnCore`.
Code over `Effects[M]` — `direct`, streams, `okay-tx`, the std's handlers — runs on each.

What was added for that (okay-core 291dd33): `A ! Es` in the interface is the facade's — `(c: Needs[Es]) ?=>
M[c.R, A]`, a program NEEDING its effects, fitted to the row of its place; `Under`, `effect`/`op.perform` with
`Path` (the place first), `handle(h)(body)` at the head of the row outside, `run` at the empty row. The row-named
primitive is `handleAt(h)(m)(using Removed)`. `Needs`, `Paths`, `Place` are machinery of rows, not of `Eff`, so the
interface takes them as they are. An analysing instance names its own `Place`: a program is fitted to the row it
proposes.

## `+`: the sum of effects, and only that

`A ! F + G` is `Both[F, G]`: what the program needs, in any order, among any others. The list a row is, `E +: T`,
is written by handler authors (`T`, the row outside) and in `at[R]` — never in a program's type. The root's other
`+` — `Pure + E1 + E2`, a row written left to right (`RowEffects.+`) — is gone: it was one name for two kinds, and
a program wrote a concrete row where it should write its needs. (The platform's `Pure + Pure` in okay-persist and
TestDistinct is okay-freer's union, untouched until those modules move.) In the kernel the row-as-a-set (`F + G`
in `!`) and the row-as-a-list (`+:` at a handler) are one thing seen from two sides; `Needs` takes the first to the
second at the place.

## Measuring: Freer against Cont

Three levels, the tools in place:

1. **One program, two artifacts — okay-bench.** The platform's mixed program (the one every lane runs) written
   once in the platform's words, built twice (`-Dokay.freer.road=classic|core`), published as two versions; two
   lanes in okay-bench, each on its own classpath, one table with `core` (223 µs), `one` (169), `java` (103) and
   `zero` (137) as the bars. The answer to "faster or not" on a program, not a microbenchmark.
2. **The same microbenchmarks, two roads — okay-std, okay2.** The ~60 lanes of `okay-std/src/jmh` (`handle*`,
   `relay*`, `state*`, `writer*`, `shift*`/`cont`/`delim*`, `step*`, `stateLex*`, `fused*`/`nested*`) and okay2's
   (`HandlerBenchmark`, `HandlerFormsBenchmark`, `DelimBenchmark`) are not rewritten: they compile against the
   words each is on. Interleaved A/B (classic, core, classic, core), one JDK, `-prof gc`, the JMH lock waited
   for — as the carrier was measured against okay-cont (within noise; `delimDollarResume` −20%, `handleCapture`
   +9%). Expected by group: `handle*`/`relay*`/`stepOneByOne` down (no `resume` loop, no `TypeableK` a level, no
   `HandleFrames`); `fib*`/`tailcall*`/`buildOnly` even (the same `Cont`); `*Cps`/`delim*`/`shift*` the kernel's
   capture (×2–3 against zero, the kernel's own sprint `capture-fixed-cost`); `stateLex*`/`fused*` an open
   question — okay-freer's fusion has no counterpart in the kernel, a loss there is the honest list of what the
   kernel still needs.
3. **Why — the profile and the inline tree.** Where a Δ is outside the noise: `-XX:+LogCompilation` (eval's inline
   tree against its budget, okay-core's AGENTS) and async-profiler, as for the 136 → 222 regression; okay-platform's
   `AsyncDriveBenchmark`/`ParBenchmark` for the stepping driver, where one driver replaces `AsyncCont` and the
   stream's twin.

Acceptance of step 4: the platform's lane in okay-bench no worse than `core`; the microbenchmarks no worse than
classic in sum; every loss beyond the noise either closed or named in okay-core's sprint.d.

## What the platform gets

A program's `k` is the machine's `Rest`: stepping (`Step`) for the async driver — `AsyncCont`, the stream's machine
twin and okay-async's drive become one driver; multi-shot `k`; `k.failed` (a handler answering with a failure the
program's own `recover` sees); guards, finalizers, cancellation, `uncancelable` regions; interceptors (`Sensitive`,
`Audited`, `observe`); `okay-tx` as a word over any program — `Typed.transact` in okay-sql brackets by construction
with no change to okay-sql; the protocol (okay-kernel) for every program, through `OnCore`.

## Order

1. The root's words the kernel's (done: `RowEffects`' `!` with `Needs`, `+` the sum, the root's test on them);
   the four handler forms mapped as modules need them. A module moves by its imports; its compiler lists the
   places holding a tree as a value.
2. Those places, each the smallest change (`at[R]`, `Place`); the suites green on the core road.
3. The walkers, one file with its tests at a time: `Cps`, `Delimited`/`Shift`, `Indexed`, `Static`, `Lexical`.
4. The lanes; `Head` in the kernel if the stepping shows in them.
5. The classic road removed: `Freer`, `Classic`, `HandleFrames`, `ShiftMachine`, `Bang`, `Eager`,
   the carrier, `Row.at`, `TypeableK`, `Distinct` — to git's history.

## Decided (2026-10-15)

- `direct` (okay-direct): a compile-time macro rewriting straight-line code into `flatMap` chains over `!`; no
  machinery of its own at run time. With the words swapped it emits the kernel's `flatMap`: no change, its suite
  checks it at step 2.
- The platform's own fitting: there is none left — `Reach` went with the OnOkay bridge (2026-10-14). The fitting
  is the kernel's `Needs`.
- Scala 2.13 (`scala2/okay-scala2-*`, on `okay.freer`): the same aliases over `okay.core.Ops` (the kernel's words
  with no inline; okay-core's `scala2/prelude`), a step of its own inside step 2, checked by the 2.13 compiler.
