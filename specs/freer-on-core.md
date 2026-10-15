# The platform on the kernel's words: `A ! F + G` resolved to a row where it is placed

Status: proposed, 2026-10-15. Follows the decision of 2026-10-14 (okay-core's docs/okay.md): the one monad
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

## Where a tree is held as a value

`A ! F` is no longer a value of a tree: it is a program fitted to a place. Code that stores a program, passes it
through a data structure, or matches its nodes needs the program AT A ROW: `p.at[R]` with the row named, or
`Place.at[R]`, giving `Eff[R, A]` — a `Cont`, a value. The compiler lists these places: switching the words and
compiling the platform gives the exact list; each is one of three kinds — a program stored (name its row), a
program matched (one of the walkers above), a program compared (`reify`/`reflect`: a round trip through `Cont`'s
nodes, which are syntax too).

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

- The platform's suites are the specification: 6937 + 184, every module, every platform, on the new words. A
  flag chooses the road while both exist: `-Dokay.freer.road=core|classic` picks which `okay.freer` the modules
  see (the classic road the current files, the core road the aliases); the flag goes when the classic road does.
- okay-std's 60 JMH lanes, A/B against the carrier road (the last run: neutral within noise against okay-cont,
  `delimDollarResume` −20%, `handleCapture` +9%). Expected: `handle*` and `dispatch*` down (no `resume` loop, no
  `TypeableK` per level, no `HandleFrames`), `pure`/`bind` even (the same `Cont`), captures as the kernel's.
- okay-bench's `core` lane (223 µs on the mixed program) is the bar the platform's own program should meet on the
  new road; the `kernel` lane (286) its program over the protocol.

## What the platform gets

A program's `k` is the machine's `Rest`: stepping (`Step`) for the async driver — `AsyncCont`, the stream's machine
twin and okay-async's drive become one driver; multi-shot `k`; `k.failed` (a handler answering with a failure the
program's own `recover` sees); guards, finalizers, cancellation, `uncancelable` regions; interceptors (`Sensitive`,
`Audited`, `observe`); `okay-tx` as a word over any program — `Typed.transact` in okay-sql brackets by construction
with no change to okay-sql; the protocol (okay-kernel) for every program, through `OnCore`.

## Order

1. `okay.freer`'s `!`, `+`, `%`, `effect`, `pure`, `flatMap`, `handle`, `run` as the kernel's, behind the flag;
   the four handler forms mapped. Compile the platform: the list of places holding a tree as a value.
2. Those places, each the smallest change (`at[R]`, `Place`); the suites green on the core road.
3. The walkers, one file with its tests at a time: `Cps`, `Delimited`/`Shift`, `Indexed`, `Static`, `Lexical`.
4. The lanes; `Head` in the kernel if the stepping shows in them.
5. The classic road removed with its flag: `Freer`, `Classic`, `HandleFrames`, `ShiftMachine`, `Bang`, `Eager`,
   the carrier, `Row.at`, `TypeableK`, `Distinct` — to git's history.

## Open

- `direct` (okay-direct) suspends a program by its own machinery over the tree; whether it stays as it is over
  `Cont` values or becomes the kernel's `Step` is decided when it is reached (step 2 lists it).
- The platform's `Needs`-like fitting (`Reach`, `Static`'s row) and the kernel's `Needs`: one or the other, not
  both; the kernel's, unless a case needs what only the macro reads.
- Scala 2.13 (`okay-scala2-*`): the kernel's words are inline; the Scala 2 modules read `okay.core.Ops` (no
  inline). The same aliases there, over `Ops`.
