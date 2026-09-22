# direct-stagers — Direct.staged for the rows people write

## Overview

`Direct.staged` (specs/direct-staged.md) compiles a `direct` block's
operations to their handler's arms at compile time — 2.24x over the
same block as a Free program, parity with the hand-written `Func`
program to within 1%. It ships with ONE `Stager`: `StateWriter`, the
row every fusion number in this repository was taken on. A user whose
block reads a configuration and fails — `Reader % Env + Throws % Err`,
the commonest shape in this repository's tests after `State` — has
nothing to pass. The staging survey (2026-09-22, after the generators
lane) ranked this first among what is left to stage: the ceiling is
MEASURED, the mechanism exists, and what is missing is breadth. The
operator: "бери".

Two roads were open. A COMPOSITIONAL stager — one arm object per
effect, a product that dispatches — cannot be written in plain Scala:
the product's `inline def stage` would call the arm's `stage` through
the arm TRAIT's abstract member, and an abstract member is never
inlined (the reason `Stager` carries no method, direct-staged's
Decisions). A macro could compose arms, but every arm needs its slot
in ONE answer layout, and the layout is the design. So this lane
takes the other road: ONE class over the FULL row with one layout,
`Stager.All[E, S, W, Err, A]`, that any subrow instantiates with
`Unit`/`Nothing` in the slots it does not use — and the benchmark
prices those unused slots against `StateWriter` on the same block. If
they cost, the four single-effect stagers with minimal layouts are
what a one-effect block passes; if they do not, `All` is the
derivation and the singles are conveniences.

## Interface

```scala
// core, Staged.scala
object Stager:
  /** every effect a staged block can hold, in one layout: the
   *  environment read, the state and the log threaded, the error as
   *  the answer's Left — Reader + State + Writer + Throws, the four
   *  effects a direct block is written over when it is not Async */
  final class All[E, S, W, Err, A] extends Stager[Reader % E + State % S + Writer % W + Throws % Err, All.Answer[E, S, W, Err, A]]:
    inline def stage[X](inline op: Row[X]): Handled[Row, R, X]
    def run(env: E, s: S)(p: Handled[Row, R, A]): ((S, Vector[W]), Either[Err, A])
  object All:
    type Answer[E, S, W, Err, A] = E => (S, Vector[W]) => ((S, Vector[W]), Either[Err, A])

  // the singles, minimal layouts — a one-effect block pays for one effect
  final class Reading[E, A]   extends Stager[Reader % E, E => A]                       { def run(env: E)(p): A }
  final class Stateful[S, A]  extends Stager[State % S, S => (S, A)]                   { def run(s: S)(p): (S, A) }
  final class Logging[W, A]   extends Stager[Writer % W, Vector[W] => (Vector[W], A)]   { def run(p): (Vector[W], A) }
  final class Failing[Err, A] extends Stager[Throws % Err, Either[Err, A]]             { def run(p): Either[Err, A] }
```

A subrow through `All`: a block over `Reader % Env + Throws % String`
is `Direct.staged(Stager.All[Env, Unit, Nothing, String, A]()) { … }`,
run as `.run(env, ())`; the `State`/`Writer` members are in the
block's row and unreachable (nothing constructs a `Writer.Say` of
`Nothing`). `Throws % Err` in a staged block is `raise(e).!?` — the
arm drops the continuation and answers `Left(e)`; there is no
`catching` INSIDE a staged block (that is a program, and a staged
block holds operations — direct-staged's rule), the block's `run`
is the catch.

## Behavior

- [x] AGREEMENT on generated data, each stager against the SAME block
      text as a Free `direct` block run by the shipping runners:
      `All` on a four-effect block (ask, get, set, tell, and a raise
      on a data-dependent branch), and each single on its one-effect
      block — state, log, environment, answer/error identical.
- [x] `All` on a SUBROW: the `Reader + Throws` block through
      `All[Env, Unit, Nothing, Err, A]` agrees with `Reader.run` +
      `runEither` on generated data; a `State + Writer` block through
      `All[Unit, S, W, Nothing, A]` agrees with `StateWriter`.
- [x] A raise in the middle of a loop ends the block there: the state
      and log at the raise are what `run` returns beside the `Left`,
      and nothing after it ran (a counter says so).
- [x] MEASURED (okay-direct `StagedBenchmark`, minima of 2 rounds × 2
      forks, `-prof gc`): (1) the 10-op State+Writer block through
      `All[Unit, Int, String, Nothing, Int]` against `StateWriter` —
      the price of two unused slots, in µs and B/op; (2) a 10-op
      `Reader + Throws` block three ways — Free block on the shipping
      runners, `Direct.staged(All)`, hand-written `Func` — the ratio
      and the parity, as direct-staged reported them for State+Writer.
      Rows in `src/jmh/history.tsv` as `dst-*`.
- [x] Docs: direct-style.md's staged section names the five stagers
      with the subrow spelling and the `raise` rule; tutorial ch. 11's
      example gains the Reader+Throws block; typepedia entries.
- [x] Every existing direct suite and the core suite unchanged.

## Out of scope

- A stager that composes per-effect arms (the compositional road) —
  see Overview; reopen if a user row needs a FIFTH effect staged.
- `Async` in a staged block: a real suspension, not an arm.
- `catching`/`local` (handlers) inside a staged block: programs, not
  operations — direct-staged's rule stands.

## Design

- **One layout, curried:** `E => Acc => (Acc, Either[Err, A])` with
  `Acc = (S, Vector[W])` — the environment is a plain argument (never
  in the accumulator: it does not change), the accumulator is a value
  threaded through continuations (never a cell: a captured
  continuation must be re-runnable), the error is in the answer.
  `Reader.Ask` reads the argument, `State.Get/Set` and `Writer.Say`
  rebuild the tuple, `Throws` answers without calling `k`.
- **Unused slots are `Unit` and `Nothing`:** `Unit` for a state or
  environment the block never reads, `Nothing` for a log or error
  nothing constructs — the arms for those members still exist in the
  inline match and are never chosen.
- **The singles are the same arms with the tuple removed** — what
  the benchmark's first lane decides is whether that removal is worth
  a user's choice.

## Decisions

- **`All` before pairs** — chosen because fifteen row combinations
  hand-written is the boilerplate the operator asked to avoid ("the
  way Handler.flat does it"), and the measurement is the honest
  answer to whether the boilerplate buys anything. Rejected for now:
  the compositional stager (see Overview: abstract members do not
  inline, and the layout is one decision, not four).
- **Names `Reading/Stateful/Logging/Failing`, not `Reader/State/…`** —
  a class `Stager.State` would shadow `okay.State` inside `object
  Stager`, where the arms pattern-match on `State.Get()` — the E177
  the generators lane met at `Uid.Gen`.

## Results

**2026-09-22** (history rows `dst-*`; okay-direct `StagedBenchmark`,
1 000 operations, per-lane minima of 2 rounds × 2 forks, `-prof gc`).

(1) The unused slots, priced — the 10-op State+Writer block:

| lane | µs | B/op |
|---|---|---|
| `stagedDirect` — `StateWriter` | 7.64 | 85 368 |
| `stagedAllSW` — `All[Unit, Int, String, Nothing, Int]` | 7.81 | 90 184 |

+2% time, +5.6% bytes (load-proof). The tuple is the same in both;
the price is the extra `env =>` currying level every arm carries —
about 5 B per operation. VERDICT: `All` is the derivation; the
singles are conveniences with a minimal layout, kept because they
cost nothing to keep, not because a measured need called for them.

(2) The Reader+Throws block three ways (nine asks and a guarded raise
never taken, per iteration), on a quiet box (load 4.7 — a round at
load 72 read 80 µs for the Free lane and was discarded; B/op did not
move by a byte between the two, which is why B/op is the load-proof
column):

| lane | µs | B/op |
|---|---|---|
| `freeDirectRT` — the Free block under `Reader.run` + `runEither` | 11.25 | 112 896 |
| `stagedDirectRT` — `Direct.staged(All[Cfg, Unit, Nothing, String, Int])` | **4.40** | **51 288** |
| `stagedHandRT` — the same program by hand, `handBlock`'s flat shape | 4.35 | 48 888 |

**2.56x** over what the user has today, parity to 1% with the hand
program (+2 400 B per run: the hoisted vals and the guarded branch's
`pure`). Larger than State+Writer's 2.24x because a Reader arm is
one closure and no tuple.

What the lane found on the way: the FIRST hand-written parity lane —
a `def ask` returning the staged op, braced lambdas, named vals — read
9.8 µs and 89 664 B, 2.2x slower and 75% more bytes than the macro's
own output. A hand ceiling is a ceiling only in the flat shape
(`rt.stage(op)` as the direct argument of every bind); rewritten so,
it was the target. Recorded in the history row, because the next lane
that writes a ceiling will write it the comfortable way first.

Tests: `TestStagers` 9/9; TestStaged unchanged. File: Handled.scala
renamed Staged.scala (operator, 2026-09-22).
