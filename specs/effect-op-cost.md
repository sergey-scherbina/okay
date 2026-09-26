# effect-op-cost — one operation without a fresh node

## Overview

Every `State.get` expands to `effect(Get())`: a fresh `Get` and a fresh
`Inject` around it, 32 B, on every call — although neither carries any
data. `Reader.ask` is the same shape. These are the operations effectful
code performs most, so their price is paid everywhere.

## Results so far

`ProbeOpCost` (exact bytes a level, `ThreadMXBean`; time rough, no JMH),
a mutual recursion performing one operation a level:

| operation | built fresh (shipping) | one shared node |
|---|---:|---:|
| `State.get` | 80 B, ~11.0 ns | 48 B, ~5.6 ns |
| `Reader.ask` | 96 B, ~11.6 ns | 64 B, ~9.9 ns |
| `!.tailcall`, no operation | 40 B, ~2.9 ns | |

## Decisions

- **D1. `State.get` and `Reader.ask` return ONE shared node**
  (`SharedOps.getNode`, `SharedOps.askNode`: an `Inject` of the one `Get()`
  / `Ask()`), cast to the caller's `S` / `R`. The cast is the whole of
  the change and is isolated in the two accessors, with the reason
  beside it: the operation has no fields, so after erasure `Get[Int]`
  and `Get[Any]` are the same object, and a program node is an
  immutable value that any number of programs may share (the handlers
  only read it). A fresh node would buy nothing but the allocation.
- **D2. The `Direct.staged` macro reads a shared node as its operation.**
  It recognises an operation by the `Free.Inject(op)` call after
  inlining, and a shared node is a reference to a val; a small table
  in DirectRow maps each shared node to the operation it holds
  (`State.getNode` → `State.Get()`, `Reader.askNode` → `Reader.Ask()`),
  so a staged block still stages them and an unrecognised program is
  still refused as before.
- Not taken: the op alone shared (`effect(GetOne)`), which saves 16 of
  the 32 B and needs the same cast.

## Behavior

- [x] `State.get[Int] eq State.get[Int]` and `Reader.ask[Int] eq
      Reader.ask[Int]` (TestState/TestReader; red on master first)
- [x] every handler answers the shared node exactly as the fresh one
      (the existing State/Reader suites, green unchanged)
- [x] `Direct.staged` stages `State.get` and `Reader.ask` as before
      (TestStaged, the stagers' tests, green unchanged)
- [x] ProbeOpCost: `State.get` 80 -> 48 B a level, `Reader.ask` 96 -> 64
- [x] JMH before/after on a get-heavy and an ask-heavy lane, and the
      handler lanes of docs/benchmarks.md §2 do not regress (Results below)

## Results (JMH, min of 3 alternating rounds)

| lane | before | after |
|---|---:|---:|
| `ReaderBenchmark.okayReaderRec`, 10 000 asks | 73.3 µs, 880 KB | **60.6 µs, 560 KB (1.21x)** |
| `StagedBenchmark.freeDirectNested`, 1 000 State+Writer ops | 14.2 µs, 164.9 KB | **13.1 µs, 152.1 KB (1.09x)** |
| `stagedDirect` (guard) | 7.76 µs, 85 368 B | 7.77 µs, 85 368 B |
| `stagedHand` (guard) | 7.90 µs, 84 568 B | 7.89 µs, 84 568 B |

Two findings on the way, both read in `StagedBenchmark`'s bytecode:

- **The node cannot live in `object State`.** A `val` with an initialiser
  made the module's initialisation impure to the inliner, and every path
  through it — a hand-staged `State.Get()`, the stagers' match on it —
  stayed a live call: `stagedHand` built a `Get` per operation. The nodes
  live in `SharedOps`.
- **The macro builds the operation with `new`, not the companion's
  `apply`.** The inliner keeps an `apply` call for its effects even when
  the inline match discards the value: a live `Get$.apply()` per staged
  block, +1 600 B a run on `stagedDirect`, until the quote said `new`.
