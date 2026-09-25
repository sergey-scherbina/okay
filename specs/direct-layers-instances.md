# direct-layers-instances — direct blocks over Layered layers and Lexical instances

## Overview

The operator asked (2026-09-25) for `direct` blocks that mix monads
through `Layered` and use effect instances through `Lexical`. Stage 0
probed what already works, before anything was built
(ProbeDirectLayersInstances):

- WORKS: a `Lexical` instance's operations take the mark (`s.get.?`),
  two instances of one effect in one block, the `tail` default on the
  pure row, `Layered.reflect(...).?`, and two layers in one block. They
  are programs, and a program block's mark already reflects programs.
- GAP 1: the mark on the foreign monad's value itself, `List(1, 2).?`,
  with its layer in scope, is refused: "neither this block's program
  nor an operation of its row".
- GAP 2: `s.set(v).?` as a statement is a warning ("unused value of type
  Int"), because `set` answers the new state, and a bare `s.set(v)` is
  the discarded-program error. `Writer` has `tell` for exactly this
  shape. State instances have nothing like it.

## Design

- GAP 1: `markTerm` (okay-direct, DirectRow.scala) gets one more branch.
  A marked value `M[X]` that is not a program, when a
  `Layered.Reflect[M, R]` is found in the implicit scope at the mark,
  becomes `Layered.reflect(m)[R, Pure]` (a `X ! Delim`), which the
  existing `narrowRow` widens to the block's row. That puts `Row`'s
  membership proof, not a cast, under it. A block whose row lacks
  `Delim` is refused, naming `Delim` as what a layer needs. The same
  single mark `.?` applies: the type decides, as it already does between
  a program and an operation.
- GAP 2: `Lexical.State`'s `put(s): Unit ! G`, the statement form of
  `set`, as `tell` is for Writer.
- Stacked (`Delim.Stacked`, `Lexical.Stacked`, `Layered.Stacked`) blocks
  are `Prog`, which `direct` does not walk: out of scope here.

## Behavior

- [x] Stage 1: `List(1, 2, 3).?` inside `reify[List, …]` and a `direct`
      block reflects to its layer, and so do two layers' values in one
      block (List and Option), with the same answers as `.reflect(...).?`.
- [x] Stage 1: in a block whose row has no `Delim`, the mark on a
      foreign monad's value is refused, naming `Delim`.
- [x] Stage 1: without a layer in scope, the refusal stays as it was.
- [x] Stage 1: `s.put(v).?` is a clean statement.

## Decisions

- **The layer is found through the value's base classes, most specific
  first.** The first cut matched only an applied type `M[X]`, and an `if`
  of `None` and `Some(…)` failed. Its type is the union `None |
  Some[Int]`, not `Option[Int]`. The macro now walks `baseClasses`, takes
  each one-parameter base type, and uses the first with a
  `Layered.Reflect` in scope. `Some[Int]` and the union reach `Option`,
  and `List[Int]` reaches `List`.
- **No cast from the macro.** The emitted term is the ordinary
  `Layered.reflect(m)[R, Pure]` call with the found capability. Widening
  its `Delim` row to the block's row is `narrowRow`'s job, which asks the
  compiler for Row's membership proof.

## Results

STAGE 0, 2026-09-25: the probe above. Five of six shapes worked with no
change, which is the reason the stage-1 list is short.

STAGE 1, 2026-09-25 (TestDirectLayersInstances 8; okayDirectJVM/test 420 green):
instances in blocks (put as the statement form), the mark on a List and
on an Option-typed `if` inside their layers, two layers equal to the
explicit-reflect spelling, a row without Delim refused by name, and no
layer refused as before. The stage-0 probe was the watched failure: the
mark on `List(1, 2, 3)` was refused before the branch existed.
