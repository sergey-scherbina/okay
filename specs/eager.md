# Eager — the opt-in kyo-style encoding

## Overview
A third Effects encoding beside Free (initial) and Eff (final): the
kyo trick, opted into explicitly. A pure computation IS its value
(an unboxed union `A | (A ! F)`), so flatMap on a pure value applies
the function AT CONSTRUCTION — runs of pure binds cost plain function
calls, no tree, no interpretation. The user chooses the contract:
Free/Eff keep the laziness guarantee (programs are values; infinite
programs construct in O(1)); Eager buys kyo-class speed on bind-heavy
pure code and pays with kyo's hazards, stated plainly.

## Interface
- `type Eager[F[+_], A] = A | (A ! F)` + `given Effects[Eager]` — the
  whole tagless surface (pure/perform/flatMap/map/foldCont/runWith)
  applies; `toFree` normalizes into the tree world at any point.

## The stated hazards (kyo's, now ours by choice)
- construction EVALUATES: a self-referential program
  (`def forever = pure(()).flatMap(_ => forever)`) diverges before
  ever being run — exactly what compare/TestLaziness catches kyo on
- values must not themselves be effect trees (the unboxed union is
  discriminated by a runtime class test on Free — kyo's Flat
  constraint, here as a documented rule rather than an evidence)

## Behavior
- [x] prog[Eager] agrees with prog[Free] on the tagless test programs
- [x] eagerness is real: a pure bind chain is fully evaluated at
      construction (probe counter), running is O(1)
- [x] benchmark lane okayEager: measured 5.1us vs kyo 50.4 — TEN times
      under (no kernel at all: pure binds are plain function calls)
- [x] the laziness contrast is documented in README next to the table

## Out of scope
- a Flat-style evidence (documented rule first; evidence if misuse
  actually bites); Eager streams (the laziness contract is what our
  stream layer is built on — Eager is for bind-heavy computation)

## Decisions

- **`fold`'s two casts stay centralized, but the function goes
  `inline`** (eager-dispatch-regression, 2026-09-02): `casts-
  encapsulated` (d6feb48) centralized the encoding's two casts into
  one `private def fold(m)(value, tree)`, replacing each operation's
  own hand-written match. Measured cost on the pure-bind hot path
  (the whole point of Eager): 5.1 -> 17.6us, 3.45x (docs/
  benchmarks.md §1). Two compounding costs, both from `fold` being
  an ordinary method taking ordinary `Function1` arguments: every
  call built a closure for BOTH branches (arguments are evaluated
  before `fold` is entered, so the branch not taken still allocates)
  and dispatched through a virtual `.apply()` instead of an inlined
  match arm. `flatMap` additionally wrapped `f` in a redundant
  `a => f(a)` on top. Fix: `fold` becomes `private inline def` with
  `inline value`/`inline tree` parameters — the two casts stay
  textually in the one function (the goal survives), but each call
  site's argument EXPRESSIONS are substituted directly into the
  match arms at compile time, so only the arm actually taken builds
  anything, and `flatMap`'s pure branch compiles to exactly what the
  pre-refactor hand-written match did. Gate: TestEager unchanged
  green; the benchmark is the receipt.
