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
