## direct-macros-shared-syntax - the direct macros read one mark syntax

The operator asked for the direct macros to be cleaned up. The spec had
refused "one IR, four back ends" with evidence, and re-examining it gave
the same answer: the monadic road nests a continuation per statement and
the arrow road appends to an environment. What the roads truly SHARE was
duplicated, though: the arrow road (`ProcMacro`) had its own copy of the
mark spellings, the colouring dispatch, `strip`, `calleeRoot`, `asMark`
and `hasMark`.

- okay-direct `macros.MarkSyntax`: that syntax, once, with the colouring
  conversions as its one abstract member. `DirectPhase` extends it,
  `DirectMarks` names `direct`'s colourings, and `ProcMacro` makes an
  instance at its own Quotes (`q: outer.type`, so the path-dependent
  `Term`s coincide) with `procColor`.
- Before: a mark spelling added to `Direct` was silently not a mark in
  `Proc.direct`. After: one list.
- Gate: 5580 tests across the affected modules, all three platforms,
  no warnings. Mutant: removing `!` from the shared list broke okay-direct's
  tests (the arrow road's compile depends on them, and holds no list of
  its own anymore).
- Spec: proc-notation.md stage 2 records what is shared and why the IR
  stays refused.
