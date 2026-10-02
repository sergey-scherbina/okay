## okay2-shift-merge - okay2's Delim folded into Shift: one effect Shift[K], Shift[Any] the dynamic form

The twin of the core's shift-merge (stages 1, 2 and 4) in the Scala 2.13
twin (specs/okay2.md stage 52).

- **One effect.** okay2's `Delim` is `Shift[Any]`, the core's `Shift % ?`
  and the operator's "Shift % Any". There is no alias: 606 uses in 22
  files moved.
- **One object.** Every door of the old `object Delim` is a member of
  `object Shift`, beside the static form keyed by the answer type.
- **One machine guard over any key.** `NoMachine` replaces `NoDelim`.
- **`Shift.dynamic`** widens a static program into the dynamic row.
- **One name differs.** The static generator is `Shift.gather`: a
  `collect` overload would cost the dynamic `collect`'s lambdas their
  parameter type in Scala 2 (measured).

Tests: TestShift (`Shift.dynamic` mixing a keyed capture with a dynamic
scope, the guard over any key), every TestDelim suite on the new
names. Docs: docs/okay2.md §6 and §13.
