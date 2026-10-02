## okay2-shift-merge-guard - okay2's one machine guard, Shift.Machine[F]: every machine-starting door nests

The core's shift-merge-guard in the Scala 2.13 twin (specs/okay2.md
stage 53).

- **One guard.** `Shift.Machine[F]`, read off the row by a blackbox
  macro, replaces `OneMachine`, `NoMachine` and `Nesting`.
- **Every door that runs a machine nests on one already running** in its
  row, instead of refusing at compile time. This covers `run`,
  `delimited`, `collect`, `resumable`, `drive`, `answer`, `replay` and
  the keyed `reset`.
- **An abstract row is a compile error** that names the fix:
  `(implicit m: Shift.Machine[F])`.

Tests: TestShift (the guard reads the row, a door nests, the abstract
row's error), TestDelim (the old refusal now nests and answers).
Docs: docs/okay2.md §6 and §13.
