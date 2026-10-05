## okay2-condition-repair - okay2 has okay's Condition, and okay2-persist the Repair road over it

Operator: "Repair: it needs okay's Condition, which okay2 doesn't have.
- можешь сделать?" (can you do it?).

**`Condition` in okay2's core.** This is okay's condition system (Common
Lisp's signal, restart and policy): `signal`, `within`, `frame` with its
`Restart` handle, `raiseC` with `Answers`, `Of` with `resume`, and the
policy handler `Condition.run`. The policy chooses `Resume`, `Invoke` or
`Fail`.

Scala 2 differences:
- the row is the trait `Condition`, with operations `Condition.Op`;
- `frame`'s restart is an explicit parameter;
- the direct-style `frame` has no twin;
- `Answers.fromOf` takes `A` from a `C <:< Of[A]` evidence.

**Two changes from okay's handler, both found by a test.** A nested
frame's region is deferred (`Free.delay`), and the menu's names are lazy.
Together they make a hundred thousand nested frames run; before, it was a
host frame each and quadratic. okay's own `Condition` still has both
costs, filed as backlog `condition-nested-frames`.

**`Repair` in okay2-persist.** Each damaged record signals `Damaged`
(offset, error, raw) under a "skip" restart, so it can be patched in
place, skipped, or failed with its offset named.

**Tests:**
- TestCondition (cross): okay's TestCondition and TestConditionTyped,
  except the direct block, plus the nested-depth test.
- TestRepair (cross): okay-persist's four.

**Left on okay2-persist-rest:** only `TestWireTls`, which waits for a TLS
module.
