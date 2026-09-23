## scala2-stm - okay-stm from Scala 2.13: Tx and Stm.atomically

Stage 15.3 of specs/scala2-facade.md (operator: "делай всё что возможно
чтобы работало в скале 2").

- `TRef` is in core and readable from scalac 2.13, so Scala 2 uses it
  directly (`Stm.ref(init)` is the same as `TRef(init)`).
- The new module okay-scala2-stm adds `Tx`, the transaction language as
  a capability of `Eff` (`read`, `write`, `modify`, `update`, `retry`,
  `check`, `orElse`), and `Stm.atomically: Eff[Tx, A] => Eff[Async, A]`
  through the platform's own strategy (TL2 on the JVM). I/O inside a
  transaction is a type error, as in Scala 3, and a `compileErrors`
  test pins that.
- `TestStmFromScala2` has 5 tests: an atomic transfer, 1000 increments
  from 8 fibers with none lost, a retry woken by another fiber's write,
  `orElse` discarding the first branch's writes, and the I/O refusal.
- `scripts/board.sh --check` now refuses a slug filed in two boards.
  Until now only `TestBoardEntries` did, so a claim that copied its
  sprint item instead of `git mv`-ing it (80a1ccec) passed the check the
  claim runs and turned every lane's gate red. The stale copy was
  removed on master in e92ba0b1.
- Docs: section 8l of docs/scala2.md (copied from the probe, with the
  Harris et al. and TL2 references), the module page, the API
  reference, and the spec's stage 15.3.
