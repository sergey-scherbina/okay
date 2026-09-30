- [ ] indexed-effects-8-prog-facade — stage 8 of specs/indexed-effects.md:
      the `Prog` facade removed. Its consumers: okay-sql's facade `Tx`
      (class over `Sql`, `Prog.transition` in its smart constructors) and
      `Tx.run`, TestTx, TestProg's facade tests (the stacked shapes stay),
      docs/guide.md "Typestate on a program" (pinned lines), docs/
      typestate.md's mention. `Tx.Data` becomes THE `Tx`: `Tx.begin/
      commit/rollback/update/batch/describe/async` and `Tx.interpret`;
      the guide's section rewritten over it with its lines pinned by
      TestTxData. Prog.scala deleted; ProbeFreerStep keeps its own
      `Prog` (a probe). Changes existing API: full `affected master
      staged`.
