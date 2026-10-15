## tx - transactions as a subtree: okay-tx

- A module of its own, `okay-tx` (`okay.tx`, JVM/JS/Native), on the kernel's words: `Tx[C, T, X]` = `Atomic(body)`
  a bracket that holds its body, `Rollback(reason)` a leaf of no value; inside a bracket the transaction's effect
  again and the resource `C` to read (`Reader % C`). `Tx.Transactor[C, T]`: begin, commit, rollback as programs over
  the row outside, `retry(e, attempt)`. Two `Deep` handlers, `closed` and `open`: one begin and one commit however
  deep the brackets; a rollback or a failure inside rolls back and fails the rest where the bracket was (the kernel's
  `k.failed`, okay-core 9aa5513); an abort or a cancel crossing rolls back. The words `atomic`, `rollback`,
  `transactions`.
- The JVM's own: `Jdbc[T]`, a transactor over a JDBC connection — a pool's `connect`, auto-commit off at the
  isolation, the driver's commit and rollback, the connection closed after either, 40001/40P01 run the bracket again;
  `Jdbc.execute`, `Jdbc.queryInt`.
- Tests: `TestTx` 7, `TestTxWords` 1, `TestJdbc` 4 on H2. Came from okay-core's `tx/` (228f6fc), moved here the same
  day: a transaction is the platform's, not the kernel's.
- okay-core pinned at 9aa5513 (`k.failed`; the kernel's `tx/` gone).
