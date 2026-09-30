- [ ] indexed-effects-2-tx-data — stage 2 of specs/indexed-effects.md:
      okay-sql's transaction protocol as an indexed DATA signature
      `TxOp[S, R, +X]` (`Begin: Idle -> Open`, `Commit`/`Rollback`:
      `Open -> Idle`, statements at any index), `Tx.Data[A, S, R]`, and
      `Tx.interpret` threading a `Conn[S]` typed by the index — `Commit`
      from a `Conn[Idle]` does not type even inside the handler. The
      `Prog` facade `Tx` stays beside it. Tests: TestTx's shapes on the
      data road, the Throws caveat, a `compileErrors` pin on the handler
      arm. Additive gate.
