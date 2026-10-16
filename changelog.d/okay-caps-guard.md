## okay-caps-guard - what the checker misses fails loud; the checker module-wide; the rule

- `Tx` is closed when its bracket ends (commit or rollback); a statement after it throws `Tx.Closed` — the leaks
  capture checking does not refuse (a caller without it, a LazyList, a kernel program) fail at the leak.
- Measured: `-language:experimental.captureChecking` for a whole module refuses what the import refuses.
- The rule in docs/modules/okay-caps.md and docs/capabilities.md: keep a capability an effect (its row stops it
  leaving the bracket), or a value tracked by the checker, with the guard behind it. TestTxCapability 6.
