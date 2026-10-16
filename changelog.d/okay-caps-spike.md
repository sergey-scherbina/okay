## okay-caps-spike - tracked capabilities measured: a transaction that cannot leave its bracket

- New module okay-caps (JVM): `Tx`, a transaction as a capability (`caps.SharedCapability`) —
  `Tx.transaction(db)(tx => ...)`, `Tx(db) { Tx.execute(...) }`; a capture-checked caller cannot carry it out
  of its bracket. Tests run the compiler on snippets (capture checking comes after the typer).
- Measured on Scala 3.9.0: no `-experimental`, no infection of dependents; refused — the transaction returned, a
  closure, a var, a list of closures, an Iterator; not refused, pinned as such — a caller without the checker, a
  LazyList, a kernel program (okay-core not capture-checked). specs/okay-caps.md, docs/modules/okay-caps.md, backlog
  okay-caps-next.
