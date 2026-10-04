## okay2-jdbc-writes - okay2-persist's log, and Writes, Poll and SqlStore on okay2

Last lane of okay2-jdbc-tails (operator: "do everything needed for okay2,
all at once").

- `okay2-persist` (JVM, Scala.js, Scala Native): okay-persist's log only
  — `Record`, `Ack`, `Policy`, `Topic`, `Store`, the typed CBOR view with
  its version envelope and upcasts, `Offsets`, `Snapshots`, `MemoryStore`
  — with okay-persist's `StoreSuite` contract. The file and replicated
  engines, the wire, Raft and the durable workflow are not ported.
- okay2-jdbc: `Writes` (intent-first journal, `recover` by `WithKey`,
  `Reconcile` or `Fail`), `Poll` (the watermark as a consumer offset) and
  `SqlStore` (the StoreSuite contract over H2), with TestWrites, TestPoll,
  TestSqlStore and TestSqlite's crash-window case.

Spec: specs/okay2.md stage 42, "okay2-jdbc-tails, lane 2"; docs §34.
okay2-jdbc-tails is closed by this lane.
