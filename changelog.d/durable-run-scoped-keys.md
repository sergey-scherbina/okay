## durable-run-scoped-keys — isolate external keys by journal run identity

Source commit ee8d4b69b: Journal.runId supplies stable identity;
MemoryJournal supports explicit IDs or one platform UUID per journal;
TopicJournal exposes its existing run without changing version-1 bytes.
Fresh scoped keys encode run and position rather than a 32-bit request
hash. Missing identity rejects fresh WithKey before append/execution.
Stored Entry.key remains authoritative during recovery and replay,
including trace spans and legacy incomplete entries. Agent factories
and key helpers forward the neutral API; the legacy helper is retained.

Validation: 47 targeted JVM/JS, adapter/agent and actual Scala 2 results;
rebased affected master staged GREEN, 2110 results, no compile warnings
and recursion inventory holds. Documentation describes namespace/length
requirements, legacy migration and the concurrency boundary. Both key
P1 items are closed; real process-crash/concurrency evidence stays in
backlog.d/okay-persist/durable-recovery-contract-tests.md.
