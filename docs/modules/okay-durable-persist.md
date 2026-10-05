# okay-durable-persist

Topic-backed storage for [okay-durable](okay-durable.md). JVM and Scala.js;
depends on okay-durable and [okay-persist](okay-persist.md).

## Journal

`okay.durable.persist.TopicJournal(topic, run)` implements `Durable.Journal`.
The run key selects a partition. Intent and completion are separate records
written with `Ack.Durable`; reading refolds that run's records. A new
journal instance over the same topic recovers the same history.

The existing version-1 Typed envelope and Intent/Complete CBOR cases are
unchanged. Fixed-byte tests check both reading old records and writing the
same bytes. The underlying Store determines persistence across restart;
MemoryStore remains memory-only. A damaged undecodable record stops the
fold, retaining the existing behavior.

`okay.agent.TopicJournal` and its Rec companion names remain available as
source-compatible aliases. Rebuild downstream applications when upgrading.
The adapter adds no worker scheduling or ownership guarantee; those
contracts belong to the chosen runtime and underlying store.
