# okay-docs

The document seam: get/put/delete by key with compare-and-set as DATA, bounded queries over declared secondary indexes, per-item atomicity — the one new seam specs/data.md allows itself.

**Depends on:** the core, `okay-codec`, `okay-persist`. Cross-built; on the JVM also the foreign engines `MongoDocs`, `DynamoDocs`, `CassandraDocs` (`okay-blob`, `okay-http`; mongodb-driver-sync and the Cassandra java driver are `Provided` — a program that builds a `MongoDocs` or `CassandraDocs` adds its driver).

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-docs.md`](../docs/modules/okay-docs.md) | what it is, and the reasoning |
| [`specs/data.md`](../specs/data.md) | the design and its decisions |
