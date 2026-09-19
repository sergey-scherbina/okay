# okay-docs-cassandra

The third foreign adapter of the Docs seam (specs/data.md, docs-cassandra): `CassandraDocs` implements the same `Docs[A]` trait `TopicDocs`, `MongoDocs` and `DynamoDocs` serve — get/put/delete with `Cond` CAS, query over declared indexes — on Cassandra through the Apache java driver (the vendor driver lives in this satellite, the Mongo precedent). It is the engine where consistency is a dial: a `Quorum` request is granted a quorum, `Strong` is ALL, `One` is one — nothing upgraded, nothing pretended.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-docs-cassandra.md`](../docs/modules/okay-docs-cassandra.md) | what it is, and the reasoning |
| [`specs/data.md`](../specs/data.md) | the design and its decisions |
