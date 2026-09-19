# okay-docs-mongo

The foreign adapter that proves the Docs seam (specs/data.md, docs-seam): `MongoDocs` implements the same `Docs[A]` trait the own `TopicDocs` engine serves — get/put/delete with `Cond` CAS, query over declared indexes — on mongodb-driver-sync. A satellite module (JVM, the one place the Mongo dependency lives), exercised live against a dockerized Mongo with the TestLive skip where none answers.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-docs-mongo.md`](../docs/modules/okay-docs-mongo.md) | what it is, and the reasoning |
| [`specs/data.md`](../specs/data.md) | the design and its decisions |
