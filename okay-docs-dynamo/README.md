# okay-docs-dynamo

The second foreign adapter of the Docs seam (specs/data.md, docs-dynamo): `DynamoDocs` implements the same `Docs[A]` trait the own `TopicDocs` engine and `MongoDocs` serve — get/put/delete with `Cond` CAS, query over declared indexes — on DynamoDB, and it is the engine the seam was designed around and had never met: condition expressions and eventual reads.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-docs-dynamo.md`](../docs/modules/okay-docs-dynamo.md) | what it is, and the reasoning |
| [`specs/data.md`](../specs/data.md) | the design and its decisions |
