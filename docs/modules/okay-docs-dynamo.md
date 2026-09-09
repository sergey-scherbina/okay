# okay-docs-dynamo

The second foreign adapter of the Docs seam (specs/data.md,
docs-dynamo): `DynamoDocs` implements the same `Docs[A]` trait the
own `TopicDocs` engine and `MongoDocs` serve — get/put/delete with
`Cond` CAS, query over declared indexes — on DynamoDB, and it is the
engine the seam was designed around and had never met: condition
expressions and eventual reads.

No AWS SDK: the adapter speaks DynamoDB's JSON protocol over the one
http client, signed by okay-blob's SigV4 with service `dynamodb`. The
document is CBOR under `d`, the version a number under `ver` advanced
by `ADD ver :one`, and every conditional write is ONE UpdateItem or
DeleteItem carrying a condition expression (`attribute_not_exists(id)`,
`ver = :ver`); a ConditionalCheckFailedException answers `Stale` with
the current version. Declared index fields are `ix_<field>` attributes
and global secondary indexes the query walks. `grants` names the two
read modes DynamoDB has: `One` is an eventually consistent read,
`Quorum` and `Strong` are `ConsistentRead`. JVM, live against a
dockerized dynamodb-local with the TestLive skip where none answers.
See docs/modules/okay-docs.md for the seam itself.
