# okay-docs-mongo

The foreign adapter that proves the Docs seam (specs/data.md,
docs-seam): `MongoDocs` implements the same `Docs[A]` trait the own
`TopicDocs` engine serves — get/put/delete with `Cond` CAS, query
over declared indexes — on mongodb-driver-sync. A satellite module
(JVM, the one place the Mongo dependency lives), exercised live
against a dockerized Mongo with the TestLive skip where none
answers.

Foreign engines keep their OWN CAS: the adapter maps the seam's
conditional writes onto Mongo's native compare-and-set rather than
reimplementing versions above it — the same honesty as the S3
engine's etags and Kafka's ops. The contract suite runs over both
engines; the seam's whole claim is that nothing above notices which
one answered. See docs/modules/okay-docs.md for the seam itself.
