# okay-docs-cassandra

The third foreign adapter of the Docs seam (specs/data.md,
docs-cassandra): `CassandraDocs` implements the same `Docs[A]` trait
`TopicDocs`, `MongoDocs` and `DynamoDocs` serve — get/put/delete with
`Cond` CAS, query over declared indexes — on Cassandra through the
Apache java driver (the vendor driver lives in this satellite, the
Mongo precedent). It is the engine where consistency is a dial: a
`Quorum` request is granted a quorum, `Strong` is ALL, `One` is one —
nothing upgraded, nothing pretended.

Every conditional write is ONE lightweight transaction — `INSERT … IF
NOT EXISTS`, `UPDATE … IF ver = ?`, `DELETE … IF ver = ?` — whose
`[applied]` row carries the CURRENT version when it refused, so `Stale`
answers with what holds now. `Cond.Always` is a bounded read-then-CAS
loop (no atomic increment on a regular column). The document is CBOR
under `d`, the version a bigint under `ver`; declared index fields are
`ix_<field>` columns with a secondary index each. JVM, live against a
dockerized cassandra:5 with the TestLive skip where none answers. See
docs/modules/okay-docs.md for the seam itself.
