## pwc-docs-seam - Docs.query as Source[Chunk[(String, A)]]

Stage 2 of producer-to-writer-carrier, the document stores: `Docs.query`
(okay-docs) was `Chunk[(String, A)] ! (Produce + Async)` and is
`Source[Chunk[(String, A)]]`, with its counting wrapper and the four
backends — `TopicDocs`, `MongoDocs`, `DynamoDocs`, `CassandraDocs` —
retyped the same way: one `Async.Run` fetches the page, and the chunk
that used to be re-emitted as a `Produce` operation
(`effect[F, Chunk[..]](c)`) is told (`effect[F, Unit](Writer(c))`).
`DocsSuite.collect`, the shared contract suite's walker over
`Produce + Async`, is `run(Source.concat(s)).toList`. No other caller
named the old shape; no module doc did.

Files: okay-docs Docs.scala, TopicDocs.scala; okay-docs-mongo
MongoDocs.scala; okay-docs-dynamo DynamoDocs.scala; okay-docs-cassandra
CassandraDocs.scala; okay-docs DocsSuite.scala; the arc's spec and
sprint checklists.
