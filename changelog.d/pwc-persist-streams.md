## pwc-persist-streams - Streams.stream/tail as Source[Chunk[Record]]

Stage 2 of producer-to-writer-carrier, the first G-effectful module
after the `Chunks` retype: okay-persist's `Streams.stream` and
`Streams.tail` were `Chunk[Record] ! (Produce + Async)` — "the
JdbcInterop shape", a chunk in the answer position — and are now
`Source[Chunk[Record]]`: each chunk a told value, each read one
`Async` operation, the end `pure(())` instead of an empty chunk
standing in for it. The row is `Writer % Chunk[Record] + Async`; the
emit is `effect[F, Unit](Writer(ChunkBuf.of(rs)))` where it was
`effect[F, Chunk[Record]](ChunkBuf.of(rs))`, and `tail`'s parked poll
widens `Async.sleep` over the writer row instead of `Produce`.
`Streams.chunks` (the pure partition recipe) was already on `Chunks`
and moved with the alias last lane. `Wire`/`WireProtocol`'s `Produce`
is the replication request, not the effect — untouched.

The only consumers were `TestStreams`' own hand-rolled walker over
`Produce + Async` (with a cast per chunk); it is now the writer
stream's own `iterator` with `take(n)` — no cast, and `take` stops
asking after `n`, which is what lets the endless `tail` be tested.
Three tests unchanged in what they assert, green. specs/persist.md's
two mentions of the old shape updated.

Files: okay-persist/src/main/scala/okay/persist/Streams.scala,
okay-persist/src/test/scala-jvm/okay/persist/TestStreams.scala,
specs/persist.md, specs/producer-to-writer-carrier.md,
sprint.d/queue/producer-to-writer-carrier.md.
