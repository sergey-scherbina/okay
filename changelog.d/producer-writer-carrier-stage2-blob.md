## producer-writer-carrier-stage2-blob - Blob moves to the writer carrier

`specs/producer-to-writer-carrier.md` stage 2, module 1 of 6 (leaves
first): `Blob.get`/`put`/`list` retyped from `Produce` to the writer
carrier — `get: Either[String, Unit] ! (Writer % Chunk[Byte] + Async)`,
`put(key, bytes: Source[Chunk[Byte]])`, `list: Source[Chunk[Meta]]`.
`getSource`/`putSource` are gone: `get`/`put` already answer exactly
what they used to bridge to, so there is nothing left for them to be
an alternative TO.

Every implementation (`Fs`, `S3`, `Blob.Counted`) and consumer
(`Backup`, `Offload`) had its `Producer.each`/`Producer.concat` call
sites become `Writer.fold`/`Writer.collect`. One of these is a real
simplification, not a rename: `S3.get`'s response body was ALREADY a
`Source[Chunk[Byte]]` from okay-http, so the old code paid exactly the
bridge tax stage 0 measured (65-90% over a direct fold) walking it
chunk by chunk back into a `Produce` row it had to answer in. It is
now `Writer.expand(src)(c => if c.isEmpty then IndexedSeq.empty else
IndexedSeq(c)).map(_ => Right(()))` — no bridge crossed, and the
empty-chunk filter the old walk did is the one line `expand` exists
for.

Blast radius, checked before touching anything: `okay-ops`,
`okay-demo`, `okay-docs-dynamo`, `okay-acme` depend on `okay-blob` but
only for `Blob.Stats` or `SigV4`, untouched by this. `okay-watch`'s own
`Producer.each` restore path (the backlog entry this arc's stage 0
cited) is a private repo, not reachable from here.

`E092`'s TypeableK caveat (Writer's split test is unchecked under
erasure for a parameterized `W` — sound by construction, since `Say`
is Writer's only constructor) shows up at every new `Writer.fold`/
`.collect` call site here; `@nowarn("msg=cannot be checked at
runtime")` at each, matching the same pattern stage 0's benchmark
already used. If later modules keep accumulating these one by one, a
scoped `-Wconf` entry might be worth proposing instead — not decided
in this lane.

Gate: `okayBlobJVM/test` 21/21 green (`TestLiveS3` skips without a
live MinIO, as it always has); all three platforms and every
dependent module compiled cold with zero warnings.

`sprint.d/queue/producer-to-writer-carrier.md` names okay-cluster as
the next module.
