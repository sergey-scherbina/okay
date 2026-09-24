## okay2-stream-async - channels and sources for the Scala 2 core

okay-stream's asynchronous layer, on okay2-async and okay2-platform:
`Cell` (one CAS cell), `Channel` with its close contract stated,
`StmChannel` (the whole state one immutable value, the batched receive
in one transaction), `Fifo` (list/array), `Drain`, `Stream[Channel,
Async]`, `drained`/`drainedChunks`, `Channel.merge`/`mergeChunked` (a
flusher that never touches the pull)/`buffer`/`bufferChunked`;
`Source` (`of`/`unfold`/`range`/`concat`, `runCollect`/`runForeach`/
`runFoldUntil`, `merge` by readiness bounded at 64 with `chunked` and
`flushAfter`, `either`, `chunked`/`unchunked`, `widen`), `Chunks.merge`;
`Writer.expand` in the core. 20 tests mirror TestChannel,
TestChannelAsyncCross, TestChannelFailureCross, TestMergeEnds, TestDrain
and TestStreamSource; 186 in the okay2 gate.

- `Source[A | B]` is `Source[B >: A]` (no union types): `merge[Any]`,
  or `either` for the side as data; `Source.widen` is the cast.
- `Channel.apply` is `StmChannel`; the ring-buffered mechanisms of the
  Scala 3 default are backlog `okay2-fast-channels`.
- `Member.pure` is the top rule with a `NotPure` side condition on the
  deeper rules: `Member[Pure, A + B]` had two derivations at one level
  and the first cut's test passed by accident.

Docs: docs/okay2.md section 12; specs/okay2.md stage 5.
