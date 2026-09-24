## okay2-stream - okay-stream's pure layer for the Scala 2 core

The stream vocabulary the Scala 3 core keeps in its core moves into
okay2's: `Stream` (uncons/iterator; List, LazyList, a writer program,
a writer program in G with the specialized walks), `Fold`/`FoldUntil`
with the four primitive accumulator shapes, `Aggregator`, `Pull`, and
`Writer.uncons/unconsIn/fold/foldUntil/of/widen`. The module
`okay2/okay2-stream` (package `okay2.stream`) holds `Chunks`/`ChunkBuf`,
`Take` and the coroutine pairings (`pipe`/`pipeIn`, `through`/
`throughIn`, `into`/`intoIn` — four names where Scala 3 overloads by
`targetName`), `Stage` (transduce, transduceUntil, mapAccumulate,
phased, chunked, unchunk), `Lines`, `Pipeline` with its optimizer,
`Windows`. 54 tests mirror the Scala 3 suites (TestChunks, TestPipe,
TestPipeline with scalacheck, TestWindows, TestTakeEach,
TestFoldUntilStreams, TestPhased's CSV) minus their Async parts;
132 in the okay2 gate.

- No casts on the writer side: `Say` is a case class here, so the
  pattern refines what the Scala 3 core's `Erased` had to assert.
- `ChunkBuf` is boxed; `range`, `ofChars` and `mapTagged`/`Pipeline.
  Mapped` (a ClassTag) are the unboxed roads, asserted by class.
- A deferred test effect `Later.Run(() => A)` (the shape of `Async.
  Run`) in the core's test sources: `Produce.Emit` takes a value, and
  three laziness tests had read construction as the run.
- Not ported, waiting for okay2-async: Channel, Source and merge, the
  buffers, ParallelChunks, Bulk/Tables, `foldWriter`, `Staged`.

Docs: docs/okay2.md section 10; specs/okay2.md stage 3.
