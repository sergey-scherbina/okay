## producer-writer-carrier-chunked-fold - the chunk-aware writer fold, half at parity

Stage 0 of the producer-to-writer-carrier migration named a
prerequisite it could not clear itself: `Chunks.fold` on the writer
carrier lost ~2x to `Chunks.fold` on `Producer`, because `Writer.fold`
dispatches on the TOLD type (`Chunk[Long]`) rather than the ELEMENT's
type, so it can never take the unboxed `Fold.OfLong` fast path
`Chunks.foldLeft` does. This lane built that missing combinator.

`Chunks.foldLeftWriter` and `Chunks.foldWriter`
(`okay-stream/src/main/scala/Chunks.scala`), on top of the existing
`Writer.foldWith` trampoline. Three JMH rounds, N=10000 longs, chunk
64:

- **`foldLeftWriter` (literal step, called directly) reaches parity**:
  5.00 us/op against `Chunks.foldLeft`'s 4.76, same sign across all
  three rounds. This is the shape `okay-cluster`'s own
  `Chunks.foldLeft` call sites already use (`Flows.scala`: 6,
  `Job.scala`: 1) — they can migrate onto the writer carrier now.
- **`foldWriter` (`Fold`-instance dispatch, what `Chunks.fold` itself
  does) does not**: 17.30 us/op, 3.5x the direct call and 6.8x
  `Chunks.fold`, stable across three different implementations tried.
  The gap is isolated to the dispatch layer — a captured `Fold.OfLong`
  instance's virtual call, identical in shape to what `Chunks.fold`
  itself does at no cost — but not fully explained; would need a
  profiler this environment does not have. `Bulk.scala`,
  `Pipeline.scala`, and `Acceptance.scala` (the only three
  `Chunks.fold`/`agg.fold` call sites) stay on `Producer` until it
  closes.

SHIP THE HALF THAT WORKS rather than block on the rest: the honest
finding, both directions, is written into
`specs/producer-to-writer-carrier.md`'s `## Results` and the sprint
item, so a second attempt at `foldWriter` starts from what's already
ruled out instead of repeating it.

Landed alongside: `writerk-companion-scope` — `given writerK` moved
from a bare top-level package member into `object Writer`'s own body,
so it resolves by companion-scope search from any package with no
import. Found while writing this lane's own tests: every
`Writer.fold`/`.collect` call on a parameterized `W` needed an
explicit `import okay.writerK` before this, on top of the `@nowarn`
its E092 erasure caveat already needs (that part stays — it is a real,
sound-by-construction caveat, not an import problem).

Gate: `okayStreamJVM/test` 346/346 green, 0 warnings; JS and Native
`compile` clean.
