## source-zip - zip for the async stream, in merge's fiber-per-side shape

- `Source.zip(s, t, capacity)` / `Source.zipWith`: lockstep pairs until either side
  ends; each side `Channel.buffer`ed on a fiber of its own, the pairing on
  the consumer's thread. The side that ends first closes the other, so a
  feeder parked on the full buffer wakes and ends; an early stop releases
  both through `Merge.closing` (now `private[okay]`), counted by
  `mergeReleases`. A failing side fails the zip after every pair before it.
- `p zip q` on `Chunks`, the extension over `Chunks.zip`.
- A companion function, not an extension: an extension would be a top-level
  `zip` in package `okay`, which the core's lazy `zip` owns, and the
  additive gate showed a dependent keeping one and losing the core's.
- okay2 in step: `Source.zip`/`zipWith`, `ChunksOps.zip`. No early-stop
  release there: okay2 has no cancel scope, and its `merge` has none either.
- Taken over from a session that stopped uncommitted on 2026-09-28. The
  rebase onto the adaptive default (scheduler-default-flip, fd6eba1d8)
  turned the feeder-ends test red with no change to zip: `!thread.isAlive`
  proves a fiber ended only where a fiber is a thread. It is pinned to
  Loom now, with a scheduler-free check beside it (the endless side stops
  producing).
- Tests: `TestSourceZip` (6, okay-stream JVM; okay2-stream JVM),
  `TestChunks`, `TestDocExamplesSourceZip`; docs/guide.md §6.
  Spec: specs/source-zip.md.
