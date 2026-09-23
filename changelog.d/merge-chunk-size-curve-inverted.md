## merge-chunk-size-curve-inverted - closed as consistency, not performance

`Source.merge`'s own `chunked = true` path still called
`through(...)(Stage.unchunk)` directly — the coroutine-pairing
mechanism a 2026-09-10 investigation (this entry) had already fixed
everywhere else via `Writer.expand` (`Source.unchunked`). Wired `merge`
onto the same proven mechanism: one canonical unchunk road instead of
two. `okayStreamJVM` unchanged (371/371).

The number, honestly: three quiet before/after pairs on
`ChunkFlushBenchmark.okayChunked` (one `before` round hit a load spike
of 1712 µs from a sibling gate and was discarded), per-arm minima
197.5 vs 198.4 µs, 4 160 026 vs 4 172 129 B/op — **parity, not a win**.
Confirmed why: `merge`'s chunked flag has no way to raise its chunk
size past the hard-coded `Source.ChunkSize = 16`, and the original
investigation's own numbers say the coroutine-pairing overhead was
"~0" at k=16 — the curve only rises past k≈256. This path could not
have moved the curve, because it never runs at a size where the curve
exists.

Kept anyway (zero measured cost, one less mechanism to maintain).
What actually remains is renamed and re-scoped as
`merge-chunked-flag-fixed-chunk-size` (backlog okay-core): expose the
chunk size on `merge`'s flag so the question can be asked at the
sizes where it matters. Entry closed to `BACKLOG-ARCHIVE.md`.
