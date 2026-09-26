## source-merge-via-ready — one merge mechanism

`Source.merge` (elementwise) is now each side buffered onto a fiber of
its own and joined by `Source.mergeReady` — the ring of source
continuations — instead of both fibers pushing into one shared
two-part channel. `mergeReady` takes the rule `merge` always had:
DRAIN, THEN FAIL — a failing source drops out, the others run to
their end, then the first failure; nobody is cancelled for it. A ready
side of `merge` tells up to a batch (64) in a row; `mergeReady` itself
keeps its strict round-robin. Measured through bench-window: 0.92-0.96x
of the old road at the default capacity 64, parity at 1024, a NAMED
~10% loss at 256 (two explanations refuted; backlog `merge-cap256-gap`).
FOUND AND FIXED IN THE CORE on the way: Writer's stream views
(`Writer.uncons`, both linear iterators — the one `Channel.buffer`
walks) applied the continuation before handing the told value over,
so a source whose next step threw lost the value it had just told; the
old `Source.merge` lost it too. `TestWriterToldBeforeThrow`,
`TestSourceToldBeforeThrow`, a Source-level failure law in
`TestChannelFailure`, TestReadyMerge's failure laws rewritten. The
chunked roads still use the shared channel: backlog
`merge-chunked-via-ready`. specs/source-merge-via-ready.md.
