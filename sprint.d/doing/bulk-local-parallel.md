- [ ] bulk-local-parallel — a parallel `Bulk[Chunks]` in one process on
      the platforms with threads (operator, 2026-09-30: "Делай без
      замера"): `BulkParallel(parallelism, lines, bytes)` on JVM and
      Native — splits read on `parallelism` fibres with order kept,
      `aggregate` folded in batches of chunks on fibres and merged in
      order (so a `Sequential` stays right), `join` with the right side
      folded in parallel and the left side streamed through a parallel
      chunk window; everything else delegated to `Bulk.local`, whose
      behaviour is unchanged. `parallelBulk(n)` beside `localBulk` on the
      JVM. Agreement law with `Bulk.local`; no measurement (operator).
