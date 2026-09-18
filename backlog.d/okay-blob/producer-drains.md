- [x] producer-drains — DONE 2026-09-16: `Producer.fold` and
      `Producer.concat` in core; the ten drains are one call each, and
      tail-recursive across chunks where they recursed through `map`.
      Was: the survey behind blob-source-road counted
      TEN hand-rolled `uncons` loops draining a
      `Chunk[X] ! (Produce + Async)` into a `Vector[X]` — Backup and
      Offload `drainList`, S3 `drainBytes`, Fs `sink`, jdbc `Poll`,
      `SqlStore`, `Migrate`, `Writes`, outbox `Rows`, rag `PgVector`
      — each summoning the same `Stream` instance and writing the
      same `go`. One `Producer.toVector` (or `each` folded into an
      accumulator) beside `Producer.each` retires all of them, and
      each is covered by its module's own suite. Six modules, so a
      full gate.
