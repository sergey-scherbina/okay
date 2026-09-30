- [ ] streams-seam-2-streamed — lane 2 of specs/streams-seam.md
      (operator: "Бери", 2026-09-30): `Streamed`, a new signature in the
      row beside `Tables`/`Sort` — JoinSorted, JoinWithin, Windowed, Zip —
      with `Streamed.viaTables`, the platform-free default through
      `toChunks` and the local machines (SortMerge, WindowJoin, Windows,
      Chunks.zip); `Flow.Join`, the engine's co-partitioned join (both
      sides exchanged by key, SortMerge / WindowJoin / hash per
      partition) answering the signatures natively on FlowBulk; the
      agreement law across Chunks and Flow at parallelism 4. Spark's
      native answers are a follow-on unless trivial.
