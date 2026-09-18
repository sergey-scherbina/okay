- core-modularise — what stays in the core and what becomes a module
  (specs/core-modules.md). The core was 74 files and 21 914 lines, and
  the graph already held the answer: the whole control layer names a
  channel, a source or a chunk ZERO times in code. The rule that fell
  out of the measurement, three times at three unrelated seams, is
  **an interface stays, the machinery that implements it leaves**.
  STAGE 1 IS THIS LANE: `okay-stream` takes the channels, sources,
  pipes, chunked collections and the buffers under them, about 6 100
  lines. The core keeps `Stream` (the `uncons` typeclass `Writer`
  implements), `Handoff` (what `Async.handoff()` answers) and the
  `Chunk` alias `Producer.concat` is typed on. The package is still
  `okay` — probed on 3.9.0 first — so no consumer's imports change and
  only eight modules needed a `dependsOn`; the rest get it
  transitively.
  STAGES FILED, NOT TAKEN: `okay-workflow` (Wf, Proc, ProcMacro, 1 979
  lines, already a leaf) and `okay-data` (Sketch, Windows, Uid, Hlc and
  the aggregators). Optics and STM are extractable in principle and
  NOT by moving files: `State.zoom` is typed on `Lens`,
  `Proc.procArrow` on `Optic.Arrow`, `Providing.Facts` on `TMap` and
  `Stm.sim` on `Sim`. Cutting one of those seams is worth more than
  the lines it moves, because it is the law applied where it is not
  yet obeyed.
