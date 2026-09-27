- [ ] map-fusion-residual — PRIORITY: MEDIUM (measured gap). After
      map-fusion (2026-09-27, specs/map-fusion.md), a right-nested program
      whose steps are `op.map(f)` then a flatMap still reads 24.6 µs
      against 12.6 µs when each step is written as one flatMap
      (BuildShapeBenchmark rowFoldM / rowOneBind). `map` builds a
      `Bind(op, Mapped(f))` that the following `flatMap` immediately
      discards to build `Bind(op, y => g(f(y)))`: two objects allocated
      and dropped per step, plus the composed lambda. Roads: `!.foldM`'s
      step (and similar library builders) could take the map-less form;
      or `Mapped`'s Bind could be reused by the next bind (mutating it
      is refused, since programs are values). Measure `-prof gc` per
      step first, to see what the remaining 110 KB per 1000 steps is.
