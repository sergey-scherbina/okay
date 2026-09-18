- [x] windows-packed-key — WRITTEN, MEASURED AND REVERTED
      (2026-09-11). The entry said to measure before writing it; the
      ceiling justified writing it and the engine refused it.
      THE CEILING, on the two map shapes alone at Wrocław's own
      boundary count (122 679 entries, each inserted by a partition
      and merged by the coordinator, carrying the accumulator the job
      really uses): tuple-keyed HashMap 21 991 us and 32 503 104
      bytes, packed LongMap 12 253 us and 27 921 208 — 1.79x, about
      9 ms of a fan that runs in 111. Worth a seam, so one was
      written: a `Store` packing (window index, Int key) into one
      `Long` with a per-ENTRY fallback, because whether the packing
      fits depends on the data and not the types.
      IN PLACE IT LOST, and not narrowly. On the Wrocław job the fan
      allocated 1 506 199 512 bytes against 1 377 755 184 — 9.3%
      WORSE — and the sliding stage 13.1% worse, with the control (the
      source alone) unmoved at 0.0005%. The ceiling was measured on
      the wrong SHAPE: one map with 122 679 entries, where the engine
      has eighteen maps that grow into that total. An open-addressing
      `LongMap` copies its whole table on every growth where a
      `HashMap` allocates a node once and never moves it, and against
      accumulators that are objects anyway, the trade loses.
      The mechanism is reverted. WHAT STAYS IS THE INSTRUMENTS:
      `MeasurePaneStore` (the two shapes, the ceiling) and
      `MeasureWroclawBytes` (what the real job allocates, per road) —
      the second is the one that decided it, because allocation is
      deterministic where this lane's wall clock moves 10% between
      runs. Anyone reopening this needs a number from the second.
