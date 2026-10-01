- [ ] frames-array-stack — PRIORITY: MEDIUM, a DECISION before code. The
      segmented frame machine's remaining limit is C2's register pressure
      in `loop$1` (three loop registers in stack slots: install/pop 1.36x,
      contAnswer 1.21x — backlog cont-frames-register-pressure). Two
      registers with shared IMMUTABLE segments is not possible (a merged
      `End(below)` copies on capture, a top-node register allocates per
      bind, nested loops exit per segment edge — 1k refuted). The way out
      is a MUTABLE ARRAY STACK per run (Loom, Kotlin coroutines): a bind
      stores `f` into a cell, the loop carries `focus` and an index, a
      capture copies its slice into an immutable `k`, a resumption copies
      it back — O(segment) per capture/resumption, the quadratic cases of
      TestKont back in theory, with arraycopy's constant. PREMISE PRICED
      (frames-loop-shape, FrameStoreBenchmark, history.d): N=1000 push+pop
      0.91 us with a fresh array (0.68 reused) against 2.85 us with a
      `Frame` node per push — 3.1-4.1x, 0-4 KB against 24 KB; G1's
      barrier is not the cost. THE OPEN QUESTION is the typing: the
      machine is GADT-typed end to end today, and an `Array[AnyRef]`
      stack reads every cell by a claim — against "no cast without a
      real necessity". Needs the operator's call on a typing approach
      (typed accessors, one isolated claim per kind of cell) before a
      probe is built.
