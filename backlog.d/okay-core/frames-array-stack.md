- [ ] frames-array-stack — PRIORITY: LOW, PARKED 2026-10-01 (operator:
      design first, optimize later). PROBED on feature/frames-array-stack
      (57c7c2259 flat/segmented, b0a1cbc8d hybrid; rows in that branch's
      history.d): bare install/pop 0.78-0.90x, everything that captures
      LOSES — hybrid contAnswer 1.75x, statePara 1.70x, writerTell 1.51x,
      layered 1.27x, with more bytes: a capture seals the top array. And
      the typing degrades to `Any => Any` cells, which the operator
      rejected. Kept for the record; the original item follows.
      Was: a DECISION before code. The
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
      SINCE cont-atm (2026-10-03) the machine is `Delimited` (Delimited.scala):
      the λ$ `Stack`/`Frames` above are gone; the question stands for its
      `Frames`/`Stack`.
