- [ ] okay2-handler-allocs — found in the review (2026-09-24), by reading,
      NOT measured: every handler loop pays per handled operation for
      the shape of `Split.split`. `State.handleAt` allocates the two
      closures passed to it (they capture `s` and `k`), a `Tuple2` and
      the `Left` that carries the step back to the `@tailrec` loop —
      four objects where the Scala 3 core's inline split has none. The
      same shape is in `relay`, `handle` and every signature's handler
      (Writer, Throws, Reader, Async). A non-allocating form keeps the
      casts in ONE place, `Split`: `Split.isF[F](e): Boolean` plus
      `Split.asF`/`asG`, and the loop branches with `if` and recurses
      directly. Measure it against the current shape under
      `okay2-bench` (JMH, one lane at a time) before rewriting the
      handlers: the JIT may scalar-replace some of it already.
      MEASURED by okay2-bench (2026-09-24, history.tsv `okay2-*`):
      stateEffect 1.83x the Scala 3 core in time and 452 960 against
      276 904 B/op — +176 B per get/set pair, which is the four objects
      named above, NOT scalar-replaced; relayPrebuilt 1.31x / 1.18x B,
      handlePrebuilt 1.07x / 1.18x B. The State loop is the one to fix
      first; the lanes and their Scala 3 twins are ready to A/B it.
