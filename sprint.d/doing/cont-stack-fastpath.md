- [ ] cont-stack-fastpath — what stage C left after THREE rounds
      (specs/cont-stack.md plan C). fib100 is CLOSED by round 3
      (2026-09-26): the `Mapped` leaf's continuation decided once at
      `applyAt` (`mappedK`) reads 0.91x master and 1.04x the
      pre-cont-stack base b4934c052, at the base's exact bytes (21 552
      B/op) on every round. LEFT: statePara, 1.11x the base with NO
      switch possible (`-Dokay.cont.room=1000000 -Xss64m`, 30.13 vs
      27.13 µs, +20 928 B/op every round), plus ~3 µs for the count
      road's one switch a run. These bytes are REAL objects, not escape
      analysis: an exact `ThreadMXBean` count reads 359 296 vs 321 888
      bytes a run. A JFR allocation SAMPLE by class does not separate
      them from noise (this tree's `Reentry` plus one lambda weigh
      what the base's three lambdas do), and shrinking `step`/`run`
      (the "callee uses too much stack" PrintInlining names at `run`,
      20 bytes against the base's 7) was measured and REFUTED
      (history.d `cont-stack-fastpath-r3-split`). NEXT: an EXACT count
      by class — an allocation-instrumenting agent (e.g.
      java-allocation-instrumenter) or `-XX:+UnlockDiagnosticVMOptions
      -XX:-UseTLAB` with JFR's InNewTLAB events on both trees — to name
      the ~21 B an operation, then remove that object; statePara builds
      `Absorbed` leaves (`Leaf.apply` → `applyAt` → `Reentry(g, k,
      room - 1)`), so that is the path to read. The switch's ~3 µs is
      the count road's, priced separately by cont-stack-jmh-native-access.
