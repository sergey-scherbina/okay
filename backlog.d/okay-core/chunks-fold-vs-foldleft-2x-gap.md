- [ ] chunks-fold-vs-foldleft-2x-gap — `Chunks.fold` (2.55us, Fold-
      instance dispatch through `Fold.OfLong.addLong`, an interface
      call that boxes its element) measures ~2x FASTER than
      `Chunks.foldLeft` called directly with a literal `(s, a) => s +
      a` step (4.9-5.0us) — on the SAME `Producer` carrier, same
      `chunks` data, same accumulator. Backwards from what the
      bytecode difference alone predicts: `ladd` (a plain primitive
      add) should never be slower than a devirtualizable interface
      call. Found 2026-09-19 while checking whether
      producer-writer-carrier-foldwriter's remaining gap against
      `Chunks.fold` (now 5.43us, matching `chunksFoldLeftWriterDirect`
      almost exactly — see [[producer-writer-carrier-arc]]) could be
      closed further; it turned out the SAME gap already exists on
      Producer's own two forms, so it is not writer-specific and
      `foldWriter` has already reached the ceiling its own carrier's
      direct-call analog reaches.
      RULED OUT: allocation (10,064 vs 10,088 B/op, essentially
      identical, both measured via `-prof gc`); insufficient warmup
      (ratio held exactly at `-wi 20`, both benchmarks fully
      stabilized, tiny stdev); obvious bytecode shape differences
      (`javap` shows both compile to nearly identical iterator-based
      loops, differing only in the interface-call-vs-primitive-add
      step, which should favor the DIRECT form, not disfavor it).
      NOT YET TRIED: a diagnostic probe (routing the literal-step loop
      through its own small, non-inline, separately-compiled method —
      mirroring how `Chunks.fold`'s own body is structured, a tiny
      wrapper delegating to a method the JIT compiles on its own) was
      built but could not be measured — the host was under severe,
      unrelated contention (load average 30+, a virtualization process
      alone at 947% CPU) during the attempt, not a defect in the probe
      or the library. The probe's source is in this backlog entry's
      own git history if picked up again (compare/src/jmh/scala/okay/
      ProducerWriterCarrierBenchmark.scala, `diagFoldLeftProducerNonInline`
      + `DiagProbe.sumProducerNonInline`, reverted uncommitted rather
      than landed since it never got a clean measurement).
      Closing this needs instruction-level profiling (`-prof perfasm`
      or JITWatch) this environment does not reliably have working —
      `perf` is absent on this Mac. Revisit with real profiling access,
      or once host contention is confirmed clear (check `uptime`
      first, load under ~4 on this 14-core box, before trusting any
      number).
