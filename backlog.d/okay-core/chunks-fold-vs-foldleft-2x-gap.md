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
      THE "NOT YET TRIED" PROBE, RUN (2026-09-19,
      producer-writer-carrier-pure-iterator, quiet host, load 2.0-2.5,
      JDK 21.0.12 pinned, 2 rounds, ±0.04): routing the loop through
      its own non-inline method (`Probe` in
      ProducerWriterCarrierBenchmark.scala, kept) does NOTHING for
      Producer — `chunksFoldProducerOwnMethod` 4.55/4.68,
      `chunksFoldLeftProducerOwnMethod` 4.52/4.53, and even the inline
      `Chunks.foldLeft` (static `summon` of the given object) inside an
      own method 4.53 — while library `Chunks.fold` stays 2.53. Ruled
      out therefore: placement (benchmark method vs own method), and
      how the instance is reached (val of interface type vs static
      summon). THE SURPRISE: the byte-identical loop over the PURE
      WRITER stream's new iterator (Writer.scala) in the same own-method
      placement IS 2.63 (from 4.9 inside the benchmark method), so
      placement matters for Feed and not for Producer, and in
      `compare`'s compiled units Feed beats Producer 1.7x with identical
      allocation (10,064 vs 12,600 B/op — the 2.5 KB is `Say` nodes).
      What is left: every Producer loop COMPILED IN `compare` is
      4.5-4.85; the only one compiled in okay-stream (`Chunks.fold`,
      `A` abstract at the inline expansion of `foldLeft`) is 2.53. One
      semantic difference found, unmeasured: with `A` abstract, `c(i)`
      is `ArraySeq.apply(i): Object` handed straight to
      `addLong(J,Object)`; with `A = Long` at the call site dotty
      unboxes it first. Next probe: an own method in compare with `A`
      kept abstract (`def f[A](p: Chunks[A])(using Fold.OfLong[A])`),
      and `-prof perfasm` if it ever becomes available. No longer on
      the writer migration's path — Feed reaches parity with
      `Chunks.fold` on its own.
