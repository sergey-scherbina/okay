- [ ] delim-split-wrap-free — PRIORITY: LOW (3.5% bytes on the capture
      path, no time signal measured). stack-safety-core (b27ae0c5d) made
      `Delim.split` a loop and it costs +32 B per capture on
      DelimBenchmark.delimGenerator (910 312 → 942 312 B/op, bisected
      by delim-generator-bytes-drift 2026-09-25): a `Wrap.On` node and
      a polymorphic frame per segment the walk passes, where the
      recursive version rebuilt the prefix on the JVM stack for free.
      THE RECIPE: rebuild the captured prefix in ONE pass with a mutable
      tail — a machine-private mutable `rest` on the copied `K`/`Ret`
      node, set exactly once by the loop (the node is unpublished until
      the loop ends, so the mutation is invisible); or a two-pass walk
      that counts and then rebuilds from an array of frames (one array
      per capture instead of one node per segment). Both keep the loop
      the stack-safety rule requires and keep `no cast`. Measure
      delimGenerator (must return to 910 312) and delimDollarResume.
      DONE WHEN: the bytes are back and specs/stack-safety.md's
      `Delim.split` bullet says so.
