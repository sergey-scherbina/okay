- [ ] cont-stack-exact-million — PRIORITY: LOW (2026-10-04, from
      cont-stack-exact-first). With the stack READ, ContDepthBenchmark
      .strictLeaf at 1 000 000 levels takes 74.8 ms against 32.4 counted
      (2.31x, +48 MB/op). Below that the two are within 4% (1.04x at
      1 000, 1.03x at 100 000). Ruled out: the reads themselves (189 a
      run), the flag (native access without reading is 1.00x), one
      deep stack (`levelsPerStack` gives both roads 4 switches), the
      shape of `deeper` (`roomEnd` out of line) and a megamorphic `within`
      (inlined). Seen: G1's workers spend 30% of the CPU in
      `steal_best_of_2` against 10% counted, and a capture's `Segment`
      (`segmentAtTop`) escapes only when reading. Next: `-Xlog:gc*` on
      both arms, and `-XX:+PrintCompilation` / LogCompilation for
      deoptimisation at the first grant.
