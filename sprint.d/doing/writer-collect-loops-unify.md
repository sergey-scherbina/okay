- [ ] **writer-collect-loops-unify** — `Writer.collect` (Writer.scala
      ~150), `Source.runCollect` (Source.scala ~220) and `Source.concat`
      (Source.scala ~172) are three hand-written copies of the loop
      `Writer.loopWith` already is, each accumulating with `Vector :+`
      per element, and `concat` finishing with a `.map` over a residual
      that still forwards Async — the documented trap (never map over a
      residual: a rotation per forwarded operation).

      WHY: the reason they were written separately — a split on
      `TypeableK[Writer % W]` was an unchecked E092 test at a
      parameterised W, so they split on the concrete G instead —
      vanished with writer-typeablek-by-class (87be6174, 2026-09-19):
      `writerK` tests the class of `Say` and needs no `Typeable[W]`.
      Both comments still cite the vanished reason. `Writer.run` already
      paid the cons+reverse move (either-scalarised, 2026-09-09: -13%
      time, -35% B/op — history.tsv), these three did not.

      HOW: `loopWith`'s `finish` takes the answer too — `(S, A) => R` —
      so a caller finishes INSIDE the loop and the residual is never
      mapped; `foldWith`/`run` are the same calls with a tuple. Then
      `collect`, `runCollect`, `concat` are one-line calls: cons per
      element, one `reverse` (+ `toVector`, or a builder for `concat`'s
      flatten) at the end. `Producer.concat` is NOT in scope: zero
      callers, and pwc-arc-close kept `Producer` whole.

      NUMBER OWED: `IdiomaticApiBenchmark`'s runCollect lane before and
      after, alternating, and a `Source.concat` row on the chunked
      source shape okay-jdbc drains. spec: specs/writer-collect-loops.md.
