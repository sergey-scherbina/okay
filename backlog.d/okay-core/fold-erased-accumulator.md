- [ ] fold-erased-accumulator — the last library-side half of the fold
      path's residual (fold-each-residual-split, 2026-09-28,
      specs/map-fusion.md "What is left in the fold path"). A
      `foldEach`/`foldM` step boxes its accumulator: `B` is erased, so
      every sum past `Integer`'s cache is a 16 B `Integer.valueOf` on its
      way into the next `go` — 3.3 µs per 1000 steps, the same size as
      the Vector read the lane removed. MEASURED CEILING of the one road:
      BuildShapeBenchmark.stateFoldEachInt, `foldEach`'s `go` verbatim
      with `B`, `A` = `Int`, reads 14.04 µs against the library's 17.35
      (1.24x, −16 B a step). That is what an `inline def foldEach` whose
      local `go` is expanded — and so compiled with the caller's concrete
      `B` — would produce, at the price of a copy of `go` per call site
      and the inline budget it adds to every caller. TRIGGER: a consumer
      whose hot fold carries a primitive accumulator (a Stage summing a
      chunk, a counter over a Source) and shows it in a profile. Not a
      `@specialized` question — Scala 3 has none.
