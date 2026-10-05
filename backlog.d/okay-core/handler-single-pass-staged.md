- [ ] handler-single-pass-staged — PRIORITY: MEDIUM (2026-10-05, from
      handler-single-pass stage 2; spec "Dispatch", level 2). The one
      walk over a stack costs the right-nested shape of a recursion 1.17x
      against today's nested handlers (FusionBenchmark.handledSWr
      13.5 against 11.5 µs), because it finds the handler through a table
      and calls its `stepAt` megamorphically where two nested loops had
      their steps inlined. Fold-built programs are faster on it already
      (0.91–0.95x). Level 2: when ONE `handle(h1, h2, h3)` names the
      whole stack with handlers known statically, a macro writes the
      walk itself, a `match` over the operation classes whose branches
      are the inlined steps. Bar: handledSWr at or under 1.00x.
