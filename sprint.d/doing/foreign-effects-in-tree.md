- [ ] foreign-effects-in-tree — PRIORITY: MEDIUM (operator, 2026-10-04).
      Probes, then a spec (specs/foreign-effects-in-tree.md): (1) each
      library's value as an effect stored in the tree, named as what it
      wraps (`IO`, `ZIO[R, E, *]`, kyo's pending type); (2) a `for` over
      ZIO steps that joins R and E the way ZIO's own flatMap does;
      (3) whether `Free` should be covariant in its row at all —
      inference of a mixed `for`, sites the tree's walkers lose, and the
      one-bridge `Unary` (a match type) under covariance. Probes are
      compile-only; nothing lands in main code in this lane.
