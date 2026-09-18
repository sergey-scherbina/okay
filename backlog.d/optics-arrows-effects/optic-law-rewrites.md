- [ ] optic-law-rewrites — CITED AS FILED IN TWO PLACES AND FILED IN
      NEITHER (found 2026-09-18 by optics-guide-page, which wanted to
      link it): docs/benchmarks.md §9b ends "Filed as
      `optic-law-rewrites` with these numbers attached" and the
      CHANGELOG entry says the same, and no board ever got the entry.
      The work: teach `Fuse` the two rewrites its own measurement
      priced. `map(f) . map(g) == map(f . g)` on a container is the
      big one — one pass is 1570 ns / 19 672 B against two at 4006 /
      38 320, and the JIT cannot do it because it does not know the
      law. `set ∘ set` into one `copy` on a product is the small one:
      1.48 ns against 3.94 for two fused sets, with allocation equal
      at 24 B because escape analysis already scalar-replaces the
      intermediate — so the prize there is ~2.5 ns of field traffic
      and nothing in bytes, ON THE JVM. Native and JS have no escape
      analysis of that quality, so the allocation half is unmeasured
      there and a lane should measure before claiming it.
