- [ ] cont-stack-fastpath — what stage C left (specs/cont-stack.md plan
      C, 2026-09-25): fib100 reads 1.08–1.17x and +1 600 B/op in JMH
      against the pre-cont-stack base, yet an EXACT allocation count
      (ThreadMXBean over 200 warm runs) is identical on both trees,
      21 771 B/run. The delta is C2 escape analysis: the base
      scalar-replaces the `Mapped` lambda / the continuation object that
      on the lane escapes — `callK`'s type test, `Reentry.enter`'s `min`
      and the re-entry into `step` push the hot path over the inliner's
      budget. Recipe: `-XX:+UnlockDiagnosticVMOptions -XX:+PrintInlining`
      (or `-prof perfasm`) on fib100 for both trees, diff the inlining
      trees of `step`/`Leaf.applyAt`/`callK`/`Reentry.enter`; then make
      the hot path smaller (a `callK` without the type test where the
      runner knows its `k`, `enter` without `min` on the flat chain), one
      change per number. Object shapes (Gauged: done, C1; Mapped's
      lambda as a class; the bridges) are NOT the lever — the exact count
      says so.
