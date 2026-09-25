- [ ] cont-stack-fastpath — what stage C left after TWO rounds
      (specs/cont-stack.md plan C, 2026-09-25; round 2 slimmed
      `Reentry.enter` 106 → 53 bytes and one fork in three then read
      the base's bytes — the lever is right, not yet deterministic): fib100 reads 1.08–1.17x and +1 600 B/op in JMH
      against the pre-cont-stack base, yet an EXACT allocation count
      (ThreadMXBean over 200 warm runs) is identical on both trees,
      21 771 B/run. The delta is C2 escape analysis: the base
      scalar-replaces the `Mapped` lambda / the continuation object that
      on the lane escapes — `callK`'s type test, `Reentry.enter`'s `min`
      and the re-entry into `step` push the hot path over the inliner's
      budget. Recipe: `-XX:+UnlockDiagnosticVMOptions -XX:+PrintInlining`
      (or `-prof perfasm`) on fib100 for both trees, diff the inlining
      trees of `step`/`Leaf.applyAt`/`callK`/`Reentry.enter`; then make
      the hot path smaller, one change per number: next `Leaf.applyAt`
      (131 bytes: split the `Mapped` and `Absorbed` cases into two
      small methods) and `callK` (its type test where the runner knows
      its `k`); `step` at 557 bytes is never inlined on either tree. Object shapes (Gauged: done, C1; Mapped's
      lambda as a class; the bridges) are NOT the lever — the exact count
      says so.
