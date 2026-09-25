## cont-stack-fastpath — no allocation per run for the stack gauge, and where fib100's bytes really go

specs/cont-stack.md plan stage C.

- C1: `Cont.run` no longer wraps the user's `k` in `Gauged` with a
  fresh `Gauge` — two allocations for every run whether or not it ever
  went deep. The gauge is attached at the chain root (the outermost
  `Reentry`'s `k`, a `var` for that one write) on a run's first
  exhaustion, and found by the same walk after. −64 B/op on fib100
  (one run per op), 1.08; 29 Cont tests green.
- THE FINDING: an exact allocation count (`ThreadMXBean` over 200 warm
  runs, on the base and on the lane) reads 21 771 B per fib100 run on
  BOTH — the runtime layer allocates nothing extra. JMH's +1 600 B/op
  is C2 escape analysis: the base scalar-replaces objects that escape
  on the lane once `callK`, `enter` and the re-entry into `step` are on
  the hot path. Two JFR profiles show the same classes on both trees.
  Stage C's remaining lever is the inliner, not object shapes:
  backlog cont-stack-fastpath carries the recipe.
- history.d `cont-stack-fastpath`: the A/B and the exact count.

**Round 2 (same day):** `PrintInlining` on fib100, both trees. The
base's 13-byte `Mapped` lambda inlined and was scalar-replaced; on the
lane `Reentry.enter` (106 bytes) read "callee is too large" and `callK`
through it "callee uses too much stack". The exhaustion road moved out
of `enter` into `exhausted` (`enter` is 53 bytes and inlines hot where
it was refused). Measured on a busy evening: fib100 one fork in three
read the BASE's bytes (21 568 vs 21 552; the others 23 152) — the JIT
can eliminate the +1 600 B again and sometimes does; time 1.19 in
noise up to ±1 111, not a verdict. Kept: a smaller hot method, no
behaviour change. What is left of C: `Leaf.applyAt` (131 bytes) and
`step` (557, never inlined either way) spend the budget the two
remaining refusals run out of — backlog cont-stack-fastpath.
