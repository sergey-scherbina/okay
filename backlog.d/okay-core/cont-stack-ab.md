- [ ] cont-stack-ab — PLAN STAGE A (specs/cont-stack.md "Stages"): the
      measurement cont-stack-switch landed without
      (operator's call, 2026-09-25): fib100 / fib1000 / statePara
      against 60a59c97e's parent, plus HandlerBenchmark.handleCapture, min of 3 alternating rounds, one
      lane per `scripts/jmh-lane.sh`, `-f 1 -wi 3 -i 5 -prof gc`, on a
      QUIET box — the afternoon's attempt was disqualified (load
      19–121, fib100 2.2x its own morning number, ±25% within a lane).
      What is expected: statePara back to master's number (its 12
      switches are gone: zero on the exact road, TestContStack); fib100
      paying `Reentry` (+1 600 B/op, 1.12x in the morning's runtime-only
      A/B) plus `Gauged` (two allocations a `run`); fib1000 unchanged.
      Record in history.d; if fib100 is outside noise, shrink the fast
      path (a `Reentry` without the `k` field for the flat chain, the
      gauge in the leaf, `Mapped` as a class) and measure again. Needs
      jmh-lane-jdk-pin or `JAVA_HOME` exported by hand (the variant
      does not compile on the PATH's JDK 17).
