- [ ] freer-base-remeasure — two base changes landed today unmeasured
      because the runner gated the box beside them: freer-consumed-index
      (S, R invariant; `tailShift` by one cast; `noProgram` a Delay) and
      freer-diag-leaf (a fifth enum case). The rule: after any change
      that could move speed, re-read the lanes. A/B, `mine` = 78f8dfec2
      against `ref` = its base before the two, alternating rounds, MIN of
      3, `jmh-lane.sh -f2 -wi3 -i5 -prof gc`: FibBenchmark.fib100 (the
      tail-shift and leaf road), HandlerBenchmark.statePara (the sharpest
      leaf lane), relayForward and stepBulk (the Free side, where a
      five-case hierarchy could change the JIT's view). Expected: nothing
      moves, bytes identical — the nodes are the same objects. Rows to
      src/jmh/history.d via `scripts/history.sh new`, verdict to
      specs/freer-base.md Results. Any lane over its bar names the change
      to bisect to.
