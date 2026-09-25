- [ ] cont-stack-fastpath — specs/cont-stack.md plan stages C and D,
      after cont-stack-ab has priced what cont-stack-switch pays on a
      program that never goes deep (fib100: `Reentry` 1.12x / +1 600
      B/op in the morning's runtime-only A/B, plus `Gauged`'s two
      allocations a `run` since). Each candidate ALONE, measured
      against the stage before on fib100/fib1000/statePara/
      handleCapture, kept only when it pays: (C1) the run's `Gauge` as
      a field of the OUTERMOST `Reentry`, found by the same chain walk,
      instead of a `Gauged` root allocated per `run`; (C2) `Mapped`'s
      `a => callK(k, g(a), room - 1)` lambda as a class carrying the
      room, so a `Mapped` chain keeps its gauge and frame count; (C3)
      the three `Function1` specialisation bridges a level
      (`apply$mcII$sp`, cold stack only). Then the reader's knobs, only
      if a profile of a deep OPAQUE program shows them: (D1) `_setjmp`
      for the pointer on macOS arm64 (326 → ~10 ns; `getcontext` saves
      the signal mask with a syscall; glibc mangles the slot, so
      macOS-only); (D2) the slice at 128 KB; (D3) a parked 1 GB thread
      reused across switches (33 → ~5 µs a switch). Target for C: the
      1.12x gone. Nothing here by taste — a number per candidate in
      history.d.
