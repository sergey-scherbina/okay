- [ ] cont-on-frames-probe — a bounded re-measure (operator ask): Cont's
      runner on the segmented frame machine, as cont-step-on-frames step 2
      did (d894c6a42, reverted: 4.3-6.3x Cont.step on fib/statePara/
      contAnswer, measured on the UNOPTIMIZED segmented machine). The
      machine has gained ~1.5-2x on Delim lanes since (Kept, under,
      re-entry). Cherry-pick step 2 onto master as a probe, Cont's suites
      and the stack suites green, A/B fib100, statePara, contAnswer against
      Cont.step (master). Criterion: within 1.2x to pursue, else refuted
      again with fresh rows. Also answer: does the lazy machine lower
      Cont's JVM stack use (a deep k chain, TestContStack).
