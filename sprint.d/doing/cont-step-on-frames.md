- [ ] cont-step-on-frames — the last stage of freer-kont-migrate
      (specs/freer-kont.md stage 2). DECIDED (operator, 2026-10-01):
      the SEGMENTED frame stack of step 1 is the machine — `Frames` one
      segment, `Stack` = Done | Run | Reset carrying its segment
      (Dybvig, Peyton Jones & Sabry 2007). No revert; the work left is
      to make it fast. Step 2 (Cont's runner on `Frames.run`) is
      refuted and reverted (history.d 2026-09-30T221639Z, 4.3-6.3x).
      Baseline at 1f against the single-list machine (history.d
      2026-10-01T050417Z-cont-step-on-frames-step1.tsv): install/pop
      1.36-1.45x (delimPushOnly, delimDollarOnly), resume 1.16-1.19x,
      generator/stateDeep 1.07-1.09x, stateLexDeep 1.02-1.03x; bytes
      -21..-33% on every resuming lane. Next: profile delimPushOnly
      (async-profiler, both arms) and close the install/pop gap first.
