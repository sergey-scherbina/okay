- [ ] scheduler-flip-remeasure — the re-run table (scheduler-default-rerun)
      was measured before two changes to every callback drive landed with
      the default flip (2026-09-28): the slice hooks of
      drive-interrupts-blocking-run (a volatile write, a monitor exit and
      a thread-local read and write per slice) and drive-resume-throw-lost's
      one `Bind` per late resumption. Re-run, one lane per `jmh-lane.sh`,
      alternating arms on the SAME build (`-Dokay.scheduler=loom` against
      the default): `AdversarialBenchmark.forkJoin10k_okay`,
      `forkJoin10k_okayOwnInside`, the cancel-1k and `parallel8` lanes, and
      spawnJoinSeq; and one before/after pair on `adaptive` alone (the
      commit before cda0a94 against it) for the hooks' own price. DONE WHEN
      specs/schedulers.md "The flip" carries the rows, or names the lane
      that moved. PRIORITY: MEDIUM — the default's speed claim rests on a
      table this code has not been measured under. (2026-09-28) ADD the
      chunked merge: it has no lane in the re-run table and pays the flip
      1.5-1.9x (backlog adaptive-chunked-merge-cost, 2026-09-29).
