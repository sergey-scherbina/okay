- [x] gate-bound-test-fanout — DONE 2026-09-18, one line and a
      measurement: `ThisBuild / Test / parallelExecution := false`.
      MEASURED rather than guessed: `show concurrentRestrictions` says
      `Limit all to 14` (TASKS) and `Limit forked-test-group to 1`,
      but `Test / parallelExecution` was TRUE on all three platforms —
      and on JS/Native a test CLASS is an OS PROCESS. One task fans
      out into all of its classes; 14 x ~10 is the 145-165 both dumps
      caught.
      THE PROOF IS THE CONDITION, not the clock: the same `affected
      master` scope that STALLED TWICE (module 77, then 78) ran to
      completion — 92 modules, 303 s — while a sibling's unbounded
      gate held 152 node processes at load 76. Peak children 104.
      NOT CLAIMED: 104 is not the ~14 one-task-per-core predicts, and
      the remainder is unmeasured (`gate-fanout-what-is-left`); and
      303 s is not comparable to the 179 s baseline, which was a quiet
      box. Only failure: `hedge-start-timing-flake`, 4th sighting,
      3 of 3 green alone.
