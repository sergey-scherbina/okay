- [ ] foreign-reduce-wire-heal-flake — `TestForeignReduce` "a WIRE
      failure heals on a fresh interpreter, invisibly to the
      coordinator" failed once at line 102 (`assertEquals(got.value,
      …)`, 0.009 s) inside an `affected master staged` run on
      2026-09-26 (ready-merge-cancel-race's gate, a busy box, the suite
      reached as a dependent of okay-stream), then passed twice alone.
      The suite sets a GLOBAL `ReduceJobs.reducer` per test and the
      fake counts `died` — shared mutable state across tests is the
      first suspect for a run-order/parallel-suite effect; confirm by
      running the suite beside another that sets `ReduceJobs.reducer`,
      or under 20 CPU burners, before changing anything.
      TRIGGER: its next red. (2026-09-26, ready-merge-cancel-race)
