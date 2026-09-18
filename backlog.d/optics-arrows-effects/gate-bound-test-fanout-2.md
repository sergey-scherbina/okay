- [ ] gate-bound-test-fanout (superseded; kept for its measurement) — THE OTHER HALF OF THE 57-MINUTE STALL
      (gate-watchdog, 2026-09-18). The watchdog now catches a hung
      gate; nothing yet stops it happening. MEASURED at the stall: 165
      test-runner processes alive at once — 100 `node` (Scala.js) and
      65 Scala Native binaries — all spawned inside the first 20
      seconds, on a 14-core box, with `Tags`/`concurrentRestrictions`
      set NOWHERE in build.sbt or project/. sbt's own default limits
      TASKS, not the processes a single test task spawns, which is why
      the fan-out is what it is. WHAT TO DO, in order: (1) `show
      concurrentRestrictions` and record what the default actually is
      here — do not guess it; (2) count the fan-out per platform on a
      quiet box; (3) bound it with `Tags.limit`, and measure the wall
      clock before and after, because the whole point of the fan-out
      is speed. TRIGGER TO DO IT NOW: a second stall whose dump shows
      the same shape. The dump that opened this is the one named in
      the CHANGELOG entry.
