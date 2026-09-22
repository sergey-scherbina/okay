- [ ] windows-stage-rerun-loses-pane — `Windows.stage` (okay-stream,
      Windows.scala) SILENTLY DROPS A PANE when the program `through`
      built over it is run a second time. Measured 2026-09-23 (found by
      java-gatherers, a throwaway probe): events ts 1,2,15,16,30,
      tumbling 10, lateness 0 — first run `Pane(0,10,3), Pane(10,20,31),
      Pane(30,40,30)`, second run of the SAME built value `Pane(0,10,3),
      Pane(30,40,30)`: the [10,20) pane is gone, no error. Mechanism:
      `through` drives the stage eagerly to its first output, so the
      built program holds the `Windows` object that run allocated, and
      the second run feeds it again after it evicted. Its own comment
      says "driving the same value twice must not share one pane map
      between the runs" — true for two `through` calls, false for one
      built program run twice. `Stage.chunked` survives the same probe
      (its state resets to empty after every emission). Fix: either
      thread the pane map as a value the continuation re-creates, or
      refuse the second run by name the way `okay.java.Gather.stage`
      does (a `finished` flag checked on await). Write the failing test
      first: the probe above, as an assertion. Count the doors: grep
      the other stages that allocate mutable state on first element.
