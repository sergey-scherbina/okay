- [ ] merge-shared-scope-gc-release — PRIORITY: HIGH (can drop data).
      Found by source-zip-lost-pairs (2026-09-29). `Merge.Shared.elements`
      has the shape `Source.zip` had: its cancel scope is ENTERED in front
      and never exited or named again, so on a plain `runWith` (no drive,
      no fiber handler holding it) nothing references it while the merge
      runs, and a collection releases it through its collector door
      (`Unreachable.onCollected`, the backstop for an ABANDONED program):
      `Merge.closing` closes the merged channel under running producers.
      Measured: a GC forced mid-run (a probe, not committed) gave
      `releases 1` on `given Merge = Merge.Shared` and 0 on `Ready`. That
      round still delivered 800 of 800; the loss needs the close to land
      before a producer's last send. The zip lost 1-99% of its pairs the
      same way (14-26 rounds in 400). THE LANE: red first —
      `TestSourceZip`'s "a collection mid-run" shape (a thread
      collecting beside ~3000 warm rounds) on `Shared` merge; then keep
      the scope reachable without a Bind per element (the zip names it
      in its end branches; `ch.drained` has no loop of its own to do
      that — hold it from the Drain, or exit at the drain's end).
      Check `Source.releasing` users and ReadyMerge the same way, since
      "count the doors".
