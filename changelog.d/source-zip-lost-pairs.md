## source-zip-lost-pairs - the zip's scope released by the collector mid-run

- `Source.zip` ended early, with no error: 1600 of 2000 pairs in the
  ci-runner's whole build, filed as a flake. Alone it failed 14-26 rounds
  in 400. Its cancel scope was entered and never named again, so on a
  plain `runWith` nothing held it. A collection mid-run released it
  through the collector door (`Unreachable.onCollected`, meant for an
  ABANDONED program), which closed both sides. The stack was captured on
  `Cleaner-0`.
- Fix: the zip exits the scope where it ends, and the loop names it, so
  every continuation keeps it reachable. An early stop is still released
  by the drive, a fiber's handler, or the collector.
- Red first: `TestSourceZip` "a collection mid-run does not end the zip"
  (a thread collecting beside 3000 warm rounds) went red at round 385.
- `scripts/recscan.py`: freer-base-step-extractor moved the trampoline
  node to `Freer$Bind`. The matcher knew only `Free$Bind`, so every lane
  touching okay-stream went RED on 17 recursions that are deferred.
  `Freer?\$` fixes it.
- Same shape still open: `Merge.Shared.elements`, filed as
  backlog.d/okay-core/merge-shared-scope-gc-release.md.
- Found beside it: this lane's `affected master staged` went red on
  `TestMergeOrder` "Channel.merge: each side arrives in exactly the order
  it sent" (a ring's worth of one side after the next). Bisected at
  OKAY_MERGE_ROUNDS=400: 5da64e406 green, f50932cbe (resumeLate on
  `forkLong`) red by round 9. Already cured on master by ebdfd0ec7
  (resume-late-small-ring-cost: `fork`, not `forkLong`), which is green
  at 400 rounds. Kept here so the next red of that law knows where to look.
