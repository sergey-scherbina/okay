- [ ] merge-scope-reachable-timeout — the CI runner flagged
      `okay.TestMergeScopeReachable` (merge-shared-scope-gc-release,
      2026-09-29) as a three-time flake: "Merge.Shared: a collection
      mid-run does not release the merge" timed out after 30 s in three
      whole-build gates on 2026-09-30 (the test ran on to 47.6 s and
      51.6 s), green alone. The outcome is deterministic — 3000 rounds
      of a 400-element merge beside a thread calling `System.gc()` in a
      loop, no assertion ever failed — only its duration depends on the
      box. So not a `Live` tag (the law guards a real defect and belongs
      in the gate) but the suite timeout, the precedent
      sentinel-single-consumer-lost-end set for ChannelLawsSuite: 3 min,
      said in the suite's doc. Gate: `okayStreamJVM/testOnly
      okay.TestMergeScopeReachable`, after freer-base-remeasure's lanes
      finish (a gate beside a benchmark corrupts the number).
