- [ ] resilience-timed-under-load — `TestResilienceTimed."hedge: max
      bounds the attempts in flight"` failed once in a full gate
      (2026-09-11 00:0x, optics-outside-ops-routes) and did NOT
      reproduce alone: 4 of 4 green on unmodified master and 3 of 3 in
      the lane's worktree, 7 for 7 in isolation. The suite is timed by
      name — `Hedge.run(10, max = 3)` needs three attempts to start
      10 ms apart and the third to win at +5 ms, then asserts exact
      counters (`starts == 3`, `cancelled == 2`) — so it measures
      whether the scheduler kept up, which under a full matrix it
      sometimes does not. Untagged today, which puts a machine-speed
      question in the default gate; the policy in AGENTS.md
      ("no flaky tests in the default gate") says `Live`. Not tagged
      by that lane on purpose: one sighting is a ledger entry, not a
      verdict, and the owner should decide between tagging it and
      making the assertions bound-based rather than exact.
