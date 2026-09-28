- [ ] ci-runner-push-after-flake — the runner's rule "a red that does
      not reproduce alone is a flake, not a regression — not pushing;
      the next whole-build turn re-tests the range fresh"
      (scripts/ci-runner.sh) held origin/master at 09036dd58 for FOUR
      whole-build turns on 2026-09-28 (~00:14 to ~05:00), 75 commits
      behind local master, while every turn's red was a different suite
      timing out under the shared box's load — okay.clojure
      TestCoreAsyncChannelLaws/PriceInterop, okay.intent TestOfflineGate,
      scala2probe TestServicesFromScala2 (turn 1), okay.persist
      TestFileStoreRace (turn 2), TestCoreAsyncChannelLaws (turn 3),
      okay.resilience TestFaults (turn 4) — and every one was green when
      the runner re-ran it alone on HEAD, by its own criterion a flake.
      The fifth turn pushed, after two siblings had Live-tagged or
      re-timed the two recurring suites. THE PROPOSAL (operator, "Да"):
      when the re-run alone is GREEN, PUSH — the runner has already
      judged the red a flake, and a whole build that never lands on a
      shared box is a gate nobody can pass; keep "not pushing" only for
      a red that REPRODUCES alone (a regression: bisect, confirm, revert,
      as today). THE RISK, named: a real intermittent regression — a
      race the range introduced that shows only under load — would be
      pushed as a flake. MITIGATION: the runner records each flake
      sighting per suite in .work/ci/flakes/<suite> (a count and the
      range), and (a) a suite sighted N times in a row (3?) is filed as
      a backlog item by the runner and reported in the room, (b) the
      nightly full run (which already exists) is the second gate for
      what a loaded turn lets through. Also: the four suites above are
      the known repeat offenders; the "no flaky tests in the default
      gate" policy (AGENTS.md) says a timing-bound suite is `Live`-tagged
      — TestOfflineGate (180 s budget) and PriceInterop (1e6 transducer,
      an OOM once) are the two still untagged. (2026-09-28,
      ready-merge-chunk-forward's landing day)
