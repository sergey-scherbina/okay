- [ ] filestore-race-openers-flake — `okay.persist.TestFileStoreRace`
      "several openers on one empty directory all succeed" failed in a
      ci-runner whole build (2026-09-28 01:26Z, log
      .work/ci/log/20260928T012637Z-…e45c0f084…): `TestFileStore.scala:358`
      "4 of 24 opener…" — and passed alone on the same HEAD, so the runner
      called it a flake and did not push. A race law that fails under load
      is either a real race in FileStore's open path (two openers creating
      the store's first files at once) or a law asserting something the
      box's timing decides. Read the assertion and the open path first;
      reproduce under load (the channel-known-producers recipe: the suite
      beside 20 CPU burners, N rounds). Per AGENTS.md's flake policy it
      is FIXED or understood, not retried. It matters beyond itself: every
      flaky red costs ci-runner a whole-build turn, and 63 commits were
      waiting to be pushed behind three different flakes on 2026-09-28.
      (2026-09-28, found reading the runner's log)
