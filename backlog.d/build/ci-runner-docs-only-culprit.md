- [ ] ci-runner-docs-only-culprit — ci-runner reverted a docs-only
      landing (a19b67d6f, 2026-09-29 15:40, `changelog.d/ci-revert-unnamed.md`)
      as the culprit of `okay.agent.TestFleet` "waited 5000ms". Two
      defects, both in the confirm step: (1) a commit that touches no
      source under a module's `src/` cannot make a test red — bisect
      should skip it (`git bisect skip`) or at least refuse to name it;
      (2) "confirmed RED on its own gate" counted a gate that was RED ON
      WARNINGS (the TestBot E176s, unrelated) as a reproduction of the
      test failure — confirmation must match the SAME `==> X` line, not
      any red. TestFleet itself is a timing flake (green alone right
      after) and belongs in the flake ledger, not in a revert. Until
      fixed: read `ci-revert-*.md` with suspicion when the lane is
      docs-only. (2026-09-29)
