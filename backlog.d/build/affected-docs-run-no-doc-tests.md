- [ ] affected-docs-run-no-doc-tests — `scripts/gate.sh "affected master"`
      maps a lane's diff to projects (project/Affected.scala), and a lane
      that changes only docs/, specs/, READMEs, boards or changelog.d
      maps to NONE: it ran 0 tests for scala2-docs-overview (2026-09-23).
      Yet okay-deploy's doc tests read exactly those files — TestDocLinks,
      TestDocsIndex, TestDocSnippets, TestBoardEntries, TestChangelogEntries
      — so the prescribed gate for a docs lane checks none of what it
      changed. Found because the gate printed "0 test results"; the lane
      then ran `okayDeploy/test` by hand (156 green). Fix: map those paths
      (and AGENTS.md, scripts/githooks) to okayDeploy in Affected.
