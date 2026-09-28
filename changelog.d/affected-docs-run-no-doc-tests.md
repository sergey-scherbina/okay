## affected-docs-run-no-doc-tests - a docs lane's `affected master` runs okay-deploy's doc tests

- `affected` mapped a diff to projects by source directory, so a lane
  that changed only docs/, specs/, a README, the boards or changelog.d
  mapped to NONE and ran 0 tests (scala2-docs-overview, 2026-09-23) —
  while TestDocLinks, TestDocsIndex, TestDocSnippets, TestBoardEntries,
  TestChangelogEntries and TestHistoryEntries read exactly those files.
- `project/Affected.scala` names what those suites read (the doc trees,
  every README/ROADMAP, AGENTS.md and the ledgers, sprint.d/backlog.d/
  changelog.d, okay2's backlog, history.d, the board scripts and hooks)
  and maps a change there to `okayDeploy` as a CHANGED project: its
  tests are the first stage, nothing is a dependent of a page, and the
  log says why ("a doc, board or ledger file changed, so okayDeploy's doc
  tests run"). `affected-selftest.sh`: `docs/tutorial.md` is 1 project
  then 0 dependents, where before it was "1 file(s) belong to no
  project". AGENTS.md's note telling a docs lane to run the suites by
  hand is gone.
