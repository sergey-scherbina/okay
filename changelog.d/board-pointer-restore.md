## board-pointer-restore — undid a bad merge that reverted boards-d/changelog-d
Landed: 2026-09-19

A merge landing the mcp-tool-authorization lane (based on a
week-stale branch, predating the boards-d/changelog-d migration)
resolved a BACKLOG.md/CHANGELOG.md conflict by keeping both sides —
which meant the lane's pre-migration edits landed on top of
origin/master's already-correct pointer-first files, breaking
`TestBoardEntries`/`TestChangelogEntries` for anyone whose gate
reached okayDeploy, regardless of what they were actually landing.

Restored both files to the known-good state (diffed byte-for-byte
against origin/master's own side of the merge first, not assumed),
filed what had been prepended properly: three backlog items to
`backlog.d/okay-security/` (new section), one changelog entry to
`changelog.d/mcp-tool-authorization.md`.
