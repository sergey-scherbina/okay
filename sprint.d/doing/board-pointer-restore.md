- [ ] board-pointer-restore — BACKLOG.md and CHANGELOG.md's own
      pointer discipline (boards-d/changelog-d, 2026-09-18) got
      broken by 140c2539 + 481ec3c9 (the mcp-tool-authorization
      lane): its own boards commit prepended a backlog section
      (27 lines) and a changelog entry (63 lines) directly into the
      root files instead of backlog.d/changelog.d, then a
      "Merge origin/master into the tool-authorization lane's base"
      compounded it -- exactly the failure TestBoardEntries/
      TestChangelogEntries exist to catch, and it's RED on plain
      master right now, for every lane whose "affected" set reaches
      okayDeploy (confirmed: same failure reproduces on bare master,
      unrelated to whatever else is in flight).

      WHY NOW: blocks the gate for anyone, not just one lane -- found
      while gating an unrelated change (jdk-adaptive-scheduler) whose
      own diff never touches these files.

      HOW: move the two prepended pieces to where they belonged --
      BACKLOG.md's "## mcp-tool-authorization, what it opens" (3
      items) -> backlog.d/okay-security/{mcp-tools-granted,
      mcp-policy-for-resources-and-prompts,
      capability-as-a-credential-elsewhere}.md (new section --
      McpAuth/Capability live in okay-security); CHANGELOG.md's
      "## mcp-tool-authorization — a valid bearer stops meaning every
      tool" -> changelog.d/mcp-tool-authorization.md, verbatim.
      Restore both root files to their canonical pointer-only content
      (git show 7018689e:BACKLOG.md / :CHANGELOG.md's own head+archive
      -- the archive itself, everything landed before changelog-d,
      is untouched).

      DONE WHEN: TestBoardEntries + TestChangelogEntries pass;
      scripts/board.sh --check clean; gate green.
