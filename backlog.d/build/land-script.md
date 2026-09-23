- [ ] land-script — PRIORITY: HIGH. Landing a lane is eight hand-run
      steps (rebase, gate, check-citations, ff-merge ALONE and read
      its exit, worktree remove, branch -d, claim release, push), and
      the cost of skipping the tail steps was counted today: 77
      fully-merged `feature/*` branches nobody deleted, three
      `../okay-wt-*` directories left holding only `target/` caches
      after a `worktree remove` that refused ignored files, and a
      claim commit whose message named a lane it had not claimed.
      AGENTS.md already prescribes the order and the "merge as its own
      command" discipline because a `;` after a failed merge deleted an
      unmerged branch twice. THE LANE: `scripts/land.sh <slug>` — runs
      from the MAIN checkout, refuses if master moved since the gate
      log it is given (re-gate when master gained source), runs
      check-citations in the worktree, merges `--ff-only` and STOPS on
      any non-zero, then removes the worktree (`--force` for ignored
      files only, after `git status --porcelain` is empty), deletes
      the branch with `-d`, `git rm`s the claim, commits "release-claim:
      <slug>, landed as <sha>", pushes, and prints every exit code it
      read. Tested under `sh` and bash (sh-not-bash), with a dry-run.
      (2026-09-23)
