## ci-runner-bisect-leftover - a bisect's leftover directory no longer blocks every bisect after it

A bisect that was killed left `../okay-ci-bisect` behind — not a
worktree any more (git had dropped the registration), just build
outputs under the path — and `git worktree add` refuses an existing
path, so every later RED on master ended in "could not create the
bisect worktree" and nothing was named, reverted or pushed: 167 commits
sat behind a red (the docs ratchet, snippet-debt-paid) that a bisect
would have found in one step. The runner now removes the path before
it adds the worktree. The leftover on this box was deleted by hand.
