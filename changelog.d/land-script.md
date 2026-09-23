## land-script - `scripts/land.sh <slug> [--dry-run]`: the landing tail as one command

The eight hand-run steps after a gate (rebase, check-citations, the
solo `merge --ff-only`, worktree remove, branch delete, claim release,
push) as one script, written after counting the cost of skipping the
tail ones by hand: 77 fully-merged `feature/*` branches nobody
deleted, three `../okay-wt-*` directories left holding only `target/`
caches, and a release-claim commit that once named a lane not
actually claimed.

- Finds the lane's worktree by BRANCH via `git worktree list
  --porcelain`, not by guessing `../okay-wt-<slug>` — siblings' real
  paths this session were `okay-wt-scanners`, `okay-wt-ts-t9`.
- Re-gate check: if master gained SOURCE commits since the lane's
  base, refuses and names every commit and touched path (verified
  against a live sibling lane in `--dry-run`); board/doc/claim-only
  gaps are rebased onto automatically, no re-gate needed.
- A rebase conflict stops the script immediately, mid-rebase, with an
  explicit message naming `git rebase --continue`/`--abort` — nothing
  past it runs (verified with a real conflicting rebase).
- `check-citations.sh` runs before the merge; a real dangling sha
  (not `deadbeef00` — that fails the script's own `\b[0-9a-f]{8}\b`
  word-boundary check, which is correct — but a genuine unreachable
  commit sha) stops the script before the merge, master untouched
  (verified).
- The merge is its own command, exit read and printed before anything
  else; worktree remove, branch `-d` (refuses anything not actually
  merged), claim release and push follow only on exit 0, each exit
  code printed. A claim file that is not tracked WARNS and skips the
  release-claim commit rather than failing the landing (verified).
- Never forces anything: a failed worktree remove or branch delete
  stops the script and leaves the state for a human, rather than
  guessing past it with `--force`.
- Tested end to end in an isolated local clone with a throwaway bare
  remote (never touching origin or master): the happy path, a rebase
  conflict, a dangling citation, a missing claim, and every guard
  clause (no slug, a slug with `/`, run from inside a worktree, no
  matching branch) — five destructive-path scenarios plus four guard
  clauses, all green, none of it against the real repository. This
  changelog entry itself was landed by `land.sh` landing itself.
