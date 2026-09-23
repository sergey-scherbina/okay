#!/bin/sh
# land.sh <slug> [--dry-run]
#
# The tail of AGENTS.md's "Coordination" section, as one command
# instead of eight hand-run steps. Written after counting the cost of
# skipping the tail ones by hand: 77 fully-merged feature/* branches
# nobody deleted, three ../okay-wt-* directories left holding only
# target/ build caches after a partial cleanup, and a release-claim
# commit that named a lane it had not actually claimed.
#
# WHAT IT DOES, IN ORDER, EXACTLY AS AGENTS.MD PRESCRIBES:
#   1. finds the lane's worktree (by branch, not by a path guess —
#      `../okay-wt-<slug>` is a convention, not a rule: siblings have
#      used `okay-wt-scanners`, `okay-wt-ts-t9`, `.worktrees/<slug>`)
#   2. re-gate check: if master gained SOURCE commits since the
#      branch's base, REFUSES and names them — rebasing past a source
#      change without a fresh gate is exactly the mistake this script
#      exists to prevent. Board/doc/claim-only commits are rebased
#      onto automatically; nothing to re-gate for those.
#   3. scripts/check-citations.sh, from the worktree (its own HEAD is
#      what is about to become master)
#   4. THE MERGE, from the MAIN checkout, its OWN command, exit code
#      read and printed before anything else runs
#   5. only on exit 0: worktree remove, branch delete (-d: refuses a
#      branch that is not actually merged — a safety net, not
#      ceremony), claim release (refuses if the claim file is not
#      actually tracked — the misnamed-claim incident), push
#
# Every step's exit code is printed. Nothing here forces anything:
# a step that fails STOPS the script and leaves the state for a human
# or the next run to read, rather than guessing past it.
#
# Run from the MAIN checkout (this script refuses otherwise). Tested
# under `sh` (its shebang) and `sh scripts/land.sh` is how this repo
# invokes gate.sh too, despite gate.sh's own bash shebang — one
# calling convention, sh-not-bash's rule: a script this repo runs as
# sh must be TESTED as sh, not assumed compatible.

set -e

slug="$1"
dry_run=false
if [ "$2" = "--dry-run" ]; then dry_run=true; fi

if [ -z "$slug" ]; then
  echo "usage: sh scripts/land.sh <slug> [--dry-run]" >&2
  exit 2
fi

case "$slug" in
  */* | .* | *' '*)
    echo "land.sh: refusing a slug with a path separator, leading dot or space: '$slug'" >&2
    exit 2
    ;;
esac

# --- 0. must run from the MAIN checkout -------------------------------
if [ -f .git ]; then
  echo "land.sh: this is a worktree (.git is a file, not a directory) — run from the main checkout" >&2
  exit 2
fi
main=$(pwd -P)
branch="feature/$slug"

# --- 1. find the worktree by BRANCH, not by a path guess -------------
wt=$(git worktree list --porcelain | awk -v b="refs/heads/$branch" '
  /^worktree / { path=$2 }
  /^branch /   { if ($2 == b) print path }
')
if [ -z "$wt" ]; then
  echo "land.sh: no worktree checked out on $branch (git worktree list shows none) — nothing to land" >&2
  exit 1
fi
echo "land.sh: worktree for $branch is $wt"

claim=".work/active/$slug.claim"

# --- 2. re-gate check: has master gained SOURCE commits? -------------
(
  cd "$wt"
  gap=$(git log --oneline "HEAD..master" 2>/dev/null || true)
  if [ -n "$gap" ]; then
    source_files=$(git diff --name-only "HEAD...master" -- . \
      ':!.work' ':!sprint.d' ':!backlog.d' ':!changelog.d' ':!docs' ':!specs' 2>/dev/null || true)
    if [ -n "$source_files" ]; then
      echo "land.sh: master gained SOURCE commits since this lane's base — RE-GATE before landing:"
      echo "$gap" | sed 's/^/  commit: /'
      echo "$source_files" | sed 's/^/  touches: /'
      echo "land.sh: rebase onto master, run scripts/gate.sh \"affected master\" again, then re-run land.sh"
      exit 1
    fi
    echo "land.sh: master gained board/doc/claim-only commits — rebasing (no re-gate needed):"
    echo "$gap" | sed 's/^/  commit: /'
    if [ "$dry_run" = true ]; then
      echo "land.sh: --dry-run, not rebasing"
    else
      if ! git rebase master; then
        echo "land.sh: rebase onto master CONFLICTED — stopping here, on purpose. The" >&2
        echo "land.sh: worktree is left mid-rebase; resolve and 'git rebase --continue'," >&2
        echo "land.sh: or 'git rebase --abort', then re-run land.sh. Nothing else ran:" >&2
        echo "land.sh: no citations check, no merge, no push." >&2
        exit 1
      fi
    fi
  else
    echo "land.sh: master unchanged since this lane's base"
  fi
)

# --- 3. check-citations, from the worktree ----------------------------
echo "land.sh: check-citations..."
if [ "$dry_run" = true ]; then
  echo "land.sh: --dry-run, would run: (cd $wt && sh scripts/check-citations.sh)"
else
  (cd "$wt" && sh scripts/check-citations.sh)
fi

if [ "$dry_run" = true ]; then
  echo "land.sh: --dry-run stops here. Would then:"
  echo "  git merge --ff-only $branch   (from $main, its own command)"
  echo "  git worktree remove $wt"
  echo "  git branch -d $branch"
  if git ls-files --error-unmatch "$claim" >/dev/null 2>&1; then
    echo "  git rm $claim; git commit -m 'release-claim: $slug, landed as <sha>'"
  else
    echo "  (no tracked claim file $claim — release-claim step would be skipped, with a warning)"
  fi
  echo "  git push origin master"
  exit 0
fi

# --- 4. THE MERGE: its own command, exit read before anything else ---
merge_exit=0
git merge --ff-only "$branch" || merge_exit=$?
echo "land.sh: merge exit=$merge_exit"
if [ "$merge_exit" -ne 0 ]; then
  echo "land.sh: merge failed — stopping. Nothing else ran: no worktree removal, no branch delete, no claim release, no push." >&2
  exit "$merge_exit"
fi
sha=$(git rev-parse --short HEAD)
echo "land.sh: landed as $sha"

# --- 5. worktree remove ------------------------------------------------
rm_exit=0
git worktree remove "$wt" || rm_exit=$?
echo "land.sh: worktree remove exit=$rm_exit"
if [ "$rm_exit" -ne 0 ]; then
  echo "land.sh: worktree remove failed — master is already landed ($sha) and this is safe to leave; investigate $wt by hand (no --force attempted: this script never forces a removal)." >&2
  exit "$rm_exit"
fi

# --- 6. branch delete (-d: refuses anything not actually merged) -----
br_exit=0
git branch -d "$branch" || br_exit=$?
echo "land.sh: branch -d exit=$br_exit"
if [ "$br_exit" -ne 0 ]; then
  echo "land.sh: branch delete failed — master is landed ($sha) and the worktree is gone; the branch is harmless left as-is. Investigate by hand." >&2
  exit "$br_exit"
fi

# --- 7. claim release: only if the claim is actually TRACKED ---------
if git ls-files --error-unmatch "$claim" >/dev/null 2>&1; then
  git rm -q "$claim"
  git commit -q -m "release-claim: $slug, landed as $sha

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01CRCGPc8TkvhH9YJhy2iEgH"
  echo "land.sh: release-claim committed ($(git rev-parse --short HEAD))"
else
  echo "land.sh: WARNING — $claim is not a tracked file; no claim to release. Master is landed regardless." >&2
fi

# --- 8. push — the last step, no permission needed -------------------
push_exit=0
git push origin master || push_exit=$?
echo "land.sh: push exit=$push_exit"
exit "$push_exit"
