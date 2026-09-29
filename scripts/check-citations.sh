#!/bin/sh
# Every commit a ledger cites must be ON master.
#
# Written after the same mistake three times in one session: the shas
# are correct when they are typed, a rebase before the merge rewrites
# them, and the hand-run check verifies the CURRENT commit rather than
# the one the file names. A check that requires remembering what to
# check keeps failing; this one reads the file.
#
#   scripts/check-citations.sh            # CHANGELOG.md and BACKLOG.md
#   scripts/check-citations.sh FILE ...   # named files
#
# Exits non-zero and names every citation that is not an ancestor of
# HEAD. Run it from the lane's worktree immediately before
# `git merge --ff-only`: HEAD there is the tip that is about to BECOME
# master, so the lane's own commits count, which is the point.
#
# (It compared against `master` for its first hour, and the first real
# use caught that: before the merge a lane's own commits are not on
# master yet, so every fresh citation read as dangling. Checking
# against the tip being merged is the same question asked at the right
# moment.)
set -e
files="$*"
# The default set: the two ledgers, plus every entry in changelog.d —
# a sha cited in a new entry is checked exactly as one in the archive
# always was (changelog-d, 2026-09-18). The glob may match nothing on
# a tree from before the directory existed, and `[ -f ]` below skips
# the literal pattern.
# The ledgers, plus every entry of the three directories. The boards
# became directories with boards-d (2026-09-18); the globs may match
# nothing on an older tree, and `[ -f ]` below skips a literal pattern.
[ -n "$files" ] || files="CHANGELOG.md BACKLOG.md changelog.d/*.md backlog.d/*/*.md sprint.d/*/*.md"
bad=0
# A SHALLOW clone cannot answer (check-citations-shallow, 2026-09-29): its
# history stops at a graft, so a commit from before the graft that IS on
# master reads as "not an ancestor" -- a cloud session's clone reported
# `12120c2a` (2026-08-31, inside v0.1.1's history) that way. There a
# non-ancestor is said, not failed: the full clone is where the verdict is.
shallow=$(git rev-parse --is-shallow-repository 2>/dev/null || echo false)
unsure=0
for f in $files; do
  [ -f "$f" ] || continue
  # shas appear as "Landed as <sha>", "Commits: <sha> (...)", "DONE (date, <sha>)"
  for h in $(grep -oE '\b[0-9a-f]{8,40}\b' "$f" | sort -u); do
    git cat-file -e "$h^{commit}" 2>/dev/null || continue   # not a commit: a hex word
    # the TIP of a local branch is a parked lane, not a landing (a backlog
    # entry saying "preserved on feature/x (sha)"), and is not expected
    # to be an ancestor — check-citations-nine-hex, 2026-09-28
    [ -z "$(git branch --points-at "$h" 2>/dev/null)" ] || continue
    if ! git merge-base --is-ancestor "$h" HEAD 2>/dev/null; then
      if [ "$shallow" = true ]; then
        unsure=$((unsure + 1))
      else
        echo "$f: $h is a commit but is NOT an ancestor of HEAD"
        bad=1
      fi
    fi
  done
done
if [ "$unsure" -gt 0 ]; then echo "citations: $unsure cited commit(s) lie beyond this SHALLOW clone's history -- not judged here; run in a full clone"; fi
if [ "$bad" -eq 0 ] && [ "$unsure" -gt 0 ]; then echo "citations: none dangling among what this clone can see"
elif [ "$bad" -eq 0 ]; then echo "citations: all reachable from HEAD"; fi
exit $bad
