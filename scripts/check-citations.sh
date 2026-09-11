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
[ -n "$files" ] || files="CHANGELOG.md BACKLOG.md"
bad=0
for f in $files; do
  [ -f "$f" ] || continue
  # shas appear as "Landed as <sha>", "Commits: <sha> (...)", "DONE (date, <sha>)"
  for h in $(grep -oE '\b[0-9a-f]{8}\b' "$f" | sort -u); do
    git cat-file -e "$h^{commit}" 2>/dev/null || continue   # not a commit: a hex word
    if ! git merge-base --is-ancestor "$h" HEAD 2>/dev/null; then
      echo "$f: $h is a commit but is NOT an ancestor of HEAD"
      bad=1
    fi
  done
done
if [ "$bad" -eq 0 ]; then echo "citations: all reachable from HEAD"; fi
exit $bad
