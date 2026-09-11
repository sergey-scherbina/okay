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
# master. Run it immediately before `git merge --ff-only`, which is the
# only moment the answer is still true.
set -e
files="$*"
[ -n "$files" ] || files="CHANGELOG.md BACKLOG.md"
bad=0
for f in $files; do
  [ -f "$f" ] || continue
  # shas appear as "Landed as <sha>", "Commits: <sha> (...)", "DONE (date, <sha>)"
  for h in $(grep -oE '\b[0-9a-f]{8}\b' "$f" | sort -u); do
    git cat-file -e "$h^{commit}" 2>/dev/null || continue   # not a commit: a hex word
    if ! git merge-base --is-ancestor "$h" master 2>/dev/null; then
      echo "$f: $h is a commit but is NOT on master"
      bad=1
    fi
  done
done
if [ "$bad" -eq 0 ]; then echo "citations: all on master"; fi
exit $bad
