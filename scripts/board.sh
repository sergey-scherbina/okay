#!/bin/sh
# A board, assembled from one file per item.
#
# `changelog-d` made a landed entry its own file because every two
# lanes landing in an hour conflicted on the head of one file. The
# boards have the same shape and the operator asked for the same
# treatment (specs/boards-d.md). They are not the same job: a board is
# READ AS A WHOLE to pick work from, and its items MOVE, so this was a
# migration rather than an additive switch — and its evidence was a
# round trip, asserted before the split was committed.
#
#   scripts/board.sh sprint       the sprint, assembled
#   scripts/board.sh backlog      the backlog, by section
#   scripts/board.sh --check      naming and shape, run by a test
#
# THE LAYOUT
#   <board>.d/_preamble.md        everything above the first heading
#   <board>.d/_order              the sections, in reading order
#   <board>.d/<section>/_section.md   the `## ` heading and its lead
#   <board>.d/<section>/<slug>.md     one item; the slug is the lane's name
#
# WHAT MOVES AN ITEM is `git mv`: backlog.d/okay-ui/x.md ->
# sprint.d/queue/x.md is a promotion, and the history records it for
# free. What lands it is `git rm` plus changelog.d/<slug>.md.
#
# `_order` is the only line two agents can both want to change, and
# only when a SECTION is added — which is rare, and where both sides
# want the same append.
set -e

emit() {
  dir=$1
  [ -d "$dir" ] || { echo "board: no $dir" >&2; exit 1; }
  sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' "$dir/_preamble.md"
  while read -r sec; do
    [ -n "$sec" ] || continue
    printf '\n'
    sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' "$dir/$sec/_section.md"
    for f in "$dir/$sec"/*.md; do
      [ -e "$f" ] || continue
      case "$(basename "$f")" in _section.md) continue ;; esac
      printf '\n'
      sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba' "$f"
    done
  done < "$dir/_order"
}

check() {
  bad=0
  for dir in sprint.d backlog.d; do
    [ -d "$dir" ] || continue
    for f in "$dir"/_preamble.md "$dir"/_order; do
      [ -f "$f" ] || { echo "board: $dir is missing $(basename "$f")" >&2; bad=1; }
    done
    while read -r sec; do
      [ -n "$sec" ] || continue
      [ -d "$dir/$sec" ] || { echo "board: $dir/_order names $sec, which is not there" >&2; bad=1; }
    done < "$dir/_order"
    for d in "$dir"/*/; do
      [ -d "$d" ] || continue
      sec=$(basename "$d")
      grep -qx "$sec" "$dir/_order" || { echo "board: $dir/$sec is not in _order" >&2; bad=1; }
      [ -f "$d/_section.md" ] || { echo "board: $dir/$sec has no _section.md" >&2; bad=1; }
      head -1 "$d/_section.md" | grep -q '^## ' ||
        { echo "board: $dir/$sec/_section.md must start with a '## ' heading" >&2; bad=1; }
      for f in "$d"*.md; do
        [ -e "$f" ] || continue
        base=$(basename "$f")
        case "$base" in _section.md) continue ;; esac
        printf '%s' "$base" | grep -Eq '^[a-z0-9][a-z0-9.-]*\.md$' ||
          { echo "board: $dir/$sec/$base — name an item after its lane, kebab-case" >&2; bad=1; }
        head -1 "$f" | grep -q '^- ' ||
          { echo "board: $dir/$sec/$base — an item starts with '- '" >&2; bad=1; }
        # THE OPEN BOARD HOLDS OPEN WORK (backlog-audit-0918, 2026-09-18):
        # 82 ticked entries had piled up on it since the last sweep,
        # and twelve more read as open whose work had landed. A
        # closed entry moves to BACKLOG-ARCHIVE.md, verbatim, the day
        # it closes — a check the same lane runs, so it cannot drift.
        [ "$dir" = backlog.d ] && head -1 "$f" | grep -q '^- \[x\]' &&
          { echo "board: $dir/$sec/$base is closed — move it to BACKLOG-ARCHIVE.md, verbatim" >&2; bad=1; }
      done
    done
  done
  # ONE ITEM, ONE BOARD (2026-09-23): a claim that ADDED the sprint item
  # instead of `git mv`-ing it left the backlog copy behind (80a1ccec),
  # and this check said "well formed" while TestBoardEntries failed
  # every lane's gate. The claim runs this script, not the test.
  dupes=$(for f in sprint.d/*/*.md backlog.d/*/*.md; do
            [ -e "$f" ] && basename "$f"
          done | grep -vx '_section.md' | sort | uniq -d)
  for d in $dupes; do
    echo "board: $d is filed twice — promote with git mv, not a copy:" >&2
    ls sprint.d/*/"$d" backlog.d/*/"$d" 2>/dev/null | sed 's/^/  /' >&2
    bad=1
  done
  [ "$bad" = 0 ] && echo "board: sprint.d and backlog.d are well formed"
  exit "$bad"
}

case "${1:-}" in
  --check) check ;;
  sprint) emit sprint.d ;;
  backlog) emit backlog.d ;;
  *) echo "usage: scripts/board.sh sprint|backlog|--check" >&2; exit 2 ;;
esac
