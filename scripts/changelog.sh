#!/bin/sh
# The changelog, assembled from one file per landed lane.
#
# WHY IT IS A DIRECTORY. Every landing used to prepend to the head of
# CHANGELOG.md, so every two lanes landing in the same hour conflicted
# on the same three lines — measured 2026-09-18, four rebases of one
# docs-only lane, each one a hand-resolved conflict whose resolution
# was always "keep both, mine on top". A conflict whose answer is
# always the same is a format problem, not a coordination problem.
#
# So a new entry is `changelog.d/<slug>.md`, named after the lane that
# landed it. Two lanes touch two files and nothing conflicts.
#
#   scripts/changelog.sh              # the new entries, newest first
#   scripts/changelog.sh --all        # ...followed by CHANGELOG.md, the archive
#   scripts/changelog.sh --check      # the naming and shape guard, for a gate
#
# ORDER IS TAKEN FROM GIT, not from the filename: each entry is dated
# by the commit that ADDED it, so nobody has to coordinate a number or
# a timestamp in a name, and an entry that is not committed yet sorts
# first (it is the newest thing there is).
set -e

dir=changelog.d
mode=${1:-new}

case "$mode" in
  --check)
    bad=0
    for f in "$dir"/*.md; do
      [ -e "$f" ] || continue
      base=$(basename "$f")
      case "$base" in
        [a-z0-9]*) ;;
        *) echo "changelog: $base — a name must start with a lowercase letter or digit" >&2; bad=1 ;;
      esac
      if ! printf '%s' "$base" | grep -Eq '^[a-z0-9][a-z0-9.-]*\.md$'; then
        echo "changelog: $base — name it after the lane, kebab-case, .md" >&2
        bad=1
      fi
      if ! head -1 "$f" | grep -q '^## '; then
        echo "changelog: $base — must begin with a '## ' title, the shape the archive uses" >&2
        bad=1
      fi
    done
    # `|| true` because an EMPTY directory makes the last command in
    # the substitution fail, and `set -e` then kills the check that is
    # supposed to say "nothing wrong here"
    titles=$(for f in "$dir"/*.md; do [ -e "$f" ] && head -1 "$f"; done || true)
    dupes=$(printf '%s\n' "$titles" | sort | uniq -d)
    if [ -n "$dupes" ]; then
      echo "changelog: two entries share a title:" >&2
      printf '%s\n' "$dupes" >&2
      bad=1
    fi
    [ "$bad" = 0 ] && echo "changelog: $(ls "$dir"/*.md 2>/dev/null | wc -l | tr -d " ") entries, all well formed"
    exit "$bad"
    ;;
esac

for f in "$dir"/*.md; do
  [ -e "$f" ] || continue
  ts=$(git log --diff-filter=A --format=%ct -1 -- "$f" 2>/dev/null || true)
  [ -n "$ts" ] || ts=9999999999
  printf '%s\t%s\n' "$ts" "$f"
done | sort -rn | cut -f2 | while read -r f; do
  cat "$f"
  echo
done

if [ "$mode" = "--all" ]; then
  # the archive: everything that landed before the directory existed
  sed '1{/^# Changelog$/d}' CHANGELOG.md
fi
