#!/bin/sh
# The benchmark history, assembled from one file per measurement.
#
# WHY IT IS A DIRECTORY (history-d, 2026-09-25, the operator's ask).
# Every measurement used to append to the tail of src/jmh/history.tsv,
# so two lanes measuring in the same hour conflicted on the same last
# lines, and the resolution was always "keep both" — the reason
# changelog.d and the boards became directories, met a third time.
#
# So a measurement is `src/jmh/history.d/<when>-<measure>.tsv`:
#   <when>     the UTC instant it was recorded, `2026-09-25T013207Z`
#   <measure>  what was measured, kebab-case: `okay2-handler-allocs`
# holding the same rows history.tsv has always held — eight columns,
# TAB-separated, no header:
#   date  sha  host_load  workload  mine  ref  ratio  note
# One file per measurement, as many rows as it produced (time AND
# bytes, several lanes). Two lanes write two files; nothing conflicts,
# and the name says when and what without opening it.
#
# src/jmh/history.tsv is the ARCHIVE: everything recorded before the
# directory existed. Its rows carry dates and no times, so they stay
# where they are rather than being given invented instants; it is not
# appended to again (TestHistoryEntries holds its row count).
#
#   scripts/history.sh [pattern]      # archive + directory, oldest first, header on top
#   scripts/history.sh new <measure>  # create the file, print its path
#   scripts/history.sh --check        # the naming and shape guard, for a gate
set -e

root=$(cd "$(dirname "$0")/.." && pwd)
dir="$root/src/jmh/history.d"
archive="$root/src/jmh/history.tsv"
# the archive's rows at the switch, header included: the gate refuses
# one more (a row appended out of habit) and one fewer (an edit that
# dropped history)
frozen_lines=2024

tab=$(printf '\t')

case "${1:-}" in
  new)
    m=${2:-}
    if ! printf '%s' "$m" | grep -Eq '^[a-z0-9][a-z0-9.-]*$'; then
      echo "history: name the measurement, kebab-case: scripts/history.sh new <measure>" >&2
      exit 2
    fi
    mkdir -p "$dir"
    f="$dir/$(date -u +%Y-%m-%dT%H%M%SZ)-$m.tsv"
    if [ -e "$f" ]; then echo "history: $f exists" >&2; exit 1; fi
    : > "$f"
    echo "$f"
    ;;
  --check)
    bad=0
    n=0
    for f in "$dir"/*; do
      [ -e "$f" ] || continue
      n=$((n + 1))
      base=$(basename "$f")
      if ! printf '%s' "$base" | grep -Eq '^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{6}Z-[a-z0-9][a-z0-9.-]*\.tsv$'; then
        echo "history: $base — name a measurement <yyyy-mm-ddTHHMMSSZ>-<measure>.tsv (scripts/history.sh new <measure>)" >&2
        bad=1
        continue
      fi
      rows=$(grep -cv '^[[:space:]]*$' "$f" || true)
      if [ "$rows" = 0 ]; then
        echo "history: $base — empty; a measurement file holds at least one row" >&2
        bad=1
      fi
      # eight TAB-separated columns, the first a date: the shape every
      # reader of the history splits on (a literal "\t" has slipped in before)
      awk -F"$tab" -v f="$base" '
        /^[[:space:]]*$/ { next }
        NF != 8 { printf "history: %s line %d — %d columns, want 8 TAB-separated\n", f, NR, NF > "/dev/stderr"; bad = 1 }
        $1 !~ /^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$/ { printf "history: %s line %d — the first column is a date, got %s\n", f, NR, $1 > "/dev/stderr"; bad = 1 }
        END { exit bad }' "$f" || bad=1
    done
    lines=$(wc -l < "$archive" | tr -d ' ')
    if [ "$lines" != "$frozen_lines" ]; then
      echo "history: src/jmh/history.tsv has $lines lines, the archive froze at $frozen_lines — a new measurement is its own file: scripts/history.sh new <measure>" >&2
      bad=1
    fi
    [ "$bad" = 0 ] && echo "history: archive intact ($frozen_lines lines), $n measurement file(s), all well formed"
    exit "$bad"
    ;;
  *)
    pat=${1:-}
    {
      head -1 "$archive"
      {
        tail -n +2 "$archive"
        # file names sort by instant, so the directory reads in order
        for f in $(ls "$dir"/*.tsv 2>/dev/null | sort); do grep -v '^[[:space:]]*$' "$f"; done
      } | if [ -n "$pat" ]; then grep -i -- "$pat" || true; else cat; fi
    }
    ;;
esac
