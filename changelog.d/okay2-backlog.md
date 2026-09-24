## okay2-backlog - okay2 keeps its own backlog, `okay2/backlog.d/`

- Operator: "a separate backlog for okay2, inside". `okay2/backlog.d/`
  has the root board's layout (preamble, `_order`, one directory per
  section): row, perf, async, build. The eight okay2 items moved there
  by `git mv`; `backlog.d/okay2` stays as a pointer section.
- The review's findings are filed in it: `okay2-intersection-row` (the
  measured model, and what to check before deciding), `okay2-ci`,
  `okay2-handler-allocs` (split out of `okay2-bench`: four objects per
  handled operation, by reading, not yet measured) and
  `okay2-one-scala2-story` (two Scala 2 roads, one surface, two
  meanings of `+`).
- A module's own board is a board: `scripts/board.sh backlog okay2`
  reads it; `--check`, the pre-commit hook and `TestBoardEntries` cover
  every `<dir>/backlog.d` with the root boards — shape, open-only, and
  one slug in one place ACROSS all of them, so a promotion is still a
  `git mv` into `sprint.d/queue`. Controls run: a copy of an okay2 item
  in sprint.d/queue and a ticked okay2 item each fail the check.
