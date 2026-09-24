# okay2 backlog

Open work for okay2 — the okay core written a second time in Scala 2.13
(specs/okay2.md). Its own board because okay2 is its own sbt build
(operator, 2026-09-24): `scripts/board.sh backlog okay2` reads it, and
`scripts/board.sh --check` checks it with the root boards — shape,
open-only, and one slug in ONE place across every board, so a promotion
is still `git mv okay2/backlog.d/<section>/<x>.md sprint.d/queue/<x>.md`.
The sprint stays the root one: okay2 work is claimed like any other.

Findings of the first review (2026-09-24) are filed here with what was
measured; read `okay2-intersection-row` before building on the row.
