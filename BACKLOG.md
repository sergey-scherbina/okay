# Backlog

> **The backlog is [`backlog.d/`](backlog.d/), one file per item**
> (boards-d, 2026-09-18) — a lane edits its own item instead of the
> middle of everyone's file, so two lanes filing work in the same hour
> no longer conflict.
>
> ```
> scripts/board.sh backlog          # read it, assembled, by section
> backlog.d/<section>/<slug>.md     # one item; the slug is the lane's name
> ```
>
> Filing is a new file. Promoting is `git mv backlog.d/<section>/<x>.md
> sprint.d/queue/<x>.md`. Landing is `git rm` plus
> `changelog.d/<slug>.md`. `scripts/board.sh --check` guards the shape
> and `TestBoardEntries` runs that check in the gate.
>
> Closed work is still in `BACKLOG-ARCHIVE.md`, untouched.
>
> This file stays because prose all over the repository says "filed in
> BACKLOG.md", and a pointer is cheaper than editing fifteen specs to
> say something they do not really care about.
