# Sprint

> **The sprint is [`sprint.d/`](sprint.d/), one file per item**
> (boards-d, 2026-09-18).
>
> ```
> scripts/board.sh sprint       # read it, assembled
> sprint.d/doing/<slug>.md      # what somebody is on
> sprint.d/queue/<slug>.md      # what to pick next
> ```
>
> Picking work is `git mv sprint.d/queue/<x>.md sprint.d/doing/<x>.md`
> beside the claim. Landing is `git rm` plus `changelog.d/<slug>.md`.
