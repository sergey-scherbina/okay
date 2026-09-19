## queue-audit - the queue named work that had landed

Seven of the ten items in `sprint.d/queue` were finished work:
arrows-plan, proc-notation, validated, schema-fold, site-framework,
ui-product, unwrap-glyph. An eighth, `workflow-engine`, was a stage-4
item whose stage closed on 2026-09-17 and whose "earlier queue notes"
named six lanes that had since landed. A queue that names finished
work sends the next agent looking for it, which is the failure the
`scrumban` skill calls out and which the board had drifted into.

WHAT IS LEFT IS NOW WRITTEN AS WHAT IS LEFT. `static-workflow` is its
stages 2-4 with the order and the reason for it; `dataflow` is the
open half of its spec grouped by value, each group pointing at the
backlog entries that carry the detail.

AND THE SPEC WAS REPORTING ITS OWN FINISHED WORK AS OPEN. The roadmap
at the top of specs/dataflow.md carries a one-line summary per stage;
the summaries for stage 4b and stage 5 were never crossed off although
every box beneath them had been `[x]` since they landed. The sprint
repeated it for a week ("next is stage 5, failure") — so "what is next
for dataflow" had a wrong answer in two places that agreed with each
other. A summary line is a claim like any other.

The three checks that make this cheap now: `scripts/board.sh --check`,
`TestBoardEntries`, and one file per item, so deleting eight of them
is eight `git rm`s that conflict with nobody.
