## boards-d - the boards are directories too

`changelog-d` made a landed entry its own file; the operator asked the
same question of the boards the same hour, and this is the answer.
`sprint.d/<section>/<slug>.md` and `backlog.d/<section>/<slug>.md`,
one file per item, read with `scripts/board.sh sprint|backlog`.

It was a MIGRATION rather than an additive switch, and that is why it
was its own lane: a board is read as a whole to pick work from, and
its items MOVE between two of them, so half in a file and half in a
directory would have been worse than either.

THE EVIDENCE IS A ROUND TRIP, and it was demanded before the split was
committed. Assemble the directory, normalise the original the same way
(items inside a section ordered by slug, one blank line between), and
diff: **zero lines, both boards**.

AND THEN A THIRD, INDEPENDENT CHECK, because the assembler and the
normaliser share a parser and could have dropped the same thing twice:
compare the MULTISET of non-blank lines, original against assembled.
2733 lines in the backlog and 251 in the sprint, **none lost, none
gained**. A fourth agreement fell out for free - the sh assembler
matches the throwaway Python one byte for byte.

PROMOTING IS `git mv`. backlog.d/<section>/x.md -> sprint.d/queue/x.md
is a promotion and the history records it; picking is queue -> doing;
landing is `git rm` plus changelog.d/<slug>.md.

THE OLD FILES STAY AS POINTERS. Fifteen specs and two source comments
say "filed in BACKLOG.md", and a pointer is cheaper than editing them
all to say something they do not care about - the same call
CHANGELOG.md's header made.

AND THE CHECK FOUND SOMETHING THE MOMENT IT COULD SEE THE SPRINT.
`check-citations.sh` read CHANGELOG.md and BACKLOG.md and nothing
else, so SPRINT had never been checked at all; taught to read the
directories, it immediately named a dangling sha in the schema-fold
line - that commit's PRE-REBASE object, which still exists locally, so
nothing had ever looked wrong. Corrected to the one on master.

Its own lesson, paid for twice now: the checker greps every 8-hex word
and cannot tell a citation from a MENTION of one, so a note explaining
a corrected sha must not quote the old one.
