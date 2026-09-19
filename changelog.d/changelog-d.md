## changelog-d - a landing writes its own file, not the head of everyone's

Every landing used to prepend to the head of `CHANGELOG.md`, so two
lanes landing in the same hour conflicted on the same three lines.
Measured 2026-09-18: four rebases of ONE docs-only lane, each a
hand-resolved conflict whose resolution was always the same - keep
both, mine on top. A conflict whose answer is always identical is a
format problem, not a coordination problem.

A new entry is now `changelog.d/<slug>.md`, named after the lane that
landed it. Two lanes touch two files and nothing conflicts.

ORDER COMES FROM GIT, not from a name. `scripts/changelog.sh` dates
each entry by the commit that ADDED it, so nobody coordinates a number
or a timestamp, and an entry not yet committed sorts first - it is the
newest thing there is.

- `scripts/changelog.sh` - the new entries, newest first
- `scripts/changelog.sh --all` - ...then CHANGELOG.md, the archive
- `scripts/changelog.sh --check` - naming and shape, run by the gate

CHANGELOG.md keeps everything that landed before the switch and is not
edited again. Splitting its 925 entries was considered and refused:
their order is the one thing the file carries that the filenames do
not, and a migration commit would date them all the same second.

`scripts/check-citations.sh` reads the directory too, so a sha cited
in a new entry is checked exactly as one in the archive always was.

THE SAME SHAPE IS WANTED FOR THE BOARDS (operator, the same hour), and
it is a separate lane rather than this one: a board is READ AS A WHOLE
to pick work from, and its items MOVE between two of them, so the
migration has to be lossless and round-trip tested rather than
additive. Filed as `boards-d` with the design in specs/boards-d.md.
