# One file per item, for the boards too

## Overview

`changelog-d` (2026-09-18) made a landed entry its own file, because
every two lanes landing in the same hour conflicted on the head of
`CHANGELOG.md` and every resolution was identical. The operator asked
the obvious next question the same hour: **the boards have the same
shape — do the same for them.** They do, and the answer is yes, but
the migration is a different job from the changelog's and this spec
says why before anybody starts it.

### Why it is not the same lane

| | the changelog | a board |
|---|---|---|
| how it is read | one entry at a time, newest first | AS A WHOLE, to pick work from |
| how an item changes | never — an entry is written once | it MOVES (backlog → sprint → landed) and is edited in place |
| what order carries | the whole meaning: when things landed | grouping by module; order inside a group is incidental |
| the migration | none — the archive stays, new entries are files | LOSSLESS SPLIT of 2000 lines, or the boards are half in a file and half in a directory, which is worse than either |

So the changelog could switch additively in one commit. A board cannot:
splitting it is a migration whose only acceptable evidence is a
ROUND TRIP — assemble the directory and diff it against the file it
came from.

## Interface

```
sprint.d/<slug>.md            one item; the file IS the claim's name
backlog.d/<section>/<slug>.md one item, grouped by the module it is about
scripts/board.sh sprint       print the sprint, assembled
scripts/board.sh backlog      print the backlog, by section
scripts/board.sh --check      naming and shape, run by a test
```

- **Promotion is `git mv`**: `backlog.d/okay-ui/x.md` →
  `sprint.d/x.md`. No two lanes touch one file, and the move is
  visible in the history for free.
- **Landing is `git rm` plus `changelog.d/<slug>.md`**, which is the
  lifecycle AGENTS.md already states, with the deletion now being a
  whole file rather than a hunk.
- **A slug is the lane's name**, so a board item, its claim
  (`.work/active/<slug>.claim`) and its changelog entry all carry one
  name — which is the thing the multi-agent protocol says the slug is
  for and which nothing currently enforces.

## Behavior

- [ ] `scripts/board.sh backlog` assembled from `backlog.d/` is
      byte-identical to today's `BACKLOG.md` modulo a stated
      normalisation (trailing whitespace, one blank line between
      items) — the round trip, and the only acceptable evidence that
      2000 lines survived the split
- [ ] the same for `SPRINT.md`
- [ ] every item file is named `<slug>.md`, begins with the `- [ ] ` or
      `- ` line the boards use, and no two share a slug
- [ ] a test in okay-deploy runs the guard, beside `TestDocsIndex` and
      `TestChangelogEntries`
- [ ] `scripts/check-citations.sh` reads the directories, so a sha
      cited in an item is checked as one in `BACKLOG.md` was
- [ ] AGENTS.md and the `scrumban` skill point at the directories, and
      the lifecycle reads promote = `git mv`

## Out of scope

- Splitting `CHANGELOG.md`'s archive. Its order is what a directory of
  filenames cannot carry, and a migration commit would date every
  entry the same second. Stated in `changelog-d` and unchanged here.
- A tool that edits the boards. The files are markdown; an agent edits
  one with the same tools it edits anything.
- Ordering inside a board. A board is a set; where order matters
  (`## Doing` before `## Queue`) the directory carries it as a
  section, exactly as the file did.

## Design

**The section is a directory, not a prefix.** `BACKLOG.md` groups by
module (`## okay-parse`, `## okay-cluster / dataflow`), and that
grouping is the one piece of structure a reader uses. A directory per
section keeps it, keeps the slug as the whole filename, and makes
"which module is this about" a `git mv` rather than a re-edit.

**The round trip is the test, and it must be written BEFORE the
split.** Assemble first from the unsplit file (trivially itself),
then split, then assert the assembly is unchanged. A migration
verified by reading is a migration that quietly loses an item.

## Decisions

- **Boards after the changelog, not with it** — chosen because the
  changelog's switch is additive and the boards' is a migration;
  bundling them would have held a useful, low-risk change behind a
  long, careful one.
- **`git mv` for promotion** — chosen because it is the operation the
  lifecycle already describes, and it makes the move auditable
  without anybody writing "promoted from backlog" into prose.

## Results

(none yet — this spec is the plan, written the hour `changelog-d`
landed so the next agent starts from it rather than from the idea)
