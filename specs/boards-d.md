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

- [x] `scripts/board.sh backlog` assembled from `backlog.d/` is
      byte-identical to today's `BACKLOG.md` modulo a stated
      normalisation (trailing whitespace, one blank line between
      items) — the round trip, and the only acceptable evidence that
      2000 lines survived the split
- [x] the same for `SPRINT.md`
- [x] every item file is named `<slug>.md`, begins with the `- [ ] ` or
      `- ` line the boards use, and no two share a slug
- [x] a test in okay-deploy runs the guard, beside `TestDocsIndex` and
      `TestChangelogEntries`
- [x] `scripts/check-citations.sh` reads the directories, so a sha
      cited in an item is checked as one in `BACKLOG.md` was
- [x] AGENTS.md and the `scrumban` skill point at the directories, and
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

### Landed 2026-09-18

Every behaviour item above is met. What is worth keeping is how the
migration was checked, because "it looks right" was never going to be
enough for 2000 lines.

**THREE INDEPENDENT AGREEMENTS, not one.**

1. **The round trip.** Assemble the directory, normalise the original
   the same way (items inside a section ordered by slug, exactly one
   blank line between), diff: **zero lines, both boards.**
2. **The multiset, which does not share the parser.** The round trip's
   two halves use one parsing, so they could have dropped the same
   thing twice and agreed about it. So: compare the MULTISET of
   non-blank lines, original against assembled — 2733 lines in the
   backlog, 251 in the sprint, **none lost, none gained.**
3. **A second assembler.** The committed `scripts/board.sh` is shell;
   the migration's was a throwaway Python script. They produce the
   same bytes.

The difference that showed up afterwards was explained rather than
patched: nine lines the assembly had and master did not, which is a
sibling landing `delim-doors-are-prompted` and deleting its item while
the split was in flight — the hazard the room was warned about, caught
because the check was run against `master` rather than against the
working copy.

**THE CHECK FOUND SOMETHING THE MOMENT IT COULD SEE THE SPRINT.**
`check-citations.sh` read `CHANGELOG.md` and `BACKLOG.md` and nothing
else, so SPRINT had never been checked at all. Taught to read the
directories, it immediately named a dangling sha in the schema-fold
line: that commit's PRE-REBASE object, which still exists locally, so
nothing had ever looked wrong. Corrected to the one on master.

**And its own lesson, paid for twice now**: the checker greps every
8-hex word and cannot tell a citation from a MENTION of one, so a note
explaining a corrected sha must not quote the old one.

**What the splitter could not name**: nine items out of 218 do not
start with a slug (they are prose bullets or sub-items) and carry
`item-NNN`. They are honest rather than pretty, and whoever next
touches one can `git mv` it to a name.

**The old files stay as POINTERS**, which is the call `CHANGELOG.md`'s
header already made: fifteen specs and two source comments say "filed
in BACKLOG.md", and a pointer is cheaper than editing them all to say
something they do not care about.
