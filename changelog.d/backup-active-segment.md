## backup-active-segment - a backup held everything except what you just wrote

`Backup.copy` copied CLOSED segments only, so a backup was bounded by
`segmentBytes` of unsaved books: append, back up, and the newest
records — the ones a shop would miss most — stayed home. The spec said
so as if it were a design ("the active segment joins next round, after
it rolls"), which is how a defect survives being written down.

The active segment travels now, under its own natural key, so nothing
about `restore` changes. What incremental means is stated instead of
implied: a closed segment is copied ONCE (they never change), the
active one whenever it has GROWN, and an idle run copies nothing.
`copy(active = false)` keeps the strict old property for a caller who
wants a second run to answer nothing more than it wants the newest
books.

**A COPY OF A LIVE FILE ENDS MID-FRAME, and that is already a shape
this store understands** — which is what made the fix small rather than
a design. Recovery's own rule, stated in `Doctor`, is that a torn tail
on the LAST segment of a partition is the ordinary crash artifact:
restorable, named, and distinguished from damage in a closed segment,
which never changes and therefore means the copy or the disk lied. A
backup of a running store is a crash that did not happen. The test
asserts exactly that path: fill without rolling, copy, restore,
`Doctor.scan` certifies it, and recovery serves all three records.

The defect was watched failing first — "the active segment was left
home, so the backup is empty" — and the OLD test's assertion ("a
closed segment copied twice") was too strong once the active file
travels; it now says the true property, with the strict variant
asserted beside it.

okay-blob 22. Found by `one-binary-story`.
