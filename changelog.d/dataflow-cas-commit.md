## dataflow-cas-commit - the fence becomes one operation, where the store can do it

Stage 10 left one box: "a compare-and-set commit. The fence is a check
before a write, so a leader deposed between the two can land one
commit; the seam permits a conditional write and no store here offers
one."

The last clause had gone stale. `okay-docs` offers exactly that write:
`put(id, a, Cond.IfVersion(v))` applies only if the document is still
at `v` and answers `Stale` with what it holds now if it is not.

`Fencing` is the seam it needed — one method, `saveIfTerm(epoch,
bytes, term)`, meaning "write this only if `term` is still the highest
any writer has used", with `false` for "somebody newer has written".
`Checkpoint.fenced` now asks whether the journal is `Fencing` and
takes that road when it is, keeping the check-then-write for journals
that cannot.

OVER A FENCING JOURNAL THE LEASE IS NOT ASKED AT ALL, and that is the
point rather than an optimisation: asking it and then writing is what
put the gap there. The store's answer is the authority, and a test
proves it with a lease that LIES — it says the seat is still held,
which is exactly the state a deposed leader is in between the two
operations, and the write is refused anyway.

A LOG CANNOT DO THIS, and the spec now says so where it used to
imply a missing feature. Read-the-tail-then-append is two operations
with the same gap. So the two defences stand side by side and the
engine takes whichever the store can give: a cell REFUSES the stale
commit, a log SHADOWS it (`Checkpoint.newest`, highest (term, epoch)
wins, which `TestPersisted` proves by writing the ghost's record by
hand). Neither replaces the other.

THE CONTROL IS THE THIRD TEST, and it fails the feature on purpose:
over a plain `Checkpoint.Memory` the same lying lease still lands the
ghost's write. Without it the suite would be four tests agreeing that
something works, with nothing showing what it changed.

`DocsJournal` lives in okay-cluster's TEST scope — the arrangement
stage 10 used for okay-persist's `Election`, and for the same reason:
the engine's compile graph stops at okay-codec, `Checkpoint` is two
methods over bytes, and the STORE belongs to the caller. 117 green in
okay-cluster.
