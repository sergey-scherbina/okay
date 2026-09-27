## ready-merge-cancel-under-consumer-ops — a cancel reaches what a running program holds: cancel scopes on the drive

`Async.CancelScope`: a program opens a scope with its callback drive
(`own`, `adaptive`, JS) by an `Async.Run` marker the drive recognises by
class, and closes it the same way; the drive releases every scope still
open when it is cancelled — between operations, while parked, or from
outside — and when the program ENDS with one open. `mergeReady` opens one
for its run, released by its idempotent `cancelAll`. That closes the two
holes the ready merge still had on a drive: a cancel landing while the
consumer worked (the merge's code inside the consumer's continuation,
never parking) missed 50 of 50 on `own`, and a consumer that stopped
early (`take`, `runFoldUntil`) left every parked source registered. Laws
in TestReadyMerge (own and Loom) and TestReadyMergeCross (every
platform's drive), each watched red first; a mutant with an empty release
turned them red again. Price, measured on a lane written for it
(`DriveRunBenchmark`, 10 000 bare `Run`s): ~1% and 8 B per drive, after
cutting the first cut's two class tests and AtomicReference (1.5%, 24 B).
On Loom the markers are empty; an early stop there is still not a
cancellation. specs/ready-merge.md.
