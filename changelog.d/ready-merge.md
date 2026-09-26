## ready-merge — merge by readiness on one thread of control

`Source.mergeReady(a, b, …)` and `a mergeReady b`: the merged program
keeps a ring of the SOURCES' own continuations and steps them — a told
element goes out and its source to the back of the ring, `Async.Run`
is done in the source's own turn, and an `Async.Await` is "not ready
yet": the source parks in its slot and its callback wakes the merge
through one waker cell. What crosses threads is an index per wake-up,
never an element. No fiber, no `Scheduler`, so it runs on JS as is;
with no source ever waiting it is a strict, deterministic round-robin.
Parallelism is a per-source choice (`Channel.buffer(n)(s).drained`).
`TestReadyMerge` (13) and `TestReadyMergeCross` (2); the cancel and
Run-keeps-its-turn laws were watched failing under mutants. Docs:
guide §6 (pinned example), theory ch.7 "Merging producers" (Kahn 1974,
Rust `Waker`/`select_all`), typepedia's `Channel` entry corrected
(pull CAN express readiness). Numbers NOT measured — the box never
went quiet; backlog `ready-merge-numbers`. specs/ready-merge.md.
