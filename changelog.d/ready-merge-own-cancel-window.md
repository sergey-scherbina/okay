## ready-merge-own-cancel-window — the merge's park is a `Discontinue`; the 2/200 was the join

On the callback drive (`own`/`adaptive`), a cancel the drive sees
BETWEEN two operations stops it before the next one without performing
it. When that next operation was `ReadyMerge`'s park, the sources the
merge had already registered were never cancelled: 0 of 200 on `own`,
deterministically, with the fiber cancelling itself inside the
consumer's operation for the first element (Loom: 200 of 200). The
park's registration is now one object per merge that is also a
`Discontinue` whose `discontinue` is `cancelAll`, so the drive's
existing door (`Async.Drive.discontinue`, drive-discontinue) reaches
every registration: 200/200 on both. The law in `TestReadyMerge` waits
on the cancellers, not the join, and was watched red first.

The sprint item's "2 of 200 before the first park" was the JOIN: a
`DriveTask`'s `cancel()` answers the fiber at once while its drive is
still running the merge's first step; that drive then registers the
park, reads `stopped` and cancels everything (probe, 400 rounds: 400
missed at the join, 0 left registered). Its proposed fix — a `Run`
carrying the `Discontinue` at the merge's START — would not have
worked: nothing is registered then, and a performed `Run` is not the
leftmost node later.

Still open, filed as backlog `ready-merge-cancel-under-consumer-ops`: a
parked source while another keeps telling to a consumer that performs
an operation per element — the drive stops at the consumer's op, the
merge never parks, 50/50 leaked on `own`. Nothing the merge builds is
reachable there. specs/ready-merge.md Decisions and Results.
