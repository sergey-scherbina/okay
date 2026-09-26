## ready-merge-cancel-race — the cancel law read a cancel still in flight

`TestReadyMerge`'s cancel law went red in a ci-runner whole build. It
read the sources' flags right after `f.cancel()`, but a cancel is
asynchronous (on Loom an interrupt the fiber acts on when it next
looks): a loop of that shape missed 297 of 300, and the lane's gates
had passed by luck. The law now waits for the merge to be parked
(`ReadyMerge`'s new `onPark` hook) and JOINS the fiber before reading;
watched failing under the `() => ()` canceller mutant. Measured with
the join, 200 rounds each: Loom 0 misses before or after the first
park; `own` 0 after, 2 before — a real, narrow window (a `DriveTask`
cancel is a flag the drive reads between operations, and it calls only
the canceller of the Await it last parked on). specs/ready-merge.md's
Decisions say so; backlog `ready-merge-own-cancel-window`.
