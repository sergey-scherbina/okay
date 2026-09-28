## drive-poll-then-park — the runners honour an Await's poll: Wait and Pause move down to okay-async, the blocking runner waits by the given strategy, the callback drive polls once

The open door of ready-merge-chunk-forward's second landing, opened on
the operator's question about poll and wait on the shared merge:
`Await.poll` was honoured by `ReadyMerge` alone, so `Merge.Shared`'s
consumer — a `drained` over one queue, run by the plain drive — took
`Wait` and `Pause` and used neither, as did every `drained` consumed
without a merge. `Wait` and `Pause` (and the `PlatformPause` seam,
JVM/Native real, JS empty) now live in okay-async beside the runners;
okay-async gains a `scala-js` source dir, okay-stream's is unwired.
Two runners, two rules: the BLOCKING runner (`Async.run`, the
`Handler[Async]` under `runWith`, `toLazyList`) asks an Await with a
poll by the given `Wait` on its own thread and parks only when the wait
gave up (`Async.pollThenBlock`); the CALLBACK drive (`runAsync`, fibers)
polls ONCE and never waits, because after its first callback it runs on
whoever woke it — a producer's thread as often as not — and a wait
there would stall the very producer it waits for. Five laws
(`TestPollThenPark`): answered on the yield rung with nothing
registered (121 polls), the whole ladder on a counting platform
(100/50/4/1, 154 polls) then a park, `Register` and `Spin(10)` as given,
the drive's one poll, a drained channel taking a send during the wait
without a registration. Landed on the operator's ask without a gate
(the runner's whole build follows); NOT measured yet: `okayChunkedShared`
with the wait (expected parity — its consumer never catches up) and
`bufferDrained` (expected the gain) — backlog `drive-poll-then-park-measure`.
specs/ready-merge.md (the runners' stage).
