## merge-scope-reachable-timeout — TestMergeScopeReachable gets a 3-minute suite timeout; the runner's three flakes were the 30 s budget under a whole-build gate

The CI runner's REPEAT OFFENDER notice (2026-09-30) on
`okay.TestMergeScopeReachable`: "Merge.Shared: a collection mid-run does
not release the merge" timed out after 30 s in three whole-build gates,
ran on to 47-51 s, and was green alone every time. The law is
deterministic — 3000 rounds of a merge beside a `System.gc()` loop, no
assertion ever failed — and only its duration is the loaded box's, so
the fix is the suite timeout ChannelLawsSuite set for the same reason
(sentinel-single-consumer-lost-end), not a `Live` tag that would take a
guard of a real defect (source-zip-lost-pairs) out of the gate. Gate:
`okayStreamJVM/testOnly okay.TestMergeScopeReachable`.
