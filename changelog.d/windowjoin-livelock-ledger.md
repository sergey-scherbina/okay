## windowjoin-livelock-ledger — okay-stream gets its BUGS.md, opened with the WindowJoin spin that hung the CI runner

Found while freer-base-remeasure waited for a quiet box (2026-09-30):
the runner's `family all` gate sat 65+ minutes on a forked okay-stream
test JVM at 104% CPU, `okay-own-1-0` RUNNABLE in `WindowJoin.trim`
under `arrive`/`left` from `Pipe`'s pull loop, `TestWindowJoin`'s fifth
test never returning. The watchdog reads burning CPU as work, so no
push and no benchmark could run. Recorded per the `bugs` skill in the
module that owns the fix, `okay-stream/BUGS.md` (new; entry
`windowjoin-trim-spins`, status open, owner stream-join-windowed, told
in the room); the thread dump is kept beside the runner's logs. Ledger
only: no source changed.
