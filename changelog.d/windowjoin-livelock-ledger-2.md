## windowjoin-livelock-ledger-2 — windowjoin-trim-spins' second sighting: the same loop from the Source/Writer road, with TestWindowJoin green

The runner's second family gate of the afternoon passed
`TestWindowJoin` (six of six) and froze anyway: the okay-stream fork's
pool worker spun in `WindowJoin.trim` reached through `Writer.loop` and
`Pipe`'s pull, so no later suite could run. Second stack added to
`okay-stream/BUGS.md` `windowjoin-trim-spins`; the fork killed by PID on
the operator's standing word; dump beside the runner's logs. Ledger
only: no source changed.
