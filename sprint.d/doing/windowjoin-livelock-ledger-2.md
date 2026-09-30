- [ ] windowjoin-livelock-ledger-2 — `okay-stream/BUGS.md`
      `windowjoin-trim-spins` gets its second sighting (2026-09-30
      12:00): TestWindowJoin passed all six this time, and the SAME loop
      spun on the pool worker `okay-own-1-0` reached from `Writer.loop`
      through `Pipe`'s pull (the Source/Writer road), freezing the
      runner's family gate after TestFeedCancel; killed by PID on the
      operator's standing word. Ledger only.
