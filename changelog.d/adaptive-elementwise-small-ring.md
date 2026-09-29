## adaptive-elementwise-small-ring — a fiber woken by a foreign thread goes home

The elementwise merge at capacity 64 read 1.12x Loom on the `adaptive`
default. CPU profile: the consumer — the caller's own thread, outside the
pool — answered each waiting producer's Await as it freed a slot, and a
callback drive resumes on whoever answers, so the consumer ran the
producers' code (31% of its time) while their workers slept. A new hook,
`Async.Drive.resumeLate` (default: inline, as before), lets the JVM
`DriveTask` of `own`/`adaptive` send a late answer from a non-worker
thread back to its scheduler with `forkLong`; an answer from a worker
still runs in place. cap 64: 90.9 -> 74.5 us (Loom 81.4); cap 256/1024,
the chunked lanes and spawnJoinSeq unmoved. Laws in TestOwnMonitor (red
first). specs/adaptive-elementwise-small-ring.md; docs/schedulers.md.
