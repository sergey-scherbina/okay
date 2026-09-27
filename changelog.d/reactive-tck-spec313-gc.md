## reactive-tck-spec313-gc — the GC budget reaches the GC wait

okay-reactive's `PublisherTckTest`: TCK §3.13 (cancel must make the
publisher drop the subscriber) sleeps `publisherReferenceGCTimeoutMillis`
before its `System.gc()` — the verification's SECOND constructor
argument, 300 ms by default. load-flakes (2026-09-23) gave the case a
3000 ms `TestEnvironment` timeout instead, which never reached that
wait, so the case kept failing under load at 1.6 s "inside" a budget it
never had (parquet-codec's gate, 2026-09-26). The budget now goes to the
GC wait, and the harness asserts the case waited at least that long —
watched red first (1223 ms) against the old wiring. 39 cases green.
