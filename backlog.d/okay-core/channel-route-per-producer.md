- [ ] channel-route-per-producer — a partitioned channel
      (`forProducers(n)`, `AdaptiveFifo`) routes a send by its THREAD
      (`AdaptiveFifo.route()`, a ThreadLocal home part), and per-side
      order in `Channel.merge`/`Source.merge` rides on a producer writing
      from one thread. A fiber is not a thread on `own`/`adaptive`: it
      moves when it parks and is resumed elsewhere. resume-late-withdraw
      (2026-09-29) found it the hard way — sending a foreign-answered
      resume home broke `TestMergeOrder` — and withdrew the handoff,
      which had taken the cap-64 elementwise merge from 1.12x Loom to
      0.92x. THE ASK: a route per PRODUCER (each feed of a merge owns
      its part, chosen at fork, carried by the feed), so order no longer
      depends on where a fiber runs; then bring the handoff back
      (specs/adaptive-elementwise-small-ring.md, both sections) and
      re-run TestMergeOrder at OKAY_MERGE_ROUNDS=5000. The inline
      resume moves a producer too (worker -> consumer thread, once per
      park); the per-producer route closes that as well. PRIORITY:
      MEDIUM. (2026-09-29)
