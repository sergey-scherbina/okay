- [ ] ready-merge-chunk-forward — the chunked merge roads
      (`Source.merge(chunked = true)`, `flushAfter`, `mergeFlushing`,
      `either`) onto the ring join, so there is ONE merge mechanism.
      OPERATOR DECISION 2026-09-28: (1) the bar moves — a 1.06x mean
      with a 1.15-1.2x tail in 2-4 forks of 10 is ACCEPTED for one
      mechanism; (2) before stage 2, the consumer's wait becomes the
      HYBRID — spin, then a micro-sleep (`parkNanos`; measured on this
      box: `parkNanos(1)` sleeps 12 us p50 on a platform thread, 10 us
      on a virtual one, so the producers get ~20-50 chunks ahead per
      sleep and the consumer takes batches), then register and park; the
      producer keeps its one check (`wakeOne` on an empty waiter queue is
      one read) and pays nothing until the consumer truly blocks. On JS
      there are no producer threads: spin and sleep are 0 there and the
      side registers at once (a platform hook). That lane also answers
      whether the residual tail is the polling consumer bouncing the
      producer's cache line (a sleep removes the pressure) or the
      producers' own fork-to-fork speed. THEN stage 2: the chunked
      roads onto `ReadyMerge[Chunk[A]]` (the f0f355bd4 road, rebuilt
      once already in this lane); `chunkedMerge`/`Channel.mergeChunked`
      STAY as a door by choice (operator 2026-09-28: "пусть останутся
      опционально на выбор для любителей") — the default goes to the
      ring, the shared channel is called by name; `okayChunked`/`okayChunkedFlush`/`okayChunkedFlushShort`
      at k = 16/256/1024 measured against today's road, 5 forks per arm
      alternating. LANDED FIRST, on its own (this lane's first landing):
      poll-then-park in `ReadyMerge` — the mechanism in the trail below,
      measured at parity on the elementwise road.
      THE TRAIL. The
      two refuted receive-side fixes (ring-standing-receiver) changed
      HOW a side wakes; this lane counted WHEN a side registers and
      found the storm: on the chunked ring road (f0f355bd4's, rebuilt)
      a slow fork made ~205 registrations per op, 160 of them while
      the other side still had work, 99 answered asynchronously — one
      hand-over of ONE chunk on the producer's thread each — against
      85/78/16 in a fast fork, while the merge's own parks were 8 and
      1.2. Fix: an `Async.Await` may carry a `poll`; a side whose Await
      is pollable is never registered while the merge has other work —
      it is polled when a turn passes and when the ring runs dry, and
      registered only then (`PollSpins` = 100 polls first). Measured
      (`src/jmh/history.d/2026-09-27T201816Z-ready-merge-chunk-forward-probe.tsv`,
      13 rows): registrations-with-work 160 → 0.0 in every fork, wakes
      99 → 2, fast forks 188-197 us — faster than any shared-channel
      fork — BUT the tail stayed: 2-4 of 10 forks at 215-247 in every
      configuration (spin 0/100/1000, `onSpinWait` or `yield` between
      polls), mean 205-211 against the control's 200.0 ± 2.0 (shared
      channel, same JVM code, same session: 195-204 in all 10 forks,
      never parks). The BAR (no fork > 225, no arm slower than the
      shared channel) was not met, twice, so stage 2 was not run. WHAT
      THE TAIL IS: in most slow forks the consumer WAITS ON THE
      PRODUCERS (2000-3000 polls per op against ~300, parks 1-2) — the
      ring's consumer is cheaper than the shared road's (no
      `popScanning`), so it catches up, and a caught-up consumer costs
      the producers something whatever it does (a hand-over at spin 0,
      a bouncing cache line when spinning); the shared road's consumer
      is slower than its two producers and never catches up. One slow
      fork (228 us) at 570 polls and parks 1.2 waited on nobody — plain
      fork variance. REFUTED on the way, by a counter: the merge
      resuming on the sending producer's thread after a park and
      staying there (0.0 of 4000 elements per op on a virtual thread,
      every fork). Both roads out of the tail are now the plan above: the operator
      accepted the 1.06x, and the hybrid wait IS the merge that runs
      behind on purpose.
      The mechanism itself is worth having on its own: the elementwise
      road measured 1.01x / 0.96x / 0.98x at capacity 64 / 256 / 1024
      against registering as before (same JVM code, arms alternating).
