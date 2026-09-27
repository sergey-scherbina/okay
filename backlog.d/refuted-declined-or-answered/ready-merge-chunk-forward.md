- [ ] ready-merge-chunk-forward — REFUTED A THIRD TIME 2026-09-28, and
      this time the mechanism it named LANDED: the chunked merge roads
      (`Source.merge(chunked = true)`, `flushAfter`, `mergeFlushing`,
      `either`) stay on the shared channel; `ReadyMerge` gained
      poll-then-park (specs/ready-merge.md, the stage). THE TRAIL. The
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
      every fork). REOPEN only with a design in which a caught-up
      consumer costs the producers NOTHING (a producer-side batch
      signal, or a merge that runs behind on purpose), or the operator
      accepting a 1.06x mean with a 1.15-1.2x tail for one mechanism.
      The mechanism itself is worth having on its own: the elementwise
      road measured 1.01x / 0.96x / 0.98x at capacity 64 / 256 / 1024
      against registering as before (same JVM code, arms alternating).
