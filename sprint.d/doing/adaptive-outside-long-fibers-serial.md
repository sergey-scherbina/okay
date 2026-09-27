- [ ] adaptive-outside-long-fibers-serial — found by
      scheduler-default-decision (2026-09-27): the Wrocław headline, 8
      CPU-bound fibers of ~70 ms each forked from OUTSIDE the scheduler
      (`OkayLane.parallel`, a plain `Async.spawn` per slice from `main`)
      and joined, reads 106/111 ms on Loom and 355/605 ms under
      `-Dokay.scheduler=adaptive` — the burst does not spread. Hypothesis,
      not checked: outside forks go to the one submission queue, which
      wakes a worker only when nobody is awake or it is deeper than
      `wakeAbove` (64); the monitor (own-scheduler-monitor) watches the
      workers' DEQUES, not that queue, so a few long fibers queued there
      wait for one worker to finish each. Probe first (distinct thread ids
      running the slices, as own-few-long-tasks-serial did), then a law
      (eight long fibers forked from outside reach more than one thread)
      and the fix. It is `own`'s too, and any `adaptive` user's today.
      The default stays `loom` (specs/schedulers.md, "The default"), so
      this is not blocking anything the default hands out. (2026-09-27)
