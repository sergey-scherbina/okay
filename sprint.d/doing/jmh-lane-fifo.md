- [ ] jmh-lane-fifo — the lane lock is not a queue: every queued lane polls
      `mkdir` every 10 s, so when the holder releases, whoever polls first
      wins — a lane queued 34 min lost the lock to one queued 49 s
      (2026-09-27 20:16: scheduler-default-decision's `okaySpawn`, want
      filed 19:42:33, behind a sibling's filed 20:16:07). Fix: a queued
      lane takes the lock only when its own bench-window request
      (`want/<pid>`, filed before the wait) is the OLDEST live one; ties
      by pid. Proof: jmh-lane-selftest, two lanes queued behind a holder,
      the older takes the lock first — red on master first.
