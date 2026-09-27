## adaptive-outside-long-fibers-serial - long fibers forked from outside now spread

- On `own` and `adaptive`, a burst of long fibers forked from OUTSIDE the
  scheduler (from `main`, not from a worker) ran one after another. The
  probe (ProbeOutsideLong) forked eight 70 ms fibers. They ran on 1
  thread in 560 ms, starting at 0, 70, 140 ms and so on. Loom ran them
  on 8 threads in 70 ms.
- Cause: an outside fork wakes a worker only when nobody is awake. So
  the first fork woke one worker, and the other seven waited in the
  submission queue behind it. The helper rule and the monitor looked
  only at worker deques. The stuck-check waits for a 100 ms window with
  no completion, and a 70 ms fiber completes inside every window.
- Fix: the monitor asks the submission queue the question it asks each
  deque. If the same task is still at the head a whole tick later, it
  wakes parked workers, and on `adaptive` it starts overflow workers
  when nobody is parked. Nothing was added to the fork path or the
  per-task path.
- Law: two new tests in TestOwnMonitor, for `own` and `adaptive`. Eight
  long outside fibers must use more than one thread and run at once.
  Both were red on master ("1 thread(s), 1 at once").
- Wrocław, 8 fibres under `-Dokay.scheduler=adaptive`: 262/359 ms on
  master, 110/112 with the fix. Loom reads 111/113 on the same build, so
  the gap to Loom is closed.
- The short-task lanes did not move. Outside fork/join 10k on `adaptive`
  read 1.03, inside 0.97, both within noise. spawnJoinSeq read 1.00
  over three alternating rounds.
- The default stays `loom`. specs/schedulers.md "What would reopen it"
  now says that blocking is the only open condition.
- Rows: `src/jmh/history.d/2026-09-27T233129Z-adaptive-outside-long-fibers-serial.tsv`.
- Commits: spec a728a0a0b, the law and the probe 8e2388bf5, the fix
  104f5f360.
