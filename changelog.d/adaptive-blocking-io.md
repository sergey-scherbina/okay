## adaptive-blocking-io - adaptive spills waiting work to virtual threads at its bound

- Five-way blocking TCP (64 fibers, each making 1 ms raw socket calls)
  ran at 0.44x Loom on `adaptive`: 51.7 ops/s against about 117. The
  calls pass no library door, so the monitor spread the fibers over
  `n + overflow` = 28 threads, and the other 36 waited for a thread to
  come back.
- Now, when every worker the scheduler may own exists and work is still
  waiting behind blocked ones, that waiting work runs each on its own
  virtual thread. The monitor, the stuck-check and the managed-blocking
  door all do this after `grow` comes back short. A fiber that has
  already started stays on its worker, because its stack cannot move.
  This needs Loom and `overflow > 0`, so plain `own` is unchanged.
- The TCP lane now reads 151.8-153.9 ops/s against Loom's 114-121 in
  the same runs (1.31x). The batch takes 6.5 ms, down from 19.3 ms
  (Loom: 8.6). The callback transport did not move.
- Priced first with no code: `overflow = 64` reaches 139 ops/s, which
  shows the thread count was the whole cause. Growth with a retire rule
  (candidate b) was not built, because the spill beats it and adds no
  platform thread.
- Laws in TestManagedBlocking, red on master first. `n + overflow + 1`
  door-blocked fibers and their queued releaser now finish on
  `adaptive` (red: "a blocker never started"). Eight fibers in a raw
  300 ms call on `workers(2).watched(overflow = 2)` are all in the call
  at once (red: 4). Plain `own` still wedges at `n`.
- Guards against the base, alternating arms: fork/join 10k outside 0.89
  and inside 1.01 (both within noise). Wrocław on `adaptive`: 115/115
  ms against the base's 115/112, and Loom reads 111.
- Both conditions in specs/schedulers.md "What would reopen it" are now
  met. The default is still `loom`; the question can be re-run.
- Rows: `src/jmh/history.d/2026-09-28T023204Z-adaptive-blocking-io.tsv`.
  Docs: docs/schedulers.md.
- Commits: spec 34437a172, the door law 619dbb5e6, the spill c9dd7cb8e,
  the raw-blocking law 890d16cd7.
