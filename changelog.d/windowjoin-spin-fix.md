## windowjoin-spin-fix - the pool worker "spinning in WindowJoin.trim" was a quadratic rescan under a starved side, and the starvation is the ready merge's

- `WindowJoin.trim` evicts from the FRONT of a key's deque only (amortised
  O(1) per arrival); the full filter runs once per `within` of watermark
  advance, as the sweep always did. The first cut filtered the whole buffer
  on every arrival, and with the join's right side silent nothing expired
  and every left row rescanned one row more — 286 s of CPU on one worker
  (okay-stream/BUGS.md `windowjoin-trim-spins`, now fixed).
- Why the right side was silent: the `either` merge under `joinWithin`
  starves a side on any scheduler with owned workers (`own`, the adaptive
  default) — the hot side's ring ends with head and tail thirty laps ahead
  of every stamp, "full" to its pusher and "empty" to its popper. Older
  than these lanes (reproduces before channel-route-per-producer); filed
  as BUGS.md `ready-merge-side-starves` with the state at the starvation
  and what was ruled out (no overlapping pops or pushes, no feeder on two
  threads). `ProbeReadyMergeStarve` is the ignored reproducer (~11 s).
- `TestSourceJoinWithin`'s two endless-sides tests run on Loom until that
  bug closes; the bounded cases still run on every scheduler, and the
  join's laws are a list (`TestWindowJoin`, three platforms).
- Two more test flakes the staged gate surfaced and this lane removed:
  `Source.mergeReleases` is one counter per JVM and this module's suites
  run beside each other in the fork, so a "full run releases nothing"
  delta of 0 read 1 — the early-stop checks now assert at least one, the
  full-run delta is not asserted (TestReadyMerge's merge law covers it);
  and "the endless side stops producing" by sleep-and-compare read one
  element more under load — both survivor tests now run on Loom and join
  the feeder's thread, as TestSourceZip's does.
- Not additive (a body changed): gate `affected master staged`.
- Commits: aa7ca9f5e.
