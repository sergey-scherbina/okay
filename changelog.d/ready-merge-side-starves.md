## ready-merge-side-starves - a merge stopped delivering one side on owned-worker schedulers; the consumer had borrowed the producer's thread

- On `own` and the adaptive default, `either`/`merge` of two endless sides
  stopped delivering the second one (rounds 16-27 of 300): the right feeder's
  `offer` woke the parked merge, `DriveTask.resumeLate` resumed the merge
  INLINE on the feeder's stack, and a merge whose other side is always ready
  never parked again — the feeder under it never ran. `Source.joinWithin`'s
  endless tests and the CI runner's two frozen gates were this.
- Fixed in `resumeLate`: in place only on a worker running no fiber now; from
  inside another fiber the answer goes home (`pushLocal`, no wake). A budget
  per drive operation was tried and refuted: the endless consumption is ONE
  operation. Measured quietly, no cost: merge cap 64 63.5 us vs 62.9, zip
  cap 7 1443 vs 1400, cap 64 351 vs 370 (src/jmh/history.d).
- The earlier "ring thirty laps ahead" reading in BUGS.md was a non-atomic
  snapshot of a live ring, corrected there.
- Tests: `TestMergeSideStarves` (own, default, loom, 300 rounds each);
  `TestSourceJoinWithin` back on every scheduler; TestOwnMonitor and
  TestAsync restated for the new rule; okay-telegram's FakeApi log race
  fixed. Gate `affected master staged` (5453 green).
- Commits: 12d56fb2a.
