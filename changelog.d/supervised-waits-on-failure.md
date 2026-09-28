## supervised-waits-on-failure - a failing scope answers after its children; cancel answers the fiber on every platform

- `Async.supervised` answered the FIRST failure at once, before its
  cancelled children had answered, where the success path waited
  (`whenIdle`). It now answers `Left` from `whenIdle` too, the first error
  kept in a cell so a body failing later, or succeeding later, cannot
  replace it. specs/cross-platform-async.md, two new boxes.
- What made that safe had to be built: cancel ANSWERS the fiber
  everywhere. JS's `PromiseDrive` never settled a cancelled fiber's
  promise (a `join` on it waited forever); `Schedulers.forkJoin` and
  Native's `pool` skipped a task cancelled while queued and completed
  nothing. Each now answers with a `CancellationException`.
- Found on the way: `Future.cancel(true)` does not interrupt a running
  `ForkJoinTask`, so `forkJoin` had never cancelled a running child —
  the ten-child test passed only because the scope abandoned its
  sleepers. `forkJoin` interrupts its running task itself, under a
  monitor its `finally` shares (no stale interrupt for the worker's next
  task). Native's `pool` keeps its own tracking; its `runner` is never
  reset, filed as native-pool-stale-interrupt.
- Tests: TestAsyncCross (a cancelled parked fiber still completes; the
  scope's failure comes after every child answered; the FIRST failure
  wins), TestSupervised's forkJoin queued case (watched red first),
  TestNativeScheduler's skipped task answers. 66 green on JVM, JS and
  Native. okay2's twin filed as okay2-supervised-waits-on-failure.
- Gate: `affected master staged` 5004 tests, one red — TestCoreAsyncChannelLaws'
  "six channels at once", which failed the same way in two of a sibling's
  whole-build gates the same quarter hour on an unrelated tree, drives
  raw threads (no Scheduler, no cancel — not on this diff's path), and
  ran 3/3 green alone on this tree; sighted in
  sentinel-single-consumer-lost-end.
