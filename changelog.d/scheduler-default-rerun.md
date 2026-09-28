## scheduler-default-rerun - the default stays Loom, now because adaptive livelocks a merge

- This lane re-ran scheduler-default-decision's table on today's code,
  loom against adaptive. Both arms were the same build, with only
  `-Dokay.scheduler` (or five-way's `okay` / `okayAdaptive`) changed.
  Part 1 was one round of the core lanes. Part 2 added a second round,
  §4 and the rest of five-way.
- Performance now allows the flip. Neither old loss is left: Wrocław 8
  fibres reads 113/112 ms against Loom's 114/110 (it was 4.4x slower),
  and five-way TCP blocking reads 155 ops/s against 117 (1.33x; it was
  0.45x). Fork/join from outside takes 0.65 of Loom's time, cancel
  0.68, `parallel8` 0.67, §4 inside 0.51, and five-way spawn/join is
  35x faster. §4 outside (0.97), fork/join inside (0.98, three rounds),
  workers work=64 (0.99) and runtime entry (0.93 ops/s) are within
  noise.
- Correctness does not allow it. With `adaptive` as the given,
  TestReadyMerge never finishes (32 min, and again 4 min, in two runs).
  One worker spins at 100% CPU in `SentinelChannel.attemptSend:351` on
  a `merge` stopped early by `take`. On Loom the same tree is 29/29
  green. The platform laws (TestSchedulerLaws, TestManagedBlocking,
  TestOwnMonitor, TestAdaptiveScheduler) are 63/64. The one red asserts
  that the property is unset.
- So `Schedulers.auto` stays `loom`. Filed backlog
  `adaptive-merge-early-stop-livelock` (okay-core), with both thread
  dumps' stacks. The flip waits for that fix.
- Spec: specs/schedulers.md, "The default, re-run". Docs:
  docs/schedulers.md. Rows:
  `src/jmh/history.d/2026-09-28T080018Z-scheduler-default-rerun.tsv`.
- Commits, all titled `scheduler-default-rerun: ...`: the spec f2294885c,
  part 1 and a preliminary verdict dc2296a23, then part 2, the laws and
  the decision.
