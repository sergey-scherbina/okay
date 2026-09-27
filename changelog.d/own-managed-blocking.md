## own-managed-blocking - a fiber that blocks on an own/adaptive worker says so first

- The library's blocking doors (`CanBlock.block`, `blockAccepted`,
  `await(Handoff)` — every `join()`, `receiveBlocking`, blocking send and
  `Nio` park) now tell an `own`/`adaptive` worker before they park, the
  way `ForkJoinPool.ManagedBlocker` does. The worker stops counting as
  awake and, if work is waiting anywhere, wakes one sleeping worker; on
  `adaptive` it starts an overflow worker when nobody is asleep. Plain
  `own` only wakes. The test is a class check on the park path only.
- The case it fixes: a fiber forked from outside while a worker was
  blocked woke nobody (the blocked worker counted as awake) and waited
  for the stuck-check. On `adaptive` that took 210 ms and now takes
  1.4 ms (OwnBlockingBenchmark.outsideForkWhileBlocked, master vs the
  lane, three alternating rounds). `TestManagedBlocking` has three laws,
  all red on master first.
- What did not move: a burst of blocking fibers (the monitor already
  spreads it, and `overflow` is the ceiling), and the non-blocking path
  (spawnJoinSeq on `adaptive` 83.8 vs 83.6 us).
- Refuted and reverted: replacing the stuck-check's per-task atomic with
  a sum of per-worker counters. `adaptive` trailed `own` by only 2.4%
  before the change, and the change moved nothing measurable.
- specs/schedulers.md: the `adaptive` row no longer says it moves a fiber
  to Loom. That was never built.
- Commits: spec and laws 49c860c84, the door 6bdf378bc, the benchmark
  lane 5fefc7475, the (B) revert fbe63c81b.
