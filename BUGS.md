# Bugs — the core `okay` module (`src/`)

The ledger for defects whose FIX lands in the root module's own sources
(`src/main/scala-jvm`, `scala-js`, `scala-native`, `src/main/scala`).
A module with its own sources keeps its own file beside them —
`okay-http/BUGS.md` is the other one today. This is not a leftovers
bin: an entry whose fix belongs to a module belongs in that module.

Newest first. Status lives in the machine-readable header, never in
the prose.

## own-long-join-deadlock — 10k fibers joined inside a fiber on `Schedulers.Owned.forLongTasks` never completes
<!-- status: open
     lane: jvm
     area: scheduler
     gate: none -->

`compare/src/jmh/scala/okay/AdversarialBenchmark.scala:124`,
`forkJoin10k_okayOwnLongInside` at `work = 100`, sat **62 minutes at
0.0% CPU** in JMH warmup iteration 1 and had to be killed by pid. It
is a lost wakeup, not slowness: the joining thread is parked and every
worker is parked, so nothing is left to wake anyone.

Found 2026-09-07 by `bench-refresh` at `dfde7ce3` (master + a claim
commit; no library code differs from master), JDK 21.0.7-tem, on a
quiet freshly-booted box with both scalascript launchd guards
unloaded. NOT a 143 from the guards, and not the box being busy — the
`exit 143` in that run's log is my own `kill`.

`jcmd <fork> Thread.dump_to_file -format=json`, 50 threads:

```
tid 22  ...forkJoin10k_okayOwnLongInside-jmh-worker-1
  Unsafe.park
  okay.Platform$package$$anon$6.block(Platform.scala:87)
  okay.Fiber.joinEither(Async.scala:128)
  okay.Schedulers$DriveTask.joinEither(Platform.scala:618)
  okay.Fiber.join(Async.scala:124)
  okay.AdversarialBenchmark.forkJoin10k_okayOwnLongInside(AdversarialBenchmark.scala:124)

42 threads  okay.Schedulers$Owned$Worker.run(Platform.scala:542)  — all parked
```

Repro shape: with `given Scheduler = Schedulers.owned(...).forLongTasks`,
`Async.spawn { 10 000 inner spawns, folded with joinAsync }.join()`.
The two neighbouring lanes with the SAME body and a different
scheduler policy — `forkJoin10k_okayOwnShortInside` (`forShortTasks`)
and `forkJoin10k_okayOwnInside` (the self-deciding default) — have
both measured fine for weeks, which points at the spread-at-once
policy rather than at the fold.

Platform.scala:542 is the park whose own comment names this race:
"publish `parked` and drop out of `awake` BEFORE the last look at the
queues: a submission that misses the flag has landed in a queue we are
about to see, one that sees it will unpark us." A worker that parks
between the queue check and the flag publish is the shape to look at
first — but that is a reading of the comment, not a diagnosis, and
nothing here has bisected it.

**Open questions, none answered yet.**
- Does it reproduce? Seen ONCE. Nothing has re-run it, so "intermittent"
  and "deterministic" are both still live, and the entry says so
  rather than picking one.
- Does `work = 10000` hang too? Unknown — the run died on the first
  param and never reached the second.
- Is `joinAsync` inside a spawned fiber required, or does the flat
  `forkJoin10k_okayOwn` shape on `forLongTasks` hang as well?

Until it is understood the lane is EXCLUDED from the bench-refresh
runs (`-e '.*forkJoin10k_okayOwnLongInside.*'`), so §4b's
`forkJoin10k_okayOwnLongInside` row carries no number from this
session. A gate is `none` on purpose: there is nothing to regress
against until a repro exists.
