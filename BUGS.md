# Bugs — the core `okay` module (`src/`)

The ledger for defects whose FIX lands in the root module's own sources
(`src/main/scala-jvm`, `scala-js`, `scala-native`, `src/main/scala`).
A module with its own sources keeps its own file beside them —
`okay-http/BUGS.md` is the other one today. This is not a leftovers
bin: an entry whose fix belongs to a module belongs in that module.

Newest first. Status lives in the machine-readable header, never in
the prose.

## own-long-join-deadlock — 10k fibers joined inside a fiber on `Schedulers.Owned.forLongTasks` never completes
<!-- status: fixed
     lane: jvm
     area: scheduler
     gate: src/test/scala-jvm/TestSchedulerLaws.scala "work-stealing deque — nothing is lost while it grows under thieves"
     fixed-in: 0815cbe8 -->

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

**ANSWERED, 2026-09-07 (fixed in 0815cbe8).**

Cause: `Deque.steal` cleared its slot after winning the `top` CAS. A
thief reads `top`, `bottom` and `buf` at three separate moments, so it
can be reading through an array the owner has already replaced in
`push`'s grow. Clearing writes a null into an array another thief is
still reading; that thief's CAS then SUCCEEDS while its `a.get(i)`
came back null, so the index is consumed and the task in it is never
run. One lost `DriveTask` is one fiber that never answers — hence a
`join` parked for ever with every worker legitimately idle. Canonical
Chase-Lev leaves the slot for exactly this reason.

The prediction in the claim — a lost wakeup in the park at
Platform.scala:542 — was WRONG, and the experiment that killed it is
worth keeping: replacing the worker's park with a 1 ms timed park left
the hang exactly in place, at 30% CPU instead of 0%. Workers were
waking, looking and finding nothing. Nobody had failed to signal; the
work was gone.

What it took to see it, in order, because three of these produced
nothing and that is the useful part:

| attempt | result |
|---|---|
| the shape alone, fresh scheduler per run, 200 runs | 0 |
| scheduler reused, all three pools alive (as `@Setup(Level.Trial)`), 2000 runs | 0 |
| cold JVM, 3 ops each, 250 JVM starts | 0 |
| the real JMH lane, 12 sequential runs | 0 |
| the real JMH lane, **4 in parallel** | hung on rounds 1, 4, 5, 6 |
| timed park instead of blocking park | still hangs, 30% CPU — not a lost wakeup |
| pool counters at the terminal state | `forked=40004 ran=40003`, 0 stranded, **1 steal that won its CAS on a null slot** |
| the fix alone, real blocking park | 120 forks clean |

Contention is the trigger, and only this lane's shape opens the
window: 10 000 `pushLocal` into ONE deque from capacity 256 is six
grows while thirteen thieves read through it.

Answers to the questions this entry opened with:
- Does it reproduce? Yes, but only under contention — about once in
  twenty thousand fork/join operations with four JVMs competing, which
  is why 12 sequential runs said nothing.
- Does `work = 10000` hang too? Still unknown and now moot; the defect
  was not work-size dependent.
- Is `joinAsync` inside a spawned fiber required? No. The requirement
  is a deque that GROWS while thieves read it, which that shape
  produces and the flat one does not.

Gate: the hang itself is a soak, not a gate, so the law is stated on
the deque instead — conservation, everything pushed comes out exactly
once, on the growing-under-thieves shape. It fails on the old line in
29 ms and passes on the new one.
