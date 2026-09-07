# Schedulers — a family, a facade, a policy

## Overview
The operator's direction (2026-09-07, schedulers-family): schedulers
the way queues are done. A `Scheduler` is one method, `fork(prog)`,
and there is more than one right way to give a program its own
thread of control; each way has properties a caller chooses by, so
they form a FAMILY with a builder facade (as `Queues.strong.adaptive
.parts(n).each(cap)`), an ADAPTIVE member that picks by what the
program turns out to do, and — the operator's condition — one member
not slower than kyo's scheduler on kyo's own ground: many short
fibers, forked and joined as throughput.

Measured before this spec (drive-scheduler-jvm, §4b of
docs/benchmarks.md): 10 000 fork/joins cost 79 ns per fiber on kyo,
187 on a raw JDK `ForkJoinPool` with no okay in it, 231 on okay's
`Schedulers.drive` over that pool, 271 on Loom. The stack profile
named the mechanism: for fibers of tens of nanoseconds the cost is
not the work but the SPREADING of it — the JDK pool signals a
sleeping worker for nearly every task (`scan → signalWork → unpark`
9 % of its profile, workers scanning 22 %, the fiber's own walk 8 %)
while kyo keeps the work on the one or two workers already awake
(87 % of its threads parked the whole time). So the fast member is
not a faster fiber; it is a scheduler that owns its threads and
chooses where NOT to wake one.

## Design
- **The unit is `DriveTask`** (JVM): fiber, pool task and promise as
  one object; the program's tree walked by `Async.Drive` on whatever
  thread runs it; a parked Await costs its callback and nothing
  else. On JS `PromiseDrive` is the same drive answering into a
  Promise. A scheduler decides only WHICH thread and WHEN.
- **The family**, each with the properties a caller chooses by:

  | member | fiber is | blocking inside a fiber | cost per fork/join (10k, ns) | parked fiber holds | for |
  |---|---|---|---|---|---|
  | `loom` (JVM default) | a virtual thread | free — the thread parks | 271 | nothing | anything that may block: I/O, `join()`, channels |
  | `own(workers, spin, wakeAbove, helpAfterNanos, spreadAboveNanos)` | a DriveTask on an owned worker | holds one of `workers` threads | 744 inside / 1 554 outside | nothing | short CPU-bound fibers, fork/join as throughput |
  | `drive(pool)` | a DriveTask on a JDK pool | holds a pool thread | 231 | nothing | when the pool is given (a container's) |
  | `forkJoin(pool)` | a pool task, Loom-free | holds a pool thread | ~230 | nothing | a JVM without Loom |
  | `threads` | a platform thread | free | heavy (a thread start) | nothing | Native's default; a JVM that must not use Loom |
  | JS `given` | a PromiseDrive on the event loop | a compile error (no CanBlock) | — | nothing | the only one there |
  | `adaptive` | starts as `own`, moves a fiber to `loom` when it blocks | free after the move | `own`'s until a block is seen | — | when the program's shape is not known where the scheduler is chosen |

- **The facade** (`Schedulers`, scala-jvm), LANDED 2026-09-07 and
  shaped like `Queues`: `loom`, `threads`, `forkJoin(pool)` and
  `drive(pool)` are values and methods; `own` and `adaptive` are
  BUILDERS — `Schedulers.own.workers(4).forLongTasks.build`,
  `Schedulers.adaptive.workers(8).watched(200.millis).build` — with
  `workers`, `spinning`, `wakeAbove`, `helpAfter`, `spreadAbove`,
  `watched` and the two presets `forShortTasks` / `forLongTasks`.
  No trait hierarchy beyond `Scheduler`: a scheduler is a value, and
  the facade is the menu. `given Scheduler = Schedulers.loom` stays
  the JVM default: `own` holds a worker when a fiber blocks, and the
  default must be the one that cannot deadlock a correct program.
  The user-facing page is `docs/schedulers.md`.
- **The policy of `own`**, as measured rather than as first drafted
  (2026-09-07). A fiber forked FROM a worker goes on that worker's own
  Chase-Lev deque: no CAS, no signal, and the owner pops from the end
  the thieves do not touch. A fiber forked from outside goes into ONE
  shared submission queue, and wakes a worker only when nobody is
  awake to see it or when the queue is deeper than `wakeAbove` — a
  submitter that picks a random worker picks a sleeping one most of
  the time, which cost 1 249 -> 5 822 us when it was tried. A dry
  worker steals from every other worker, then the submission queue,
  spins `spin` rounds, then parks; the flag it publishes before its
  last look is what a submitter reads, so a wake is never lost.

  **The helper rule, which is what makes one scheduler hold both
  columns of the fork/join table.** At every 16th task a worker knows
  two numbers it did not have to compute: how long it has been busy
  and how many tasks that took. It wakes ONE sleeping worker — any
  sleeper, wherever it sits — only when it is past `helpAfterNanos`
  AND its tasks are averaging more than `spreadAboveNanos`. Fibers of
  thirty nanoseconds stay home, where waking a core costs more than
  the work; fibers of microseconds spread. The rule reads what the
  work IS instead of being told.
- **The adaptive policy, as landed**: what `Queues.adaptive` does for
  producers — decide by what shows up — done twice here. The helper
  rule adapts to task COST (above), and the stuck-check adapts to
  BLOCKING: every `watched(after)` the scheduler asks one question —
  is work pending while nothing at all has completed since the last
  look? — and starts one more worker if so, up to `overflow`. A fiber
  that blocks then costs latency instead of the program. No hook in
  `CanBlock` was needed, which the first draft assumed; the observable
  "pending but nothing completing" is enough, and it also catches a
  fiber wedged for a reason nobody predicted.

  The draft's other idea — moving a blocking fiber's continuation to a
  Loom thread and remembering the SITE — is NOT built, and is not
  needed for the law: it would make blocking cheap rather than
  survivable. Left in Open boxes.

## Behavior
Laws every member must pass (`TestSchedulerLaws`, parameterised by
member the way `TestManyToMany` is by buffer):
1. every fiber's answer is joined exactly once; 10 000 forks, sum.
2. a failing fiber fails its join as `Left`, with the exception.
3. `onComplete` fires exactly once whether registered before or
   after completion.
4. `cancel()` of a parked fiber: the late answer is dropped, nobody
   is resumed (the `TestDriveScheduler` law).
5. `par` and `race` hold: both sides on their own thread of control,
   a child failure fails the pair and cancels the sibling.
6. NO LOST WAKE: `own` with one worker, a submitter that forks one
   fiber after the worker parked, deadline 10 s — the fiber runs.
7. FAIRNESS UNDER STARVATION (own): one worker running a fiber that
   forks 1 000 children; another submitter's fiber is answered within
   the deadline (children are stolen, the submitter's fiber is not
   behind all of them).
8. `adaptive`: a fiber that blocks on `own` completes (`workers = 1`,
   two fibers, the first blocks on the second's channel — a deadlock
   under `own`, a delay under `adaptive`).

All eight hold as of 2026-09-07: `TestSchedulerLaws`, 33 tests over
six members (loom, drive, own, own.forShortTasks, own.forLongTasks,
adaptive). Two of them were CORRECTED by the run rather than the code:
a callback registered before completion may fire after `join` returns
(the waiters are a stack), and `cancel` on `loom` completes the fiber
exceptionally, so the law is that a LATE answer never becomes the
fiber's, not that nothing is answered.

The performance law: `forkJoin10k_okayOwnInside` within 10 % of
`forkJoin10k_kyo` on the same run, recorded in the ledger with both
numbers; a regression past that is a failing gate the way §17's rows
are. MET 2026-09-07 and then some — 744 us against kyo's 779 on its
own ground, and 3 327 against 25 419 when each fiber does real work.

## Decisions
- **`own` is not the default** until the adaptive member has its laws:
  a blocking call inside a fiber on `own` holds a worker, and the
  default must be the one that cannot deadlock a program that was
  correct on `loom`.
- **Threads are platform threads, not virtual**: a worker is a place
  to run continuations; making it a virtual thread would put a
  scheduler on a scheduler (kyo virtualises for a different reason —
  to give blocking calls somewhere to go — which is what `adaptive`
  does by moving the fiber instead).
- **The unit is `DriveTask`, not a `Runnable`**: one object, and the
  fiber object is what a later scheduler schedules; the 4 % it bought
  over four objects was measured (§4b), and it is the shape, not the
  4 %, that this family is built on.

## Open boxes
- `adaptive`'s move to Loom needs `CanBlock.block` to know which
  scheduler it is on: a `given` in the fiber's context, or the
  worker's thread-local. The thread-local is the cheap one and the
  one `own` already keeps.
- `own` on Native: the same code once Native has the worker
  primitives (`Schedulers.pool(size)` there is the JDK-pool shape).
- kyo's admission control and preemption (`Task.Preempted`) are not
  in this family's first cut; the fairness law is the placeholder.
