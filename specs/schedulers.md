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
  | `adaptive` | `own` plus overflow workers: a fiber that blocks through `CanBlock` says so and a spare runs the rest (managed blocking, below); a third-party blocking call is found by the monitor/stuck-check. Nothing moves a fiber to `loom` — NOT BUILT, a running platform-thread stack cannot move | holds its worker; the others keep running, up to `overflow` spares | `own`'s | nothing | when the program's shape is not known where the scheduler is chosen |

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
  `own.build` and `adaptive.build` return `Schedulers.Running`, a
  `Scheduler` that is also `AutoCloseable` and carries an `id` that
  appears in its worker thread names — a scheduler that owns threads
  must give them back, and a benchmark with three of them alive at
  once is a benchmark measuring its own thread count (found exactly
  that way, 2026-09-07). The user-facing page is `docs/schedulers.md`.
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
   is resumed (the `TestDriveScheduler` law) — AND the same holds
   when the fiber has not parked yet, which is a separate law with
   its race forced rather than hoped for (scheduler-cancel-wins,
   below).
5. `par` and `race` hold: both sides on their own thread of control,
   a child failure fails the pair and cancels the sibling.
6. NO LOST WAKE: `own` with one worker, a submitter that forks one
   fiber after the worker parked, deadline 10 s — the fiber runs.
7. FAIRNESS UNDER STARVATION (own): one worker running a fiber that
   forks 1 000 children; another submitter's fiber is answered within
   the deadline (children are stolen, the submitter's fiber is not
   behind all of them).
9. `close()` stops the workers: after it, no thread of that
   scheduler is alive (they are named `okay-own-<id>-<n>`).
8. `adaptive`: a fiber that blocks on `own` completes (`workers = 1`,
   two fibers, the first blocks on the second's channel — a deadlock
   under `own`, a delay under `adaptive`).

10. CANCEL WINS THE RACE IT IS IN (scheduler-cancel-wins,
   2026-09-07): a value delivered AFTER a cancel is never the
   fiber's answer, even when the cancel arrived before the fiber
   parked. The law forces the window instead of waiting for it —
   the registration spins uninterruptibly until the test has
   cancelled and delivered, so the fiber returns into the window on
   every run and every member.

All nine hold as of 2026-09-07: `TestSchedulerLaws`, 34 tests over
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
  correct on `loom`. HELD TO on 2026-09-20 (own-lost-wakeup): `auto`
  had been handing plain `own` to every program on a JVM without
  Loom, and okay-http's `Nio` — correct on `loom` — hung there with
  one worker in `accept()` and thirteen parked. The non-Loom pick is
  `Schedulers.platform`, `own` watched every 5 ms, and the stuck-check
  wakes a parked worker before it grows; three laws in
  `TestSchedulerLaws` say so. BUGS.md `own-lost-wakeup`.
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

## Cancel wins the race it is in (2026-09-07, scheduler-cancel-wins)

Law 4 was red about one run in three on `loom`, and only under load:
three consecutive runs went green, green, red, and a full gate went
red once. Filed with a diagnosis that turned out to be WRONG — the
first hypothesis was that a cancel arriving before the park leaves
only an interrupt flag, and a widened window (a sleep inside the
registration) refuted it in one run: a sleep throws on interrupt and
the fiber fails correctly.

The real window is one line earlier. `CanBlock.block` reads the
slot's fast path — "already filled, never waited" — BEFORE it ever
looks at the interrupt:

```scala
if slot.filled then slot.value   // never waited
else { ...the loop, which reads the interrupt FIRST... }
```

The loop had been corrected the same morning to read the interrupt
before the value; the fast path had not. A cancel and an answer that
both land while the registration is still running are therefore seen
by a fiber that has not looked at its interrupt yet, and it takes the
answer.

- [x] the fix is the same rule in the same shape, one line earlier:
      `if Thread.interrupted() then { cancel(); throw ... }` before
      the fast path. It is at the SOURCE, so it covers every member
      built on `block` rather than one of them.
- [x] the law that proves it forces the race: the registration spins
      on an `AtomicBoolean` (a plain spin, not a park, so an
      interrupt does not end it) until the test has cancelled and
      delivered. Without the fix it fails on every run; with it, 46
      of 46 laws pass over six members.
- [x] REJECTED as unnecessary: making `cancel()` also complete the
      fiber's future exceptionally (what the drive member does).
      It masks the same symptom at the fiber level, but the root
      cause is one line in `block` and covers `loom`, `forkJoin` and
      `threads` at once; a second mechanism would have been a place
      for the two to disagree.

### The lesson worth keeping
The first fix was written from a plausible mechanism and would have
shipped with a test that passed with AND without it — the test was run
both ways precisely to check that, and it did not fail without the
fix. A repair with no failing test is a guess wearing a diff.

## Every park site, and the rule stated once (2026-09-07, park-interrupt-order)

`scheduler-cancel-wins` gave `CanBlock.block` on the JVM the rule that
the interrupt is read before the answer. There are three park sites
per platform, and the rule had reached one of six:

| site | JVM before | Native before |
|---|---|---|
| `block` | fixed that morning | took the answer |
| `blockAccepted` (a blocking channel SEND) | fast path AND the loop still read the value first | took the acceptance |
| `await(Handoff)` | fast path read the value first | took the answer |

All six now read the interrupt first, in the fast path and at the top
of the loop, and refusing also withdraws the registration.

### How it is proved, after a false start
The scheduler law states the consequence — a cancelled fiber never
takes an answer that arrived after the cancel — and it CANNOT force
the window: the fiber must be asleep while both the cancel and the
answer land, and nothing in a test can hold a thread there. A law
written at that level passed with and without the fix, which is how
it was caught (the same trap as the lane before, checked for this
time).

So the rule is asserted where it lives: a caller that is ALREADY
interrupted, handed an answer that is ALREADY available, must refuse
it. That is precisely the state the racing fiber is in when it wakes,
and it is one line to set up. `TestParkInterruptOrder` does that for
all three sites, once per platform — a near-copy on purpose, because
the cross suite deliberately never touches `CanBlock` and the two
implementations share no machinery (Loom parks against wait/notify).

- [x] without the fix: 3 of 4 red on Native, 2 of 4 on the JVM (its
      `block` was already right, which is what makes the suite
      precise rather than merely red)
- [x] with it: 4 of 4 on both, and the 52 scheduler laws over six
      members still pass
- [x] the consequence law is kept beside them ("cancel wins on a
      blocking send too") and says in its own comment that it does
      not force the window — it guards the composed path, the unit
      suite proves the rule


## Two defects: local work nobody was told about (2026-09-26, own-scheduler-monitor)

### own-few-long-tasks-serial
**Symptom.** A burst of a FEW LONG fibers forked from inside a fiber
runs on one thread. The five-way benchmark (docs/benchmarks.md §4a),
8 workers x 4 096 items at work 64: `own` and `adaptive` 1 847 ops/s,
the default Loom scheduler 3 466 — `own`'s number is the serial time
(4 096 x ~135 ns = 0.54 ms). **Probe** (the five-way clone's
ThreadProbe, distinct thread ids running the work): 8 on Loom, 1 on
`own`, 1 on `own.forLongTasks` (both thresholds 0), 1 on `adaptive`.
**Cause** (Platform.scala). A fiber forked FROM a worker is
`pushLocal`ed onto that worker's deque with no CAS and no signal, and
the only thing that wakes a sleeper for local work is the helper rule,
evaluated once per 16 COMPLETED tasks (`(windowRan & 15) == 0`). This
burst is about ten tasks, so the rule never runs and its thresholds
never matter — which is why `forLongTasks` is no better.

### adaptive-short-blocking-calls
**Symptom.** Fibers that block inside a worker reach a handful of
threads on `adaptive`. Five-way blocking TCP (64 lanes, 1 ms server):
`adaptive` 5.4 batches/s against Loom's 117 and CE's 121 — the collapse
Kyo shows there too (4.9). **Probe** (64 lanes x 4 calls of a 1 ms
sleep): Loom reaches 64 concurrent calls, 8.5 ms a batch; `adaptive`
peaks at 5 concurrent calls on 5 threads, 167 ms a batch.
**Cause** (Platform.scala). The 64 lane fibers are forked from inside
one worker (its deque, no signal — the same gate as above) and that
worker blocks in the first call. The stuck-check (`watched`, every
100 ms) starts a worker only when NOTHING has completed since its last
look; the lanes keep completing something every few milliseconds, so
it sees progress and grows almost nothing.

### Decisions
- **Chosen: a monitor, Go's sysmon in miniature.** One daemon thread
  per `own` scheduler looks, every `monitorEvery` (a knob on `Own`,
  default by measurement; `unmonitored` turns it off), at each
  worker's deque and calls a worker STUCK when work waits on it and the
  owner has not moved since the last look — the owner has been inside
  one task for a whole tick while work queues behind it. For a stuck
  worker it wakes parked workers, one per waiting task (they steal);
  on a `watched` scheduler (`adaptive`, `platform`), when nobody is
  parked, it starts overflow workers the same way, up to `overflow`.
  It parks itself when no worker is awake and nothing is queued, so an
  idle scheduler has no ticking thread. The old stuck-check stays for
  the case the monitor cannot see: work in the shared submission queue
  with every worker wedged.
  *"The owner has not moved" is read from the deque's owner end*
  (`bottom`, a volatile the deque already has; it moves on every push
  and pop by the owner, never on a steal) rather than from a
  per-worker completed count: a count the monitor can trust needs a
  published write per task, the one thing the per-task path must not
  gain. A pop and a push between two looks can leave the end where it
  was and cost one spurious wake, which the woken worker answers by
  finding nothing and parking again.
- **Rejected: a signal on every local push, or on a burst of pushes**
  (ForkJoinPool's `signalWork`). It spreads long fibers at once, and it
  is exactly the cost `own` exists to avoid: sequential spawn/join on
  `own` reads 14 486 ops/s against Kyo's 6 371 because a tiny child
  stays home and nothing is woken; the kyo-shape fork/join lanes
  (~30 ns fibers, AdversarialBenchmark) are the same case.
- **Rejected: reading the clock per task** (deciding at task end
  whether a task was long). Also on the per-task path, and it decides
  too late: the long task has already run, and with a shared work
  index the first long fiber does all the work before any end is seen.
- **Rejected: a shorter stuck-check interval alone.** "Nothing
  completed" is the wrong question when a few blocked workers hold most
  of the pending work and the rest keep completing; any interval is
  defeated by one completion per interval.
- **Deferred: moving a blocking fiber to Loom** (Open boxes, above). It
  would make blocking cheap on `adaptive` rather than survivable, and
  the monitor is needed for the long-CPU case regardless.

- [x] eight 0.5 ms CPU-bound fibers forked inside an `own` fiber run
      on more than one thread (TestOwnMonitor; red on master: one)
- [x] fibers forked inside an `own` fiber that block wake the parked
      workers (TestOwnMonitor; red on master: a peak of 2 calls on 4)
- [x] on `adaptive` they also reach the overflow workers
      (TestOwnMonitor; red on master: a peak of 2 calls on 4 + 4)
- [x] the scheduler laws hold for every member (TestSchedulerLaws)
- [x] must not regress: sequential spawn/join on `own` (five-way) and
      the kyo-shape fork/join lanes (AdversarialBenchmark
      forkJoin10k_okayOwnInside / forkJoin10k_okayOwn) — within 2%
- [x] must improve: five-way workers work=64 on `own`/`adaptive`,
      blocking TCP on `adaptive`

### Results (2026-09-26)
- **As landed, two corrections to the design above, both found by
  measurement in one fork** (compare OwnMonitorBenchmark, the monitor a
  `@Param`). (1) "Stuck" is the deque's THIEF end unmoved for a tick,
  not the owner end: on a burst of 87 µs tasks the owner pops one
  between every two looks, so the owner-end test never fired — the
  burst read 709 µs with the monitor and 695 without. The thief end
  moves on every steal and when the owner pops the LAST task, so tiny
  fork/join moves it constantly and a burst being worked down leaves it
  still. With it: 254 µs. (2) The monitor parks only after ~10 ms idle.
  Parking at the first idle look put an unpark syscall on whichever
  worker woke next, and sequential spawn/join — whose workers park
  between operations — paid 7% for it (85.9 against 80.2 µs); parked
  late, 82.3 against 81.8.
- **Refuted on the way**: spurious wakes as the spawn/join cost —
  ProbeMonitorWakes counted ~110 extra wakes over 2 million tasks.
- **Tick**: 100 µs. A 1 ms tick is longer than the bursts it exists
  for (longBurst 703 µs at 1 ms, i.e. not spread).
- **Five-way** (docs/benchmarks.md §4a note): workers work=64 on `own`
  1 841 -> 3 339 ops/s, `adaptive` 1 838 -> 3 350; blocking TCP on
  `adaptive` 5.2 -> 51.2, its overflow bound being the ceiling
  (ProbeAdaptiveOverflow: 28 threads on 14 cores, 17.6 ms a batch;
  `watched(overflow = 64)` reaches 64 at 7.4 ms, Loom's 8.5).
- **The price, stated**: workers at work 0 on `own` 5 343 -> 4 097. Tiny
  steps over one shared index are faster on one core, and the monitor
  spreads work that WAITED, without knowing what it is. `own` now reads
  Loom's number there. `forShortTasks` ("never spread") turns the
  monitor off, so the shape has its builder.


## Managed blocking (2026-09-27, own-managed-blocking)

A fiber that blocks on an `own`/`adaptive` worker through the library's
own doors — `CanBlock.block`, `blockAccepted`, `await(Handoff)`, which is
every `join()`, `receiveBlocking`, blocking send and `Nio` park — SAYS SO
before it parks, and the scheduler answers at once instead of noticing a
tick later (`ForkJoinPool.ManagedBlocker`'s protocol, ours). Before this
the remedies were sampling only: the monitor (100 us, a stuck deque) and
the stuck-check (every `watched` interval: 5 ms on `platform`, 100 ms on
`adaptive`), and an `unmonitored` scheduler had the stuck-check alone.

### Design
- **The door knows its thread by its CLASS, not a ThreadLocal.** A worker
  thread is a `ManagedWorker` (a `Thread` subclass with `blocking()` /
  `unblocked()`); the door tests `Thread.currentThread()` only on its
  SLOW path — after the fast path found no answer, right before the first
  park. The non-blocking path gains nothing; a virtual or foreign thread
  fails the type test and parks as before.
- **`blocking()`** (owner thread only): the worker leaves `awake` — a
  blocked worker cannot see a submission, and counting it as awake is the
  lost wakeup own-lost-wakeup found — and, if work is waiting anywhere
  (the submission queue or ANY worker's deque: a woken worker that steals
  one of a burst and blocks in turn must pass the wake on), wakes ONE
  parked worker; when none is parked
  and the scheduler has overflow room (`watched`), it starts one, within
  `n + overflow`, exactly as the stuck-check does. **`unblocked()`**
  rejoins `awake` (and wakes a parked monitor, as a worker leaving a park
  does). Re-entrant blocking (a register that itself blocks) is counted
  once.
- **Plain `own` (no overflow) — decided:** the door WAKES a parked worker
  and never starts a thread. `own` owns `workers` threads and that is its
  contract; a sibling left on the blocked worker's deque now runs on a
  worker that was asleep, but `workers` fibers all blocked at once still
  stall the program (law 8's deadlock is kept: it is what `adaptive` is
  for).
- **Spares step down by PARKING**, as every dry worker does; an overflow
  thread is not retired (`live` only grows, bounded by `overflow`), and a
  later block wakes the parked spare instead of starting another.
- **Not covered, said so:** a third-party blocking call (JDBC, a raw
  `Thread.sleep`, a socket read not through `Nio`) does not pass a door;
  it is still the monitor's and the stuck-check's.
- **The per-task counter** (second part, its own commit) — REFUTED and
  reverted, see Results: the stuck-check's global
  `completed.incrementAndGet()` per task stays.

### Behavior
- [x] a fiber that blocks through `CanBlock` inside a worker does not stall
      a sibling forked before it: `own.workers(2).unmonitored` — the
      sibling runs while the first is blocked (red before: never, no
      stuck-check on plain `own`) (TestManagedBlocking)
- [x] on a `watched` scheduler with no parked worker, the door starts an
      overflow worker at once: `workers(1).unmonitored.watched(10 s,
      overflow = 1)` — the sibling runs well inside the interval (red
      before: it waited for the stuck-check) (TestManagedBlocking)
- [x] a blocked worker is not counted awake: `own.workers(2).unmonitored`,
      one worker blocked in the door, the other parked — a fork from
      OUTSIDE runs (red before: the blocked worker counted as awake, so
      the submission woke nobody) (TestManagedBlocking)
- [x] the scheduler laws and the monitor's tests hold (TestSchedulerLaws,
      TestOwnMonitor)
- [x] must not regress (non-blocking path): forkJoin10k inside and
      spawnJoinSeq on `own` and `adaptive`, alternating arms
      (OwnBlockingBenchmark)
- [x] must improve: a fiber forked from outside while a worker is blocked
      (OwnBlockingBenchmark.outsideForkWhileBlocked) — the burst lane
      (blockingBurst) did NOT move, see Results
- [x] the counter: `adaptive` forkJoin10k inside no slower than before;
      if `adaptive` lagged `own` by >3 % before, the gap closes — it did
      not lag: refuted, reverted

### Rejected
- the per-task clock read and a shorter stuck interval (above, the
  monitor's Decisions) — both still refuted; the door is not sampling.
- a ThreadLocal lookup in the door: `Owned.current` is per scheduler, so
  the door would need a global one; the class test is one load and a
  compare, and only on the park path.

### Results (2026-09-27)
All three laws were RED on master first (TestManagedBlocking: each
sibling or outside fork waited its 3 s deadline out) and green with the
door; TestSchedulerLaws, TestOwnMonitor and TestParkInterruptOrder hold
(59 tests). Rows: `src/jmh/history.d/2026-09-27T102953Z-own-managed-blocking.tsv`.

- **The win is the lost wakeup, not the burst.** One fiber blocked in the
  door, the other workers parked, a tiny fiber forked from OUTSIDE and
  joined, on `adaptive`: **210 ms -> 1.41 ms** (of which 1 ms is the
  lane's own sleep), three alternating rounds against master, -f 2. On
  master the blocked worker counted as awake, so the submission woke
  nobody and waited for the stuck-check (two 100 ms ticks); on
  `platform` (5 ms) the same stall is shorter but is the same stall.
- **blockingBurst did not move** (64 fibers x 4 x 1 ms forked inside a
  fiber: `adaptive` 17.36 -> 17.49 ms, `own` 29.0 -> 29.0 ms). The
  monitor already spreads a burst within a 100 us tick, and `adaptive`'s
  ceiling is `overflow` (28 threads on 14 cores). The "5-100 ms stair"
  the plan named was the monitor's to remove, and it had.
- **Non-blocking path unchanged**: spawnJoinSeq on `adaptive` 83.8 ->
  83.6 us against master (-f 2). The -f 1 rounds read +-10% either way
  on the same code (own's forkJoin10kInside, untouched by either part,
  2 998 vs 2 730), which is why only the -f 2 row is quoted.
- **(B) REFUTED, reverted.** Before it, `adaptive` trailed `own` on
  forkJoin10kInside by 2.4% (3 071 vs 2 998 us, medians) — under the 3%
  the plan set as the sign that the counter costs anything — and with the
  per-worker sum in its place the lane read 3 155 (1.03, noise). The
  kyo-shape lane keeps its fibers on one worker, so that atomic is
  uncontended there; a shape that spreads short tasks over every worker
  on a `watched` scheduler is the one place it could still show, and it
  was not measured. The global counter stays until a lane shows it.

