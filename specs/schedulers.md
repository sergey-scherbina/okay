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
  | `adaptive` | `own` plus overflow workers: a fiber that blocks through `CanBlock` says so and a spare runs the rest (managed blocking, below); a third-party blocking call is found by the monitor/stuck-check. Nothing moves a fiber to `loom` — NOT BUILT, a running platform-thread stack cannot move | holds its worker; the others keep running, up to `overflow` spares | `own`'s | nothing | fork/join and cancel-heavy programs whose blocked fibers stay under `n + overflow`; not the default — measured ("The default", below) |

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


## The default: loom or adaptive (2026-09-27, scheduler-default-decision)

The question: should `Schedulers.auto` hand `adaptive` rather than `loom`
to every `spawn`/`par`/`supervised` user on a JVM that has virtual
threads? The case FOR is the fork/join rows (five-way spawn/join 472
ops/s on the default against 14 486 on `own`; §4 24.0 against kyo's
18.5). The case AGAINST is the bound: Loom's blocking is free and
unbounded, `adaptive`'s is `n + overflow` threads, and a fiber blocked in
a third-party call gets the monitor's and stuck-check's latency, not
managed blocking's.

### How it is measured
- ONE switch for the arms: `-Dokay.scheduler=adaptive` (the `given`'s own
  override) through JMH's `-jvmArgsAppend`, so every lane is the SAME
  code and only the default differs — a matched pair by construction.
  `okay.AbSwitchProbe` proves the property reaches a forked JVM. The
  five-way harness pairs its `okay` runtime (the default) with
  `okayAdaptive` (`Schedulers.adaptive.build`, what the flip would hand
  out); the Wrocław lane is `runMain` forked with and without the
  property.
- Lanes: §4 (100 fibers, outside and inside), §4b (10 000, outside and
  inside — `forkJoin10k_okayInside` is added on the default for the
  pair; work=100 and 10 000), cancel 1 000 parked, `DirectParallelBenchmark
  .parallel8`, five-way (spawn/join, workers work=0/64, TCP blocking,
  runtime entry), Wrocław okay 8 fibres.
- 2 forks, 2 alternating rounds (loom, adaptive, loom, adaptive), each
  lane its own `scripts/jmh-lane.sh`; more only where the verdict hinges.
- Expected before measuring: `adaptive` wins every fork/join-inside lane
  by 2x or more and cancel by ~1.5x (§4b's `own` rows); it LOSES blocking
  TCP (five-way 2026-09-26: 51 against 117, the 28-thread ceiling); §4
  outside, `parallel8` and Wrocław (a few long CPU fibers forked from
  OUTSIDE, which land in the submission queue) are unknown, and the
  Wrocław row must not move down.

### Behavior
- [x] the table below, every row a matched pair
- [x] laws under `adaptive` as the given: TestSchedulerLaws,
      TestAdaptiveScheduler, TestManagedBlocking, TestReadyMerge
- [x] THE BOUND, as a law: `n + overflow + 1` fibers blocked at once on
      the library's own door, with the fiber that would release them
      forked after — on `loom` they all finish; on `adaptive` the bound is
      exactly `n + overflow` (that many finish, one more wedges until
      someone outside the scheduler releases it) (TestManagedBlocking)
- [x] the decision, with the table as its reason (Decision, below)

### Results, part 1 (2026-09-27) — the reduced run, one round
Worktree on master 625316caa + this lane's lane and laws; each arm its own
`scripts/jmh-lane.sh`, loom then adaptive, `-f 2 -wi 3 -i 5`, us/op (lower
better). AdversarialBenchmark carries an unrelated `shape` param, so each
fork/join cell below has two runs of 10 samples; both are shown.

| lane | loom | adaptive | loom / adaptive |
|---|---:|---:|---:|
| §4b fork/join 10k OUTSIDE, work=100 | 3 279 / 3 221 | 1 773 / 1 824 | 1.8x |
| §4b fork/join 10k OUTSIDE, work=10 000 | 3 214 / 3 260 | 3 020 / 3 051 | 1.06x |
| §4b fork/join 10k INSIDE, work=100 | 3 249 / 3 276 | 2 948 / 2 803 * | 1.1x |
| §4b fork/join 10k INSIDE, work=10 000 | 3 258 / 3 298 | 3 231 / 3 264 * | 1.0x |
| cancel 1 000 parked | 1 010 / 1 023 | 670 / 673 | 1.5x |
| `DirectParallelBenchmark.parallel8` | 10.3 | 6.7 | 1.5x |

\* the adaptive inside arm was flagged by jmh-lane's END quiet check on all
three attempts (load 18-23 with no other sbt running — the lane's own 28
threads are the likely load) and the last attempt is shown; its error bars
(5-20%) are inside what separates the cells that matter.

Already measured the same day in the five-way harness (two rounds, his
settings, `okay` = the default against `okayAdaptive`; ops/s, higher
better, `.work` results copied into the history rows):

| lane | loom | adaptive | adaptive / loom |
|---|---:|---:|---:|
| sequential spawn/join, 1 000 | 485 / 498 | 12 917 / 13 041 | 26x |
| 8 workers x 4 096, work 0 | 3 959 / 3 990 | 3 781 / 4 312 | 1.0x |
| 8 workers x 4 096, work 64 | 3 408 / 3 438 | 3 169 / 3 445 | 0.97x |
| TCP blocking, 64 lanes, 1 ms | 128.0 / 131.9 | **58.2 / 58.4** | **0.45x** |
| TCP callback, 64 lanes, 1 ms | 131.8 / 128.6 | 157.9 / 157.4 | 1.2x |
| runtime entry | 151 429 / 148 027 | 138 768 / 146 429 | 0.96x |

The TCP blocking batch is 7.7 ms on loom and 17.2 ms on adaptive (1 / ops);
the harness records no per-request latency, so there is no histogram — the
batch time is the latency this lane can state.

**Preliminary verdict: keep `loom`.** `adaptive` wins every CPU-bound
fork/join and cancel lane (1.0x-1.8x here, 26x on sequential spawn/join)
and loses the one lane where fibers block: 64 concurrent blocking calls
read 0.45x, because 64 blocked fibers meet a ceiling of `n + overflow` =
28 threads — the same bound the law in TestManagedBlocking pins as a
deadlock when the fiber that would release them is queued behind it. A
default that is 2.2x slower and can wedge on the program shape Loom makes
free is the wrong trade for the default; `adaptive` stays one import away.
Part 2 (second round, §4, Wrocław) decides.

### Results, part 2 (2026-09-27) — the second round, §4, Wrocław
Same worktree rebased on master f43cb700e (no main-code change in between);
the AdversarialBenchmark lanes pinned to `-p shape=4x4` this time. Rows:
`src/jmh/history.d/2026-09-27T192315Z-scheduler-default-decision.tsv`.

| lane | loom r1 / r2 | adaptive r1 / r2 | adaptive / loom (time) |
|---|---:|---:|---:|
| §4b 10k OUTSIDE, work=100 (us) | 3 279 / 3 098 | 1 773 / 1 903 | 0.58 |
| §4b 10k OUTSIDE, work=10 000 | 3 214 / 3 405 | 3 020 / 3 075 | 0.92 |
| §4b 10k INSIDE, work=100 | 3 249 / 3 360 | 2 948 / 2 788 * | 0.87 |
| §4b 10k INSIDE, work=10 000 | 3 258 / 3 790 | 3 231 / 3 292 * | 0.93 |
| cancel 1 000 parked | 1 010 / 1 025 | 670 / 699 | 0.67 |
| `parallel8` | 10.33 / 10.23 | 6.71 / 6.81 | 0.66 |
| §4, 100 fibers OUTSIDE (one round) | 20.4 | 12.6 | 0.62 |
| §4, 100 fibers INSIDE (one round) | 29.6 | 14.4 | 0.49 |
| **Wrocław, okay 8 fibres, ms wall (best of 5)** | **106 / 111** | **355 / 605** | **4.4** |

\* flagged again by the end quiet check (same reason as part 1). §4 got one
round each: the arms differ by 1.6x and 2.1x, well past the 10% that would
have asked for a second.

**Wrocław is the row that decides it, and it went the wrong way by 3.3-5.5x.**
Eight ~70 ms CPU fibers forked from outside the scheduler (`main`) and
joined: on Loom each is a virtual thread on the JDK's carrier pool and all
eight run at once; on `adaptive` they do not spread. Filed as backlog
`adaptive-outside-long-fibers-serial` (a hypothesis there: the monitor
watches the workers' deques, not the submission queue these land in).
Part 2 confirms part 1 and adds a reason: the headline is the one row the
item said must not move down.

**Laws with `adaptive` as the given** (`-Dokay.scheduler=adaptive` in the
test fork of okay-platform and okay-stream): TestSchedulerLaws +
TestManagedBlocking 54/54, TestReadyMerge 15/15 (its cancel laws
included), TestAdaptiveScheduler 2/3 — the one red asserts the property is
UNSET ("the default given Scheduler is auto's pick, unset"), which is the
arm itself, not a defect. So correctness is not what stands in the way.

### Decision (2026-09-27): the default stays `loom`
- **Kept: `Schedulers.auto` is `loom` where virtual threads exist.** The
  table says `adaptive` is the faster scheduler for fork/join and cancel
  (0.49-0.92 of Loom's time; 26x on five-way sequential spawn/join) and
  the slower one for two program shapes the default must serve: blocking
  (five-way TCP blocking 0.45x — 64 blocked fibers over `n + overflow` = 28
  threads) and a few long CPU fibers forked from outside (Wrocław 3.3-5.5x
  slower). A default is what a program gets without choosing, and it must
  not turn a program that was fast and correct on Loom into a slow or
  wedged one.
- **The bound, priced.** `adaptive` survives exactly `n + overflow`
  fibers blocked at once in the library's doors (the TestManagedBlocking
  law; default `n` = cores, `overflow` = cores, so 28 on this box); one
  more, with the fiber that would release them queued behind, is a
  deadlock until something outside the scheduler releases one — Loom has
  no such number. Third-party blocking (JDBC, a raw socket) is worse: it
  passes no door and costs a monitor tick or a stuck-check interval before
  a spare starts, within the same bound. Growing past the bound for door
  blocking was NOT built: an unbounded platform-thread count is the
  `threads` member's cost model, and bounded growth only moves the number.
- **What would reopen it**: `adaptive-outside-long-fibers-serial` fixed
  AND the blocking rows answered — i.e. Wrocław within noise of Loom and
  the TCP blocking lane within 10%, with the deadlock bound still stated.
  **Half met (2026-09-28):** the Wrocław half is answered. The monitor now
  watches the submission queue, and Wrocław on `adaptive` reads 110/112
  ms against Loom's 111/113 ("Outside bursts", below). Blocking is now the
  ONLY open condition: five-way TCP blocking at 0.45x and the `n +
  overflow` bound.
  **Both met (2026-09-28, adaptive-blocking-io):** five-way TCP blocking
  reads 151.8-153.9 ops/s on `adaptive` against Loom's 114-121, 1.31x
  ("Blocking past the bound", below). The deadlock bound is still
  stated, in its new form: `n + overflow` fibers blocked on platform
  threads, with any more waiting work spilled to virtual threads where
  Loom exists; with no Loom, or `overflow = 0`, the old number holds.
  The default question can now be re-run with the table in "The
  default". This lane did not flip it.
  **Re-run (2026-09-28, scheduler-default-rerun):** every performance
  row now allows the flip, and a correctness row does not. TestReadyMerge
  livelocks with `adaptive` as the given. The default stays `loom` until
  backlog `adaptive-merge-early-stop-livelock` is fixed ("The default,
  re-run", below).
  Until then the fast member is one line away and documented as the
  choice for fork/join-heavy, non-blocking programs:
  `given Scheduler = Schedulers.adaptive.build`.
- Rejected, as the item said in advance: deciding on spawn/join alone.
  On spawn/join alone the flip reads 26x; on the full table it is a loss
  on the two rows a default cannot afford.


## Outside bursts: work the monitor could not see (2026-09-27, adaptive-outside-long-fibers-serial)

**Symptom.** The Wrocław headline (compare `okay.wroclaw.OkayBench 8 5 1
8`, `OkayLane.parallel`): eight CPU-bound slices of ~70 ms, each an
`Async.spawn` from `main`, then joined. 106/111 ms on Loom, 355/605 ms
under `-Dokay.scheduler=adaptive` (scheduler-default-decision, part 2).

**Hypothesis, to be checked by a probe before any fix.** A fork from
OUTSIDE goes into the one submission queue, which wakes a worker only
when nobody is awake or the queue is deeper than `wakeAbove` (64). The
first fork wakes one worker; the other seven see it awake and wake
nobody. The helper rule reads the worker's DEQUE size (zero here), the
monitor looks only at deques, and the stuck-check fires only when
nothing completed for a whole `watched` interval (100 ms on
`adaptive`) — so the eight run mostly one after another.

### Behavior
- [x] PROBE: eight 70 ms fibers forked from outside on `adaptive.build`
      and `own.build`: distinct threads and start times (ProbeOutsideLong)
- [x] LAW: eight long fibers forked from OUTSIDE onto parked workers run
      on more than one thread and overlap in time, on `own` and on
      `adaptive` (TestOwnMonitor) — red on master first
- [x] the fix at the cause the probe names; nothing added to the
      per-task or per-fork path
- [x] must not regress (short tasks stay home): AdversarialBenchmark
      forkJoin10k_okay (outside) and forkJoin10k_okayInside on
      `adaptive`, OwnMonitorBenchmark.spawnJoinSeq — alternating arms
- [x] must improve: Wrocław 8 fibres under `-Dokay.scheduler=adaptive`,
      two rounds; the verdict against Loom stated below

### Probe (master 66fa8351f)
ProbeOutsideLong, eight 70 ms spins forked from the test thread after
50 ms idle, three rounds per member (start times in ms after the first
fork):

| member | threads | wall | starts |
|---|---:|---:|---|
| `adaptive` | 1, 1, 2 | 568, 560, 280 ms | 0, 70, 140, 210, 280, 350, 420, 490 |
| `own` | 2, 2, 2 | 280 ms | 0, 0, 70, 70, 140, 140, 210, 210 |
| `loom` | 8 | 70-74 ms | all at 0-3 |

**The hypothesis holds, and the probe adds two details.** (1) The
fibers run strictly one after another: each one starts when the one
before it ends. The first fork finds `awake == 0` and wakes a worker.
The next seven see it awake and wake nobody. That worker's helper rule
checks its deque (`size > 0`), and the deque is empty because the
fibers sit in the submission queue. The monitor looks only at deques.
(2) The stuck-check does not rescue it. It fires only when NOTHING
completed in a whole `watched` interval (100 ms), and a 70 ms fiber
completes inside every interval. The two threads on `own` are a race,
not a rescue: the second fork arrived before the first woken worker had
counted itself awake.

### The fix
The monitor asks of the submission queue the question it already asks
of each deque: has the work at its head waited a whole look? It reads
`submissions.peek()` each tick. If the head is the SAME task it saw
last tick, it wakes parked workers, one per waiting submission, and on
a `watched` scheduler it starts overflow workers when nobody is parked.
That is `look()`'s rule for a stuck deque, unchanged. The fork path and
the per-task path gain nothing. A worker taking tiny submissions changes
the head every few nanoseconds, so short fibers from outside stay with
the workers already awake. `forShortTasks` (monitor off) still never
spreads.

- Rejected: waking a worker on every outside fork, or waking when a
  worker is awake but BUSY. Both are the per-fork signal that `own` was
  measured without (1 249 -> 5 822 us when the victim was random,
  "The policy of own"), and both decide before anyone knows whether the
  fiber is long.
- Rejected: shortening the stuck-check. "Nothing completed" is the
  wrong question (own-scheduler-monitor's Decisions), and here one
  completion every 70 ms defeats any interval longer than that.
- Rejected: counting a worker's submissions into its helper rule. The
  rule runs every 16th completed task, and eight fibers complete fewer
  than 16 before the burst is over (own-few-long-tasks-serial again).

### Results (2026-09-28)
Law red on master: TestOwnMonitor's two new tests, "a round used 1
thread(s), 1 at once, on four workers" on `own` and on `adaptive`.
Green with the fix, together with the monitor's other three tests. The
probe with the fix: 8 threads on both, wall 76-127 ms on a loaded box
(Loom 84-101 ms in the same run).

A/B: master 66fa8351f (ref) against the lane, each lane its own
`scripts/jmh-lane.sh`, arms alternating, `-f 2 -wi 3 -i 5`. Rows:
`src/jmh/history.d/2026-09-27T233129Z-adaptive-outside-long-fibers-serial.tsv`.

| lane | master r1 / r2 | lane r1 / r2 | lane / master |
|---|---:|---:|---:|
| **Wrocław, okay 8 fibres, `adaptive`, ms wall (best of 5)** | **262 / 359** | **110 / 112** | **0.36** |
| Wrocław, okay 8 fibres, Loom (lane build) | — | 111 / 113 | — |
| forkJoin10k_okay OUTSIDE, work=100, `adaptive` (us) | 2 203 * / 2 164 * | 2 200 * / 2 291 * | 1.03 (pooled medians 2 246 / 2 184) |
| forkJoin10k_okayInside, work=100, `adaptive` (us) | 3 230 / 2 888 * | 3 055 * / 2 821 * | 0.97 (pooled medians 2 972 / 3 059) |
| OwnMonitorBenchmark.spawnJoinSeq, `own`, monitor 100 us (us) | 73.8 / 87.2 / 92.1 | 86.8 / 83.6 / 88.0 | 1.00 (medians 86.8 / 87.2) |

spawnJoinSeq got a third pair, run lane-first. The first two read 1.18x
and 0.96x, and master alone moved from 73.8 to 92.1 across its three
runs.

\* median of every attempt jmh-lane.sh made. The fork/join lanes read
+-10-30% within a run on this box, on BOTH builds: master's outside lane
gave up after five noisy attempts in round 1, and so did the lane's
inside lane. The pooled medians are the comparison, and both are inside
the noise. Nothing here moves the short-task lanes.

**Verdict: the Wrocław gap to Loom is closed.** `adaptive` reads 110/112
ms against Loom's 111/113 on the same build and box (it was 3.3-5.5x
slower in scheduler-default-decision, and 2.4-3.2x in this lane's master
arm). One of the two conditions in "What would reopen it" is met. The
other, blocking TCP within 10% of Loom, is not, so the default stays
`loom` (see the note there).


## Blocking past the bound (2026-09-28, adaptive-blocking-io)

**Symptom.** Five-way TCP blocking, 64 lanes x 1 ms
(`bench.io.IoBench.requests`, `transport=blocking`): `adaptive` 58
ops/s against Loom's 128-132, 0.45x (scheduler-default-decision). And
the bound law: `n + overflow` fibers blocked in the library's doors,
with their releaser queued behind them, wedge until something outside
the scheduler releases one.

**What kind of blocking the lane is — read before pricing anything.**
The five-way okay backend makes the call as `async(io.blocking(lane,
index))`: a RAW socket read inside a `Run`, not a library door. So
managed blocking never hears of it. Only the monitor sees it: the 64
lane fibers are forked from one fiber onto one worker's deque, and the
monitor spreads them to parked workers and then overflow workers, up to
`n + overflow` = 28 threads. The other 36 wait until a thread comes
back. Each lane fiber makes its four calls one after another, so the
batch runs in about three waves of ~5 ms: 17.2 ms against Loom's 7.7.
Candidate (b) as the item names it (growth for DOOR blocking only)
cannot move this lane. Whatever moves it must act on what the monitor
sees.

**Candidates, each priced alone, in this order:**
- (b0) a knob, no code: `Schedulers.own.watched(overflow = 64)` in the
  harness. It prices "is it only the thread count?" before anything is
  built.
- (a) SPILL TO VIRTUAL THREADS AT THE BOUND. When the monitor, the
  stuck-check or the door wants to start a worker and `live` is already
  `n + overflow`, the waiting task (from the stuck worker's deque or the
  submission queue) runs on its own virtual thread instead of waiting.
  That is only on a JVM with Loom and only on a `watched` scheduler. A
  fiber that started on a worker cannot move: its stack is there. What
  moves is work that has not STARTED. A spilled fiber runs on its
  virtual thread until its drive parks on an `Await`. It then resumes
  wherever its answer arrives, which is the Drive's rule on every
  member. So neither "it stays on the virtual thread" nor "it returns to
  a worker" is a policy to choose: the existing rule decides. A raw
  socket read on the virtual thread unmounts. A door parks it. Its forks
  go to the submission queue (it is not a worker). Nothing is added to
  the per-task or per-fork path.
- (b) growth past `overflow` with a retire rule. Priced by (b0). It is
  built only if (b0) reaches Loom and (a) does not.
- (c) keep the bound and say so.

### Behavior
- [x] reproduce on master: five-way TCP blocking, `okay` (Loom) vs
      `okayAdaptive`, his settings
- [x] (b0) priced
- [x] (a) priced. If it lands: the bound law changes. `n + overflow`
      door-blocked fibers plus their queued releaser FINISH on
      `adaptive` where Loom exists (red first on master). The old bound
      still holds where nothing can spill (no Loom, or `overflow = 0`),
      and that is pinned too (TestManagedBlocking)
- [x] the TCP shape as a law: eight fibers in a raw 300 ms call on
      `workers(2).watched(overflow = 2)` are all in the call at once (red
      on the base: 4 = `n + overflow`) (TestManagedBlocking)
- [x] TestSchedulerLaws, TestOwnMonitor, TestAdaptiveScheduler green
- [x] must not regress: AdversarialBenchmark forkJoin10k_okay and
      forkJoin10k_okayInside on `adaptive`, Wrocław 8 fibres on
      `adaptive` (~110 ms). OwnMonitorBenchmark spawnJoinSeq runs on
      plain `own`, where every changed line is behind `overflow > 0`, so
      it was not run.
- [x] BAR: TCP blocking within 10% of Loom. Met: "What would reopen it"
      says both conditions are met. The default is NOT flipped here.

### Results (2026-09-28)
Harness: his repository cloned at 82ac6f1 into the lane's `.work/`,
patched by `compare/five-way/apply.py`, okay published locally at a
private version per arm (`0.2.0-abi-*`, so no sibling's `SNAPSHOT`
moved), run by `compare/five-way/run.sh` with `FIVE_WAY_LANES=tcp`. His
settings, `-f 3 -wi 3 -i 3`. Rows:
`src/jmh/history.d/2026-09-28T023204Z-adaptive-blocking-io.tsv`.

| TCP, 64 lanes x 1 ms (ops/s, higher better) | Loom (`okay`) | `adaptive` | adaptive / Loom |
|---|---:|---:|---:|
| master, 3 rounds (two bases) | 118.8 / 114.4 / 117.1 | 51.8 / 51.5 / 51.7 | 0.44 |
| (b0) `own.watched(overflow = 64)`, no code | — | 139.3 | 1.19 |
| **(a) spill, 3 rounds** | 120.5 / 114.3 / 114.5 | **151.8 / 153.9 / 153.0** | **1.31** |
| callback transport, master / (a) | — | 147.3 / 149.4 (medians) | untouched |

A batch takes 19.3 ms on master's `adaptive`, 6.5 ms with the spill,
and 8.6 ms on Loom. `adaptive` now beats Loom here. The likely reason is
that 28 of the 64 fibers read on platform threads with no virtual-thread
mount or unmount. That is a hypothesis: it was not measured.

- **(b0) says the thread count is the whole story.** 78 platform
  threads reach Loom too. But that is candidate (b): threads that are
  started for one burst and never retire (`live` only grows). (a) beats
  it without adding any platform thread, so (b) was not built.
- **(a) LANDS.** The law was red on master first ("a blocker never
  started": the fifth door-blocked fiber had no thread for 10 s), and
  the raw-blocking law read 4 fibers in the call at once, which is
  exactly `n + overflow`. Both are green with the spill. TestSchedulerLaws,
  TestOwnMonitor and TestAdaptiveScheduler hold (64 tests).
- **Guards**, lane against its base 9ea842d62 with `-Dokay.scheduler=
  adaptive`, alternating base, lane, lane, base, each lane its own
  `scripts/jmh-lane.sh`:

| lane | base | lane | lane / base |
|---|---:|---:|---:|
| forkJoin10k_okay OUTSIDE, work=100 (us, pooled median of 6 attempts each) | 2 422 | 2 160 | 0.89 |
| forkJoin10k_okayInside, work=100 (us, pooled median) | 3 196 | 3 215 | 1.01 |
| Wrocław, okay 8 fibres, ms wall (best of 5) | 115 / 112 | 115 / 115 | 1.01 |
| Wrocław on Loom, the lane build | — | 111 | — |

  The outside lane read +-10-90% within a run on both builds, and
  jmh-lane gave up on it every round. Its pooled medians are inside the
  noise. The short-task lanes never reach the bound, so the spill never
  runs there. The rows say only that nothing else moved.
- **Where it resumes.** The rule came with the code, so there was
  nothing to measure. A spilled fiber stays on its virtual thread until
  its drive parks on an `Await`. After that it continues on whichever
  thread delivers the answer, as on every member. "Return to an owned
  worker after the block" cannot be built for a raw call, because
  nothing announces when the call ends. For a door it would put a
  submission and a wake on every resumed fiber, and it has no lane
  that needs it.
- **Not covered, said so:** a fiber that has already STARTED on a worker
  and then blocks still holds that worker. The spill moves only waiting
  work. So `n + overflow` is now the number of fibers that can block on
  PLATFORM threads, and it no longer caps how many fibers can block.
  Where Loom is absent (JDK 17-20) nothing can spill, and the old bound
  stands.


## The default, re-run (2026-09-28, scheduler-default-rerun)

Both conditions in "What would reopen it" are met: Wrocław on `adaptive`
reads Loom's time (adaptive-outside-long-fibers-serial) and five-way TCP
blocking reads 1.31x Loom (adaptive-blocking-io). So the table of "The
default" is measured again on today's code, and the default is decided
again from it.

### How it is measured
The same matched pairs as "The default": ONE switch, `-Dokay.scheduler=
loom|adaptive` through JMH's `-jvmArgsAppend` (and through `run /
javaOptions` for Wrocław, `runMain` forked), so both arms are the same
code. Five-way pairs its `okay` runtime (the default given) with
`okayAdaptive`, in the harness clone at 82ac6f1 patched by
`compare/five-way/apply.py`, okay published at a private version
(`0.2.0-sdr`), run by `compare/five-way/run.sh`. JMH lanes `-f 2 -wi 3
-i 5`, AdversarialBenchmark pinned to `-p work=100 -p shape=4x4`, every
lane its own `scripts/jmh-lane.sh`; five-way at his settings (`-f 3 -wi
3 -i 3`). Wrocław: `OkayBench 8 5 1 8`, best of 5, ms wall.

- PART 1, one round, arms alternating loom then adaptive per lane:
  fork/join 10k outside and inside (work=100), cancel 1 000,
  `DirectParallelBenchmark.parallel8`, Wrocław 8 fibres, five-way TCP
  blocking. Table and a preliminary verdict committed before part 2.
- PART 2: a second round of every part-1 lane (adaptive first), §4 100
  fibers outside and inside (`okaySpawn`, `okaySpawnInside`), and the
  rest of five-way (spawn/join, workers work=0/64, TCP callback, runtime
  entry). A third pair only where two rounds disagree in sign.
- Correctness with `adaptive` as the given: TestSchedulerLaws,
  TestManagedBlocking, TestOwnMonitor, TestReadyMerge,
  TestAdaptiveScheduler (its property-unset assertion is expected red:
  it asserts the arm itself away).

Expected before measuring, from the three lanes' rows: `adaptive` at
0.5-0.9 of Loom's time on fork/join, cancel, `parallel8` and §4; 20x+
on five-way spawn/join; Wrocław within noise (~110 ms both); TCP
blocking ~1.3x Loom. The disqualifying evidence: any row where
`adaptive` is worse than Loom beyond the lane's noise.

### Behavior
- [x] part 1 table + preliminary verdict
- [x] part 2 table
- [x] correctness column under `adaptive` as the given
- [x] the decision: flip `Schedulers.auto` to `adaptive` where virtual
      threads exist ONLY if no row loses beyond noise; JDK 17-20 keep
      `platform` (no spill there, so no `adaptive` by default either)

### Results, part 1 (2026-09-28) — one round, loom then adaptive
Worktree on master 9ec45ca40, the same build for both arms. JMH lanes
us/op (lower better); five-way ops/s (higher better); Wrocław ms wall,
best of 5. Every lane its own `scripts/jmh-lane.sh` (five-way: run.sh's
own quiet-gated lock), and every one finished on a quiet box.

| lane | loom | adaptive | adaptive / loom (time) |
|---|---:|---:|---:|
| §4b fork/join 10k OUTSIDE, work=100 | 3 257 | 2 135 * | 0.66 |
| §4b fork/join 10k INSIDE, work=100 | 3 515 | 3 294 ** | 0.94 |
| cancel 1 000 parked | 1 023 | 697 | 0.68 |
| `DirectParallelBenchmark.parallel8` | 10.08 | 6.86 | 0.68 |
| **Wrocław, okay 8 fibres, ms wall** | **114** | **113** | **0.99** |
| **five-way TCP blocking, 64 x 1 ms, ops/s** | **117.3** | **155.2** | **0.76 (1.32x ops/s)** |
| five-way TCP callback (same run), ops/s | 117.6 | 150.4 | 0.78 (1.28x ops/s) |

\* the adaptive outside arm never came out within jmh-lane's 10% error
bar: five attempts read 2 485 / 2 135 / 1 894 / 1 975 / 2 416 (errors
14-31%), each on a box quiet at both ends. The median is shown, as in
adaptive-outside-long-fibers-serial and adaptive-blocking-io; even the
worst attempt is 0.76 of Loom's time. \*\* first attempt 3 050 was
discarded at 14%; the kept one is shown (pooled median 3 172, 0.90).

**Preliminary verdict: flip.** Neither of the old table's losses is
there. Wrocław reads 113 against Loom's 114 (it was 3.3-5.5x slower),
and TCP blocking reads 1.32x Loom's ops/s (it was 0.45x). Every other
row is `adaptive` at 0.66-0.94 of Loom's time, the same side as the
first decision. Part 2 (a second round of these, §4, the rest of
five-way) decides.

### Results, part 2 (2026-09-28) — the second round, §4, the rest of five-way
The same build (9ec45ca40). Round 2 ran adaptive first. Rows:
`src/jmh/history.d/2026-09-28T080018Z-scheduler-default-rerun.tsv`.

| lane | loom r1 / r2 (/ r3) | adaptive r1 / r2 (/ r3) | adaptive / loom (time) |
|---|---:|---:|---:|
| §4b 10k OUTSIDE, work=100 (us) | 3 257 / 3 175 | 2 135 * / 2 019 | 0.65 |
| §4b 10k INSIDE, work=100 (us) | 3 515 / 3 358 / 3 355 | 3 294 / 3 546 / 2 797 | 0.98 (medians 3 294 / 3 358) |
| cancel 1 000 parked (us) | 1 023 / 1 033 | 697 / 706 | 0.68 |
| `parallel8` (us) | 10.08 / 10.27 | 6.86 / 6.89 | 0.67 |
| §4, 100 fibers OUTSIDE (us, one round) | 19.39 ± 0.15 | 18.83 ± 1.08 | 0.97 |
| §4, 100 fibers INSIDE (us, one round) | 31.77 | 16.24 | 0.51 |
| **Wrocław, okay 8 fibres (ms wall, best of 5)** | **114 / 110** | **113 / 112** | **1.00** |

Five-way, ops/s (higher better), his settings:

| lane | loom (`okay`) | `okayAdaptive` | adaptive / loom (ops/s) |
|---|---:|---:|---:|
| **TCP blocking, 64 lanes x 1 ms** | **117.3 / 116.4** | **155.2 / 155.0** | **1.33** |
| TCP callback, 64 lanes x 1 ms | 117.6 / 116.6 | 150.4 / 151.0 | 1.29 |
| sequential spawn/join, 1 000 (one round) | 448.8 ± 52 | 15 680 ± 141 | 35x |
| 8 workers x 4 096, work 0 (one round) | 3 959 ± 100 | 4 188 ± 164 | 1.06 |
| 8 workers x 4 096, work 64 (one round) | 3 406 ± 57 | 3 376 ± 154 | 0.99 |
| runtime entry (one round) | 155 365 ± 7 601 | 143 790 ± 16 861 | 0.93 |

\* part 1's median of five noisy attempts (see part 1).

- The inside fork/join lane disagreed in sign between rounds (0.94, then
  1.06), so it got a third pair, loom first: 3 355 against 2 797. The
  medians read 0.98, within noise.
- §4 outside moved from 0.62 (625316caa) to 0.97. Loom got faster there
  (20.4 to 19.4), and `adaptive` got slower (12.6 to 18.8, ± 1.1). The
  lane is one round, and both arms fall within the other's error.
  It is the one row that is not a clear win, and it is not a loss.
- Runtime entry reads 0.93 of Loom's ops/s. `adaptive`'s ± 12% covers
  Loom's number, and 625316caa read 0.95. Within noise, not a loss.
- **Neither of the old table's losses is left.** Wrocław reads 1.00
  (was 4.4x slower) and TCP blocking 1.33x Loom (was 0.45x).

### Correctness with `adaptive` as the given
`-Dokay.scheduler=adaptive` in the test fork of okay-platform and
okay-stream:

| suite | result |
|---|---|
| TestSchedulerLaws + TestManagedBlocking + TestOwnMonitor + TestAdaptiveScheduler | 63 / 64. The one red is TestAdaptiveScheduler's "the default given Scheduler is auto's pick, unset", which asserts that the property is unset. That is the arm itself, not a defect |
| **TestReadyMerge** | **HANGS: no result after 32 min, and again after 4 min.** Loom on the same tree: 29 / 29 green |

The hang is a LIVELOCK, not a slow test. In two thread dumps 6 s apart,
the given's worker `okay-own-1-0` had used 243 s of CPU in 247 s. It was
in `SentinelChannel.attemptSend:351`, sending into the early-stopped
`merge` of TestReadyMerge.scala:239, which a receiver had resumed
inline. The send loop enqueues, sees room, claims its own waiter back,
removes it and retries, for as long as another waiter sits at the head
of `sendersAt(route)`. That is a busy-wait that relies on another
carrier. The other 13 workers were parked. Filed as backlog
`adaptive-merge-early-stop-livelock` (okay-core), with both stacks. In
scheduler-default-decision (625316caa) this suite was 15 / 15 green
under `adaptive`, so the shape has appeared since.

### Decision (2026-09-28): the default stays `loom`, and correctness is the reason
- **Performance allows the flip.** Every row is a win or within noise.
  Wrocław 1.00, TCP blocking 1.33x Loom's ops/s, fork/join outside
  0.65, cancel 0.68, `parallel8` 0.67, §4 inside 0.51, spawn/join 35x.
  §4 outside (0.97), fork/join inside (0.98), workers work=64 (0.99)
  and runtime entry (0.93 ops/s) are all inside their error bars.
- **Correctness does not.** A default must not turn a program that
  finishes on Loom into one that spins forever. TestReadyMerge does
  exactly that under `adaptive`, deterministically, on a library
  combinator (`merge` stopped early by `take`). The flip is NOT made.
- **What would reopen it:** `adaptive-merge-early-stop-livelock` fixed,
  with TestReadyMerge green under `-Dokay.scheduler=adaptive`. The
  performance table above then stands, unless the fix touches the
  channel's send path. In that case re-run the rows it could move:
  TCP, Wrocław, and the fork/join lanes.
- **FIXED the same day (adaptive-merge-early-stop-livelock).** The
  spin was `attemptSend`'s else branch — a sender queued behind
  another's waiter rechecking room and retrying until the head left —
  which under a callback drive waits for a fiber only the thread it
  spins on could resume (the receiver that woke it inline). It retries
  only when its own waiter has become the head now; with a waiter
  ahead, the freed slot's wake is in flight to that one and the sender
  parks — and a sender that claimed its own waiter back on seeing room
  OWNS that wake and pushes at once (the head rule alone deadlocked
  TestChannelLaws' two-producer law: the self-claimed sender re-asked
  the queue, met a later waiter, and parked behind it with the slot
  empty). Red first (TestReadyMerge, "Merge.Shared on a callback
  scheduler …": `adaptive round 13` hung at the 10 s bound), then
  30/30 under Loom and **30/30 under `-Dokay.scheduler=adaptive`**.
  The fix IS on the channel's send path (one `peek` per queued-behind
  send, no new allocation), so by the rule above TCP, Wrocław and the
  fork/join rows are re-run before the flip; the flip itself is not
  made by that lane. `AbruptChannel` got the same two halves the same
  day (abrupt-sender-head-recheck); TestSendBehindWaiter holds the law
  for both channels on `own` and `adaptive`.
- The flip, when made, keeps JDK 17-20 as it is (`platform`). There is
  no spill there, so `adaptive`'s old `n + overflow` bound would come
  back.

## The flip (2026-09-28, scheduler-default-flip)

`Schedulers.auto` hands out ONE shared `adaptive` where the JVM has Loom
(`platform` on 17-20, unchanged); `given Scheduler` is `auto` unless
`-Dokay.scheduler` says otherwise. `loom` is a `given` away. The
performance table of "The default, re-run" stands; what this section
records is the correctness gate, which found three defects the Loom
default had been hiding and one test that read a drive's semantics as
a failure.

### The gate: the whole JVM family under the new default
`scripts/gate.sh "family jvm"` on a 4-core Linux box, twice. The first
run (5 475 results) named what follows; the second (4 710, after the
fixes; okay-intent now compiled under a UTF-8 locale) had no red that
reproduces on its own suite under `adaptive` and not under Loom:

| red | cause | outcome |
|---|---|---|
| TestAsync "a bracket cancelled by timeout releases its resource" | a drive stopped only BETWEEN operations; Loom interrupts | **fixed**: drive-interrupts-blocking-run |
| TestPoolElastic (okay-pool), 0/3 on own, drive, adaptive; 3/3 Loom | a late continuation's throw escaped the drive (`apply(k(x))`) | **fixed**: drive-resume-throw-lost |
| TestSupervisionShapes "a child forked AFTER the scope failed" | a drive cancels a not-yet-started child by never starting it: no canceler to count | the law accepts "never started" as well as "cancelled" |
| TestAdaptiveFifo "what it keeps" hung the okay-stream fork | producers had no deadline; the consumer's ran out under full-build load | producers share the consumer's deadline |
| TestStackBytes, TestCmd (stm-ui-close) | load: green alone under both schedulers | none |
| TestDelta, TestModels | red under Loom too on this box (Hadoop on JDK 25; a stored artifact) | not this flip's |

### Semantics a user sees change
- A cancel reaches a fiber's blocking `Run` (an interrupt), as it did
  on Loom — now on every drive, not only the default.
- A cancelled fiber that had not started never runs. On Loom it started
  and met its interrupt at the first park.
- A fiber's blocking call holds a platform worker until `adaptive`'s
  watch adds one (`workers + overflow`, then virtual threads); on Loom
  it held nothing. `given Scheduler = Schedulers.loom` is the answer
  for a program whose fibers spend their lives blocked.

### Re-measured on this code (2026-09-30, scheduler-flip-remeasure)
The re-run table below ("The default, re-run") was taken before the
slice hooks (drive-interrupts-blocking-run) and the late-resume `Bind`
(drive-resume-throw-lost). The same lanes on master of 2026-09-30 —
the hooks as made cheap again by spawnjoin-rise-bisect, `forkLong`
(adaptive-chunked-merge-cost), per-producer routes and the foreign-resume
handoff (channel-route-per-producer) — one lane per `jmh-lane.sh`, two
rounds each with the arms alternating by `-Dokay.scheduler`, `-f 2`:

| lane | default | Loom | ratio | 2026-09-28 |
|---|---:|---:|---:|---:|
| AdversarialBenchmark.forkJoin10k_okay (outside, work 100) | 2102 (1988.5 / 2215.4) | 3111 (3074.1 / 3148.7) | 0.68 | 0.65 |
| forkJoin10k_okayInside (work 100) | 3140 (3220.6 / 3060.0) | 3249 (3266.3 / 3231.5) | 0.97 | 0.98 |
| cancel1k_okay (parameters do not enter it; 4 rows each) | 716 | 1017 | 0.70 | 0.68 |
| AsyncBenchmark.okaySpawn (100 outside) | 10.61 (10.62 / 10.61) | 19.31 (19.38 / 19.24) | **0.55** | 0.97 |
| AsyncBenchmark.okaySpawnInside (100 inside) | 16.92 (16.87 / 16.96) | 31.32 (31.87 / 30.78) | 0.54 | 0.51 |
| DirectParallelBenchmark.parallel8 | 7.01 (6.91 / 7.12) | 10.18 (10.15 / 10.20) | 0.69 | 0.67 |

Every row holds; the one that moved moved FOR the default — spawn from
outside, 0.97 to 0.55, the per-slice price paid back. The lanes this
table never had are measured in their own lanes: the chunked merge
(adaptive-chunked-merge-cost, 185 against Loom's 197-229), sequential
spawn/join on `own` (spawnjoin-rise-bisect, 64.4 against 64.2 before the
hooks), the elementwise merge and zip (channel-route-per-producer, every
capacity under Loom). Rows: src/jmh/history.d/2026-09-30T131624Z-scheduler-flip-remeasure.tsv.
