# Schedulers

## What this is, and who needs it

A **scheduler** answers one question: when a program says "run this
too", what runs it? That is the whole of `Scheduler`:

```scala
trait Scheduler:
  def fork[A](prog: () => A ! Async): Fiber[A]
```

It takes the PROGRAM, not a computed answer, which is what lets an
event loop be a scheduler as easily as a thread pool does.

Most code never chooses one. The default is right almost always, and it
is already given: on a JVM with virtual threads it is ONE shared
`Schedulers.adaptive` (`Schedulers.auto`), and on JDK 17-20 a watched
`own`.

```scala
val f = Async.spawn(async(work()))
f.join()                               // blocking here is free
```

Read on if you have one of the three reasons to look further: your
fibers are tiny and there are a great many of them; you must not use
virtual threads; or you want the machine to spread a burst over its
cores without you saying when.

## The family

| member | a fiber is | blocking inside a fiber | for |
|---|---|---|---|
| `Schedulers.loom` | a virtual thread | free — the thread parks | fibers that spend their lives blocked: I/O-bound servers, long `join()` chains; the default until 2026-09-28 |
| `Schedulers.own` | a task on a thread the scheduler owns | holds one of the workers | short CPU-bound fibers, fork/join as throughput |
| `Schedulers.adaptive` | the same, watched | costs latency, not the program — `workers + overflow` block on platform threads, and waiting work past that runs on virtual threads | the default where the JVM has Loom: `own`'s speed, and blocking that is survivable |
| `Schedulers.drive(pool)` | a task on a JDK pool | holds a pool thread | when the pool is given to you — a container's, a framework's |
| `Schedulers.forkJoin(pool)` | a pool task, no Loom | holds a pool thread | a JVM without virtual threads |
| `Schedulers.threads` | one platform thread | free | Native's default; a JVM that must not use Loom |

On JS there is exactly one and it is given: a fiber is a continuation
walked by the event loop, and a blocking `join()` is a compile error
rather than a frozen page.

## Why the default is `adaptive`, measured

`adaptive` is the faster scheduler for fork/join, so making it the
default was measured rather than assumed, twice (2026-09-27 and 09-28;
the tables are in specs/schedulers.md, "The default" and "The default,
re-run"). Same code, only `-Dokay.scheduler` changed, alternating rounds.
The first round, which kept Loom:

- **`adaptive` wins** every fork/join and cancel lane: 100 fibers
  forked and joined in 0.49-0.62 of Loom's time, 10 000 in 0.58-0.93,
  1 000 parked fibers cancelled in 0.67, an eight-way direct `parallel`
  block in 0.66, and sequential spawn/join in someone else's harness
  26 times over.
- **`adaptive` lost** the two shapes a default cannot afford to lose,
  and both losses are now FIXED (2026-09-28). Eight long CPU fibers
  forked from `main` (the Wrocław headline) took 355-605 ms where Loom
  took 106-111. The monitor now also watches forks from outside, and they
  take 110-115 ms. 64 fibers in blocking 1 ms calls ran at 0.44 of Loom's
  throughput, because 64 blocked fibers shared 28 threads. Waiting work
  past that point now runs on virtual threads (below), and the lane
  reads 1.31x Loom.
- **It still has a number Loom does not**, but the number now means
  something smaller. `workers + overflow` (28 on a 14-core machine by
  default) is how many fibers can block on PLATFORM threads. Work that
  is still waiting when all of them are taken goes to a virtual thread
  instead of waiting. On a JVM without virtual threads, nothing can
  spill: one fiber more than the bound, if the fiber that would release
  them is queued behind them, waits until something outside the
  scheduler lets one go.

**Re-decided on 2026-09-28: the default is `adaptive` now.** The table
was run again on the same lanes. Every row is a win for `adaptive` or
within noise: Wrocław 1.00, blocking TCP 1.33x Loom's throughput,
fork/join from outside 0.65 of Loom's time, cancel 0.68, sequential
spawn/join 35 times over. One thing held the flip for a day: with
`adaptive` as the given, a `Source.merge` stopped early by `take` left a
channel sender spinning on one worker. The sender retried behind another
parked sender whose wake only the spinning thread could deliver. It
parks now, on both bounded channels, and the whole JVM build was run
under the new default before the default changed (specs/schedulers.md,
"The flip").

Loom is a line away, and it is still the right choice for a program
whose fibers spend their lives blocked:
`given Scheduler = Schedulers.loom`, or `-Dokay.scheduler=loom` for a
whole run.

## Choosing and tuning, the way a queue is chosen

`own` is a builder, like `Queues.strong`. Every knob has a measured
default, and `build` ends it:

```scala
Schedulers.own.build                        // the defaults, which adapt on their own
Schedulers.own.workers(4).build             // four threads instead of one per core
Schedulers.own.forShortTasks.build          // never spread — keep every fiber where it was forked
Schedulers.own.forLongTasks.build           // spread at once — a core per fiber if the machine has one
Schedulers.adaptive.build                   // own, plus a worker when a fiber blocks
Schedulers.adaptive.workers(8).watched(200.millis).build
```

| knob | what it decides | default |
|---|---|---|
| `workers(n)` | how many threads the scheduler owns | one per core |
| `spinning(rounds)` | how long a worker with nothing to do looks before parking | 64 |
| `wakeAbove(tasks)` | how deep the submission queue may get before a sleeper is woken | 64 |
| `helpAfter(d)` | how long a worker may be busy, with work still pending, before it asks for help | 50 µs |
| `spreadAbove(d)` | the task cost above which spreading pays at all | 1 µs |
| `watched(after, overflow)` | start one more worker when nothing has completed for `after` | off in `own`, 100 ms in `adaptive` |
| `monitorEvery(d)` / `unmonitored` | how often the monitor looks for a worker stuck in one task with work waiting behind it; see below | 100 µs (off in `forShortTasks`) |

## The one decision this scheduler makes

Everything above is in service of a single choice, made continuously:
**keep the burst here, or wake a core to share it?**

Both answers are right, on different work, and the difference is
enormous. Ten thousand fibers, forked and joined, measured
(docs/benchmarks.md §4b):

| µs per 10 000 fork/joins | 30 ns of work each | 2.5 µs of work each |
|---|---|---|
| kyo (never spreads a burst forked inside a worker) | 880 | 27 097 |
| **okay `Schedulers.own` — deciding for itself** | **750** | **3 645** |
| okay `own.forShortTasks` — the decision pinned to "never" | 674 | 26 430 |
| okay `own.forLongTasks` — pinned to "always" | 2 038 | 3 678 |
| okay `drive`, on a JDK pool | 785 | 3 021 |

The two pinned rows are the two runtimes this table compares, in one
scheduler. The default row is the point: within 11 % of the better
preset in each column without being told which case it is in.

At 30 ns a fiber, waking a core costs more than the work: keeping the
burst at home wins by 3x. At 2.5 µs a fiber, a burst kept at home is a
burst on one core: spreading wins by 7.2x. A scheduler that only knows one of these is wrong half the time,
and a knob that asks the PROGRAMMER which case they are in is a knob
that will be set wrong.

So `own` measures instead. At every sixteenth task a worker asks two
questions over two different spans: have I been busy longer than
`helpAfter` since this run of work began, and are my LAST sixteen
tasks averaging more than `spreadAbove`? Both true, it wakes one
sleeper. (The second span is not a detail: the fiber that forks a
burst is itself a long task, and averaging from the start made every
burst look expensive.) Nothing is declared;
short fibers stay home, long ones spread, and a program whose fibers
change size gets both without touching a setting.

`forShortTasks` and `forLongTasks` exist for when you already know,
and they are the same rule with the threshold pinned.

**The monitor, for work nobody was told about** (2026-09-26). A fiber
forked inside a worker goes on that worker's own queue silently, and
the rule above is asked only every sixteenth completed task — so a few
LONG fibers, or fibers that block, used to sit behind a worker busy
with one of them while the others slept: eight long fibers ran on one
thread. A small monitor thread per scheduler now looks every 100 µs
(`monitorEvery`) for a worker whose queued work has waited a whole
look without being touched, and wakes sleeping workers to take it —
on `adaptive` it also starts overflow workers when nobody is asleep. It
costs nothing measurable on tiny fibers (sequential spawn/join within
2%), goes to sleep after ~10 ms with nothing to do, and turns a burst
of 87 µs fibers from 700 µs into 260 µs. It spreads work that has
WAITED, whatever it is: a burst of tiny fibers over shared state is
faster kept on one core (five-way workers at work 0: 5 400 ops/s home,
4 000 spread), which is what `forShortTasks` — monitor off — is for.

The monitor also watches the queue that forks from OUTSIDE the
scheduler land in (2026-09-27). Such a fork wakes a worker only when
none is awake, so eight long fibers forked from `main` used to wake one
worker and wait behind it: eight 70 ms fibers took 560 ms on one
thread. Now a task still at the head of that queue a whole look later
wakes sleeping workers too, and the same eight run at once, as on Loom.
Short fibers from outside leave the head moving and stay with the
workers already awake.

**A fork that says it is long** (2026-09-29). The monitor needs a
whole look to see a waiting task, and it can only see it twice in a
row: 100-200 µs. For a 70 ms fiber that is nothing. A chunked
`merge` of two 2 000-element streams is over in ~200 µs, and its second
feed waited that long every time: 351 µs on `adaptive` against Loom's
201. The merge knows what the scheduler cannot, that its feeds run for
the stream's whole life, so it forks them with
`Scheduler.forkLong(prog)`. On `own`/`adaptive` that is `fork` plus
one sleeping worker woken at once (one no other `forkLong` is already
waking); on every other scheduler it is `fork`. The chunked merge now
reads 185 µs against Loom's 197-229, and `fork` itself is unchanged
(sequential spawn/join 112.4 against 112.8, same session — a number that
was itself 1.7x too high that day, from the per-slice cancel hooks, and
is 64 µs again since spawnjoin-rise-bisect). Use it for
your own long-lived producers; a short fiber forked with it only wakes
a worker for nothing. The elementwise `buffer` keeps `fork`: with a
64-slot ring the spread feeds block each other, and it measured 1.25x
Loom with `forkLong` against 1.13x without
([specs/adaptive-chunked-merge-cost.md](../specs/adaptive-chunked-merge-cost.md)).

**Who runs a fiber that was woken** (2026-09-29). A fiber parked in an
`Await` resumes on whoever answers it: inside the pool that saves a
wake. But the thread answering is often not ours — the caller's own
consumer taking from a channel frees a slot and answers the producer
waiting on it — and it then ran the PRODUCER's code instead of its own:
31% of a consumer's time in the 64-slot merge. On `own`/`adaptive` a
late answer from a thread that is not one of the scheduler's workers
now sends the fiber home with a plain `fork`, and an answer from a
worker still runs it in place. The first cut sent it home with
`forkLong` and woke a sleeper every time: right for a 64-slot merge,
2.7x too slow for a 7-slot `zip`, where a resume comes every few
elements. With `fork`, on the same build: merge at capacity 7 / 64 92 /
61 µs (Loom 312 / 82), `zip` 1383 / 370 (Loom 2396 / 793). The one shape
that pays is a `zip` with a tiny ring: 1383 against 483 when the
consumer ran the producers itself; at the default capacity (64) the
handoff wins, 370 against 472
([specs/adaptive-elementwise-small-ring.md](../specs/adaptive-elementwise-small-ring.md)).

## What `own` costs you, and what `adaptive` buys back

A worker is a real thread, and a fiber that BLOCKS inside one holds
it. Block on every worker at once — a `join()` inside a fiber, a
`receiveBlocking` on a channel nobody is filling — and the program
stops. No policy over queues can see this: the queues are not empty,
they are unattended.

`Schedulers.adaptive` is `own` with the stuck-check on. Every
`watched(after)` it looks at one thing: is work pending while nothing
at all has completed since the last look? If so it starts another
worker. Blocking then costs latency instead of the program. Since the
monitor (above), fibers that block inside a worker are also spread
within a tick onto sleeping workers and then overflow workers — up to
`overflow`, which defaults to one per core. That bound is the ceiling
for PLATFORM threads. When every worker it may own exists and work is
still waiting behind blocked ones, the waiting work runs on virtual
threads (2026-09-28). 64 fibers each making 1 ms blocking calls used to
reach 28 at once on 14 cores (19.3 ms a batch). Now all 64 are in their
call at once (6.5 ms, against Loom's 8.6).

A fiber that blocks through the library's own doors — `join()`,
`receiveBlocking`, a blocking send, `Nio` — does not wait for a tick at
all: the door tells the worker before it parks (the protocol of the
JDK's `ForkJoinPool.ManagedBlocker`). The worker stops counting as
awake, and if work is waiting anywhere, one sleeping worker is woken to
take it; on `adaptive`, when nobody is asleep, an overflow worker is
started, within `overflow`. Measured on `adaptive`: a fiber forked from
outside while one worker is blocked and the rest asleep used to wait
for the stuck-check (210 ms a round trip); it now takes 1.4 ms, a
millisecond of which is the benchmark's own sleep. Plain `own` only ever wakes — its thread
count is its contract — so a sibling left behind a blocked fiber runs on
a sleeping worker, but blocking on every worker at once still stops it.
A blocking call that does not go through a door (JDBC, a raw
`Thread.sleep`) is still found by the monitor and the stuck-check.
A fiber that has already STARTED on a worker stays there: a running
platform-thread stack cannot move. Only work that has not started
spills to a virtual thread. Once spilled, a fiber runs there until it
waits on an answer, and then it continues on whichever thread delivers
the answer, as on every member. Spilling needs Loom and `overflow > 0`,
so plain `own` never spills.

```scala
given Scheduler = Schedulers.adaptive.workers(1).build
// this deadlocks under `own` and completes under `adaptive`
val r = Async.spawn(async {
  val filler = Async.spawn(async { ch.sendBlocking(9); 0 })
  val got = ch.receiveBlocking()
  filler.join(); got
}).join()
```

It is off in `own` because it is a timer and a thread, and neither is
free. When blocking is the norm rather than the exception, neither
member is the answer — `Schedulers.loom` is, where blocking costs
nothing at all.

## A scheduler that owns threads gives them back

`own` and `adaptive` return a `Schedulers.Running` — a `Scheduler`
that is also `AutoCloseable`, with an `id` that appears in its thread
names (`okay-own-3-7`, so a thread dump says which scheduler).

```scala
val crunch = Schedulers.own.workers(6).build
try   items.map(i => Async.spawn(async(heavy(i)))(using crunch)).map(_.join())
finally crunch.close()
```

`close()` lets the workers finish what they are holding and then exit;
it is idempotent, and a fiber forked afterwards is a fiber nobody will
run. `loom`, `drive` and `forkJoin` own nothing of their own and have
nothing to close.

## What every member promises

`TestSchedulerLaws` runs these against each member, and a new member
is not a member until it passes them:

1. every fiber's answer is joined exactly once (10 000 forks, summed);
2. a failing fiber fails its join, as a `Left` carrying the exception;
3. a callback registered before OR after completion fires exactly
   once — the waiters are a stack, so a `join` may return before an
   earlier-registered callback runs, but it runs;
4. after `cancel()`, an answer that arrives late never becomes the
   fiber's answer, and nothing is answered twice;
5. `par` runs both sides on their own thread of control;
6. no lost wake: a fork after every worker has parked still runs;
7. fairness: a fiber forked from outside is answered while a worker is
   grinding through a burst of two thousand;
8. `adaptive`: a fiber that blocks inside a worker does not stop the
   program.

## Recipes

**A pool of workers for a CPU-bound stage, and Loom for everything
else.** A scheduler is a value; pass the one that fits where it fits.

```scala
val crunch = Schedulers.own.workers(6).forLongTasks.build
val results = items.map(i => Async.spawn(async(heavy(i)))(using crunch)).map(_.join())
```

**A container's pool, not ours.** Some runtimes hand you an executor
and expect everything to run on it.

```scala
given Scheduler = Schedulers.drive(containerPool)
```

**A JVM without virtual threads.** Nothing to choose: the default
`given` is `Schedulers.auto`, which is `loom` where there are virtual
threads and `Schedulers.platform` where there are none — `own` with
its stuck-check on, so a fiber that sits in a raw blocking call costs
a tick of latency rather than the program. `Schedulers.forkJoin()`
behaves too, and `Schedulers.threads` always works.

**Bounding the damage of a runaway stage.** `workers(2)` is a
concurrency limit that needs no semaphore: two threads, and the deque
holds the rest.

## What the numbers say

The full tables, with the runs behind them, are in
`docs/benchmarks.md` §4 and §4b. The short version:

- fork/join of 100 fibers is Loom's floor, and okay sits on it: about
  5 ns of bookkeeping per fork/join over a raw virtual thread.
- fork/join of 10 000 tiny fibers is a measure of SCHEDULING, and
  `own` leads it (750 µs against kyo's 880, a raw pool's 1 871).
- the same 10 000 fibers with real work in them is a measure of
  SPREADING, and `own` is 7.4x kyo there (3 645 against 27 097).
- the JDK pool underneath `drive` is 2.4x kyo's scheduler per small
  task and spreads what kyo will not; `own` is the one that does both.
