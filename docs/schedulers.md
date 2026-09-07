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

Most code never chooses one. The default is right almost always:

```scala
given Scheduler = Schedulers.loom      // the JVM default, already given
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
| `Schedulers.loom` | a virtual thread | free — the thread parks | the default; anything that may block: I/O, `join()`, channels |
| `Schedulers.own` | a task on a thread the scheduler owns | holds one of the workers | short CPU-bound fibers, fork/join as throughput |
| `Schedulers.adaptive` | the same, watched | costs latency, not the program | `own`'s speed when you are not certain nothing blocks |
| `Schedulers.drive(pool)` | a task on a JDK pool | holds a pool thread | when the pool is given to you — a container's, a framework's |
| `Schedulers.forkJoin(pool)` | a pool task, no Loom | holds a pool thread | a JVM without virtual threads |
| `Schedulers.threads` | one platform thread | free | Native's default; a JVM that must not use Loom |

On JS there is exactly one and it is given: a fiber is a continuation
walked by the event loop, and a blocking `join()` is a compile error
rather than a frozen page.

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

## The one decision this scheduler makes

Everything above is in service of a single choice, made continuously:
**keep the burst here, or wake a core to share it?**

Both answers are right, on different work, and the difference is
enormous. Ten thousand fibers, forked and joined, measured
(docs/benchmarks.md §4b):

| µs per 10 000 fork/joins | 30 ns of work each | 2.5 µs of work each |
|---|---|---|
| kyo (never spreads a burst forked inside a worker) | **779** | 25 419 |
| okay `Schedulers.own` | **744** | **3 327** |
| a raw JDK `ForkJoinPool` (wakes a worker for nearly every task) | 1 871 | ~2 900 |

At 30 ns a fiber, waking a core costs more than the work: the runtime
that keeps the burst at home wins by 2.4x. At 2.5 µs a fiber, a burst
kept at home is a burst on one core: the runtime that spreads wins by
7.6x. A scheduler that only knows one of these is wrong half the time,
and a knob that asks the PROGRAMMER which case they are in is a knob
that will be set wrong.

So `own` measures instead. At every sixteenth task a worker already
knows two numbers — how long it has been busy and how many tasks that
took — and it wakes one sleeper only when it is past `helpAfter` AND
its tasks are averaging more than `spreadAbove`. Nothing is declared;
short fibers stay home, long ones spread, and a program whose fibers
change size gets both without touching a setting.

`forShortTasks` and `forLongTasks` exist for when you already know,
and they are the same rule with the threshold pinned.

## What `own` costs you, and what `adaptive` buys back

A worker is a real thread, and a fiber that BLOCKS inside one holds
it. Block on every worker at once — a `join()` inside a fiber, a
`receiveBlocking` on a channel nobody is filling — and the program
stops. No policy over queues can see this: the queues are not empty,
they are unattended.

`Schedulers.adaptive` is `own` with the stuck-check on. Every
`watched(after)` it looks at one thing: is work pending while nothing
at all has completed since the last look? If so it starts another
worker. Blocking then costs latency instead of the program.

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

**A JVM without virtual threads.** `Schedulers.forkJoin()` behaves,
and `Schedulers.threads` always works.

**Bounding the damage of a runaway stage.** `workers(2)` is a
concurrency limit that needs no semaphore: two threads, and the deque
holds the rest.

## What the numbers say

The full tables, with the runs behind them, are in
`docs/benchmarks.md` §4 and §4b. The short version:

- fork/join of 100 fibers is Loom's floor, and okay sits on it: about
  5 ns of bookkeeping per fork/join over a raw virtual thread.
- fork/join of 10 000 tiny fibers is a measure of SCHEDULING, and
  `own` leads it (744 µs against kyo's 779, a raw pool's 1 871).
- the same 10 000 fibers with real work in them is a measure of
  SPREADING, and `own` leads that too (3 327 µs against kyo's 25 419).
- the JDK pool underneath `drive` is 2.4x kyo's scheduler per small
  task and spreads what kyo will not; `own` is the one that does both.
