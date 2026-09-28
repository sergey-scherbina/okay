# ready-merge — merge by readiness on one thread of control

## Overview

The operator's question (2026-09-26): what IS a merge? Several streams,
read in turn — so the ring is of STREAMS, not of data. With asynchrony a
stream either has its next element ready or not yet, and the merge takes
what is ready and does not wait on what is not. Merging streams gives a
stream; merging async streams gives an async stream. Threads are then the
PRODUCERS' business: push where there is room, otherwise wait until
called.

`Source.merge` today answers the concurrent half of that and only it: a
fiber per source feeds one channel (`Channel.merge`, specs/channel-known-
producers.md), so the queue between the sides holds DATA and every merge
costs a fiber per source and a scheduler — none on JS without one. The
single-thread half is absent: there is no merge that keeps the sources as
what they already are, programs, and steps them.

This spec builds it. A `Source[A]` is `Unit ! Writer % A + Async`; one
`resume` of it answers one of four things, which ARE the four answers a
readiness merge needs:

| the source's next node | what it means to the merge |
|---|---|
| `Return(())` | ended — drops out of the ring |
| `Say(a)` then `k` | READY — tell `a` out, `k(())` goes to the back of the ring |
| `Async.Run(f)` then `k` | work to do now — do it, the source keeps its turn |
| `Async.Await(register)` then `k` | NOT READY — park `k` in the source's slot, register a waker |

`Async.Await` is already "Pending plus a callback when ready" (the shape
of Rust's `Poll::Pending` + `Waker`, which `futures::stream::select_all`
is built on), so no new primitive is needed: the ring holds the sources'
own continuations, the queue between threads holds WOKEN SOURCES (an
index per wake-up, not an element per element), and a source whose
elements are ready is drained with no synchronisation at all.

**Where iteratees are.** Kiselyov's iteratees are the CONSUMER as a
suspended program (`Take.Await`/`Stage` here, theory ch.7), and merging
two PRODUCERS is exactly the case the push form (enumerator into
iteratee) finds hard — one side has to be turned inside out into the
pull form. `Source` is a program and `resume` is its pull, so the merge
is written in the pull form and the merged source is consumed by any
iteratee unchanged (`through`, `pipe`, a `Stage`).

**Where parallelism is.** One program is one thread of control, so the
merge runs one source's step at a time: a source that computes for a
millisecond before its next element holds everyone for that millisecond,
and two CPU-bound sources get one core between them. That is the price,
and it is paid only where a source computes. Parallelism is a per-SOURCE
choice, not a property of the merge: `Channel.buffer(n)(s).drained` puts
that one source on its own fiber and turns its pull into an `Await` on a
ring — which the merge treats as any other not-ready source. So
`mergeReady(buffered(a), b)` gives `a` a core and leaves `b` on the
merge's thread; `Source.merge` is the special case that buys a fiber for
every side whether it needs one or not.

## Interface

- `Source.mergeReady[A](sources: Source[A]*): Source[A]` — N sources,
  no `Scheduler`, no `CanBlock`, no `Timer`: it is a program and runs
  wherever `Async` is handled (the JVM drive, `Async.run`, JS's event
  loop).
- `s mergeReady t` — the two-source extension, `Source[A | B]`.

## Behavior

- [x] each source's elements arrive in exactly the order it told them
- [x] nothing lost, nothing invented: the multiset of outputs is the
      union of the inputs
- [x] all sources always ready (no `Await` anywhere) ⇒ strict
      round-robin: s0, s1, …, s(n-1), s0, …; an ended source drops out
      and the turn passes on — a DETERMINISTIC interleave
- [x] a parked source does not hold the others: a source waiting on a
      timer/channel does not delay elements another source has ready
- [x] when every live source is parked, the merge itself parks ONCE
      (one `Await`) and resumes on the first wake-up; woken sources
      rejoin the ring in wake order
- [x] a callback that fires DURING its own registration (synchronous
      answer) is not lost and does not recurse
- [x] `Async.Run` is performed in the source's own turn, in place
- [x] a failing source (a `Left` answer, a throwing `Run`) fails the
      merged program — SINCE source-merge-via-ready after the other
      sources have run to their end (drain, then fail), none cancelled;
      the first cut failed at once and cancelled the parked ones
- [x] cancelling the merged program while it is parked cancels every
      parked source's registration
- [x] a cancel that stops the drive BETWEEN two operations, with the
      merge's park the next one, cancels every parked source's
      registration — on `own` as on Loom (ready-merge-own-cancel-window:
      the park's registration is a `Discontinue` whose `discontinue` is
      `cancelAll`)
- [x] stack-safe: 10^6 elements, and 10^5 synchronous wake-ups
- [x] parallelism by buffering: a `Channel.buffer`ed side keeps its
      order and merges with an unbuffered one
- [x] the merged source is consumed by an iteratee (`Take`/`through`)
- [x] cross-platform: the laws that need no thread run on JS too
- [x] the numbers: `MergeBenchmark` 2x500, see Results

## Design

State, per merge (one instance per run of the merged program):

- `slot: Array[Source[A]]` — each source's current continuation
- `ring: int deque` — indices of READY sources, touched only by the
  drive (one logical thread: the merged program's drive hands over
  between threads with a happens-before, never runs twice at once)
- `woken: ConcurrentLinkedQueue[Integer]` — indices a callback made
  ready (the callback writes `slot(i)` before publishing `i`)
- `waker: AtomicReference[callback]` — set only while the merge is
  parked; whoever takes it (`getAndSet(null)`) fires it, so it fires once
- `cancels: AtomicReferenceArray[() => Unit]` — each PARKED source's
  canceller (at most one registration per source: a parked source is out
  of the ring until it wakes)

Step (a `@tailrec` loop, re-entered through `flatMap` only after a tell
or the merge's own park):

1. ring empty → move `woken` into `ring`; still empty → no parked
   sources: `Return(())`; else PARK: `Async.Await(cb => { waker = cb;
   if woken nonEmpty then fire; cancel = cancel all parked })`, and on
   resume go to 1.
2. `i = ring.pop`, `resume slot(i)`: `Return` → drop `i`; `Say(a)` +
   `k` → `slot(i) = k(())`, push `i` to the BACK, emit `tell(a)` and
   continue; `Run(f)` + `k` → `slot(i) = k(f())`, push `i` to the FRONT
   (its turn continues); `Await(reg)` + `k` → register a callback. An
   answer DURING the registration (the callback wins a CAS on a per-
   registration cell, `Async.Drive`'s handshake) is taken in place and
   the source keeps its turn — no queue touched. A later answer sets
   `slot(i) = pure(x).flatMap(k)` (a `Left` becomes a continuation that
   throws) — BUILT, not run, since `k` is the source's code and only the
   drive runs that — clears `cancels(i)`, enqueues `i`, fires the waker.

The race between a wake-up and the park is Dekker's with volatiles on
both sides: the callback ENQUEUES then reads the waker; the park SETS the
waker then reads the queue — at least one side sees the other.

## Decisions

- **One element per turn** — the fair shape, and the only one whose
  order is a law (round-robin). Rejected for now: a quantum > 1 (fewer
  ring rotations, measured if the per-element cost shows a ring share).
- **`Run` keeps the turn** — a source's `Run` is the work between two of
  its elements; giving the turn away after it would interleave a
  source's own computation with others' elements for no gain.
- **Indices, not pairs** — the ring and the wake queue carry an `Int`,
  the continuation lives in `slot(i)`, so a turn allocates nothing but
  the continuation the source itself built.
- **Early stop and a cancel under the consumer's work reach the parked
  sources on a DRIVE** (ready-merge-cancel-under-consumer-ops,
  2026-09-27). The merge opens an `Async.CancelScope` with its drive when
  it starts and closes it when every source has ended; the drive (`own`,
  `adaptive`, JS) releases every scope still open when it is cancelled —
  between operations, while parked, or from outside — and when the
  program ENDS with one open, which is exactly a consumer that stopped
  early. The release is the merge's `cancelAll`, idempotent. Before it:
  a cancel landing while the consumer worked (the merge never parked, its
  code inside the consumer's continuation) missed 50 of 50 on `own`, and
  an early stop left every parked source registered. The laws:
  TestReadyMerge "a cancel while the CONSUMER is working…" (own and Loom)
  and "an EARLY STOP…" (own), TestReadyMergeCross's early stop on every
  platform's drive — each watched red first. Price: one class test per
  `Run` on the drive, ~1% on a chain of 10 000 bare `Run`s
  (`DriveRunBenchmark`), and 8 B per drive. On Loom (`Async.run`) the
  markers are empty `Run`s: a cancel is an interrupt seen at the next
  wait, whose own canceller releases; an early stop there still leaves
  parked sources registered (below).
- **…and on the BLOCKING schedulers too, and a merge's producers stop**
  (merge-scopes-everywhere, 2026-09-28). A fiber on Loom, `forkJoin` or
  `threads` runs its program through `Async.runFiber`: a handler of its
  own that keeps the scopes the program opens and releases every one still
  open however the program leaves (answer, throw, interrupt). And
  `Source.merge`'s release also CLOSES its sides' channels (in `Merge.Ready`,
  all three joins; in `Merge.Shared` the chunked joins' shared channel
  through `Source.releasing`, and the ELEMENT join through a scope
  ENTERED in front and never exited — `releasing`'s trailing `flatMap`
  would be a Bind over the whole element program, a rotation per
  element, while a scope the drive releases at the program's end costs
  one Bind in front; at a normal end the channel is closed already and
  the release counts nothing), so a merge that is cancelled or stopped
  early ends its feeder fibers — before, each
  filled its buffer and parked for good. Uncovered on every road: an
  ABANDONED `toLazyList` — the program never ends, so no scope is
  released (an iterator close is its own item). Laws: TestReadyMerge "…on Loom
  too" and "Source.merge stopped early releases its sides…" (Loom and
  own), each turned red by its mutant. Cost: the first cut kept the scopes
  in a ThreadLocal and paid +152 B on EVERY Loom fork; the fiber's own
  handler is parity (forkJoin10k_okay 5.97 MB either way per 10 000
  fork/joins). A plain `runWith` on a thread of the caller's own still sees
  no scopes: that is the one road left where an early stop releases
  nothing.
- **Early stop is not cancellation (a bare `runWith`)** — a consumer that stops reading
  (`take`, `runFoldUntil`) drops the merged program without running it,
  so a PARKED source's registration stays registered and its answer is
  dropped when it fires. `Source.merge` has the same property (its
  fibers keep feeding a channel nobody reads). A source whose
  registration consumes data (a channel receive, whose canceller is
  `() => ()`) loses that element either way; recorded, not solved here.
- **A cancel is seen by the sources — with one narrow window, on one
  scheduler** (ready-merge-cancel-race, 2026-09-26). MEASURED, 200
  rounds each, the fiber JOINED before the flags are read: on Loom (the
  default) 0 misses whether the cancel lands before or after the
  merge's first park — the interrupt is sticky, and `block` registers
  the merge's park, sees it and calls the merge's canceller. On `own`
  (a `DriveTask`, where a cancel is the drive's `stopped` flag) 0 misses
  after the park and **2 of 200 before it**: the drive stops at its next
  operation without running it, knows only the Await it last parked on,
  and nothing calls the merge's canceller — the sources already parked
  stay registered, as on an early stop. Backlog
  `ready-merge-own-cancel-window` — CORRECTED by it, next entry: the
  2 of 200 was the join reading a cancel in flight, and the real window
  was a cancel between two operations. The law cancels once the merge is
  parked (`ReadyMerge`'s `onPark` hook, a test's only view of that
  moment) and joins before it reads. THE FIRST CUT of the law read the
  flags right after `cancel()` and so measured the cancel still in
  flight: 297 misses of 300 in a loop, green in the lane's gates by
  luck, red in a ci-runner whole build. Rejected for the window:
  registering the sources' Awaits only when the merge itself parks —
  that delays a parked source's timer or read while the others are
  busy, which is what a readiness merge exists not to do.
- **The window, located and closed where it can be**
  (ready-merge-own-cancel-window, 2026-09-27). A probe, 400 rounds on
  `own`, cancelling right after the fork as the law of 2026-09-26 did:
  missed AT THE JOIN 400/400, and of the rounds whose sources had
  registered, never cancelled once waited on 0/400. So the "2 of 200
  before the park" was the join reading a cancel still in flight: a
  `DriveTask`'s `cancel()` answers the fiber AT ONCE (`done(Left)`),
  while its drive is still running the merge's first step on a worker;
  that drive then reaches the park's `op`, which registers it, reads
  `stopped` and calls the park's canceller — `cancelAll`. Not a leak.
  The REAL window is one step over: a cancel that the drive sees
  BETWEEN two operations — `looping = !stopped` after an op — stops
  it before the next op without running it. When that next op is the
  merge's park (a consumer's operation for one element, then the merge
  registers its parked sources and parks), the park is never
  registered and nothing reaches the sources: 0 of 200 cancelled on
  `own`, deterministic, the fiber cancelling ITSELF inside the
  consumer's operation. The drive's door for exactly that is
  `discontinue` (drive-discontinue): it descends to the LEFTMOST node,
  which there is the park's `Inject(Await(reg))`, and calls
  `discontinue()` on a `reg` that is a `Discontinue`. So the park's
  registration is one object per merge that is also a `Discontinue`,
  its `discontinue` being `cancelAll`. The sprint item's first idea —
  a `Run` carrying the `Discontinue` at the merge's START — does not
  work: at the start nothing is registered yet, and once that `Run` is
  performed it is no longer the leftmost node.
  STILL OPEN (backlog `ready-merge-cancel-under-consumer-ops`): a
  source parked while the others keep telling, and a consumer that
  performs an operation per element. The drive stops before the
  CONSUMER'S next op; the merge's code is inside the consumer's
  continuation, a function the drive must not call, and the merge
  never parks — 50 of 50 leaked on `own` (probe, 20 M-element ready
  side plus a gate, `runForeach` with an `Async.Run` per element).
  Nothing the merge builds is reachable there; it needs the drive, or
  the Writer handlers, to carry a cancel hook. The same shape as
  "early stop is not cancellation" above, reached through a cancel.

## Results

**Laws, 2026-09-26.** `TestReadyMerge` (13, JVM) and
`TestReadyMergeCross` (2, every platform) green; every box above but
the numbers is covered. Two were watched FAIL under a mutant before
being trusted: the park's canceller as `() => ()` (the cancel law read
`a=false b=false`) and a `Run` that gives its turn away (`Vector(1,
10, 2, 20)` came back reordered). The first run found a real defect:
the initial sources were never put in the ring (`size = 0`, `live =
n`), so the merge parked on its first step with nothing to wake it —
caught as a gate STALL, located from a `jstack` of the test fork.

**Cancel between operations, 2026-09-27** (ready-merge-own-cancel-window).
The law (`TestReadyMerge`, "a cancel between the consumer's operation
and the merge's park…") runs 200 rounds on Loom and 200 on `own`, the
fiber cancelling itself inside the consumer's operation for the first
element, and waits on the sources' cancellers (5 s bound), not the
join. Unfixed: Loom 200/200 cancelled, `own` red at round 0
(`a=false b=false`). With the park's registration a `Discontinue`:
both 200/200. The earlier "2/200 before the park" reproduced as a join
artefact only (probe, `own`, 400 rounds: missed at the join 400/400,
registered-and-never-cancelled 0/400). What stays open is in
Decisions and backlog `ready-merge-cancel-under-consumer-ops` (50/50
leaked on `own`).

**Numbers, 2026-09-26 evening** (ready-merge-numbers, the first lanes
through `bench-window`; `src/jmh/history.d/2026-09-26T165404Z-ready-merge.tsv`).
`MergeBenchmark`, 2x500 `LazyList` in, `toLazyList` sum, `-f 2 -wi 3 -i
5`, JDK 26, two rounds alternating, quiet at both ends of every lane.
The control (`okaySourceSingleDrain`, one source, no merge) read 44.31
±0.79 and 44.51 ±0.96: the box held still.

| lane | round 1 | round 2 | against `okaySourceMerge` |
|---|---|---|---|
| `okaySourceMerge` (a fiber per side, one two-part channel) | 114.3 ±8.0 | 106.6 ±5.1 | — |
| `okayReadyMergeBuffered` (a fiber per side, each into its own ring, read by the ready-merge) | 98.6 ±2.8 | 99.0 ±1.7 | **0.86x / 0.93x** |
| `okayReadyMergePure` (no fiber, no channel) | 69.8 ±1.0 | 68.1 ±1.7 | 0.61x / 0.64x (not a matched pair) |

- **The matched pair favours the ready-merge**, 7-14% in both rounds,
  bars separate in both, same sign — and it is also the TIGHTER lane
  (±2-3% against ±5-7%). Same fibers, same buffering, same `Drain`
  batches; what differs is the join: two private rings and a queue of
  wake-ups here, one shared two-part channel with a scanning reader
  there. Filed as `source-merge-via-ready` (backlog okay-core): with
  this, `Source.merge` can be buffer-each-side + `mergeReady` — one
  merge mechanism — provided it keeps `chunked` and `flushAfter`.
- **Pure is the price of merging ready inputs with no concurrency at
  all**: 68-70 us for 1000 elements against 44 us for draining ONE
  source of 1000 — ~25 ns per element for the ring, the tell and the
  second source's walk. It is not a faster concurrent merge and is not
  quoted as one; it is what a caller whose sources are already ready
  (in memory, decoded, generated) no longer has to pay a fiber for.

Before this, over an hour of `jmh-lane.sh` attempts (101 tries) never
got the lane lock and a quiet box at once; the protocol that made the
window is `bench-window`.

## Stage: poll, then park (ready-merge-chunk-forward, 2026-09-27)

WHY. On the chunked ring road (merge-chunked-via-ready, reverted) the
forks were bimodal, and two receive-side fixes were refuted before the
cause was counted: `operate`'s `Await` arm REGISTERS a side the moment
its channel is empty, whatever the other side holds. Counted on that
road (`src/jmh/history.d/…-ready-merge-chunk-forward-probe.tsv`,
`okayChunked`, 10 forks): a slow fork makes ~205 registrations per op,
160 of them while another side was in the ring or `woken`, and 99 of
them are answered asynchronously — a hand-over of ONE chunk on the
producer's thread each; a fast fork registers 85 times, 78 with work,
16 async. The shared channel of today's chunked road makes ~1 per op,
because one queue registers only when BOTH producers are behind. The
merge's own parks are 8 and 1.2 per op: the registrations were never
needed to park on. So: a side that finds nothing while the merge has
other work is not registered — it is POLLED again later, and registered
only when the ring runs dry and the poll still finds nothing.

HOW. A registration function that can be asked without registering is
a `Pollable[X]` (`poll(): Either[Throwable, X] | Null`, null = nothing
now) — the `Discontinue` idiom, a marker on the function the source
already passes. `Channel.drained`/`drainedChunks` pass one over a new
non-registering `receiveManyNow(max)` (`SentinelChannel`: the first
half of `receiveManyAsync`, which now calls it; the `Channel` default
answers null, so every other channel behaves as before). In the merge,
an `Await` whose registration is pollable, met while `size > 0` or
`woken` is non-empty, goes to an `idle` set with its operation and
continuation held typed (`Held[X]`, no cast); idle sides are polled
when a turn passes (the streak ends — once per quantum, which keeps
`mergeReady`'s round-robin at its own granularity) and when the ring
runs dry; an idle side still empty when the ring is dry is registered
then, as today, and the merge parks as today. `PollSpins` (a written
bound) re-polls a dry ring that many times before registering, so a
producer about to send is met by a poll and not by a hand-over.

- [x] a pollable side that finds nothing while another source is ready
      is not registered (`onRegister`, the test hook, never fires), and
      its later data is taken by a poll
- [x] a pollable side is registered only when the ring is dry and its
      poll found nothing (the hook fires with the ready side's three
      elements already consumed); the merge then parks ONCE, as before
- [x] fairness kept: `mergeReady` (quantum 1) over a ready source and a
      pollable side whose data arrives later interleaves the side's
      element within TWO turns of its arrival — the poll runs when a
      turn passes, before that turn's element is consumed, so data the
      consumer itself produces is seen at the next turn's end
- [x] an idle side whose poll answers the end ends like any; one whose
      poll answers a failure drops out — drain, then fail
- [x] cancel: an idle side holds no registration; the existing cancel
      laws (non-pollable `Gate`s) are unchanged, and a pollable side is
      registered — hence cancellable — before the merge parks
- [x] a non-pollable registration (any other channel, a timer) is
      registered as before — the 15 earlier laws unchanged
- [x] THE REGIME CHECK: `okayChunked` k=16, 10 forks, chunked ring road:
      registrations-with-work 0.0 in every fork — but forks above 225 us
      REMAIN (Results), so the bar is not met
- [x] the elementwise road (`Source.merge`, `MergeCapBenchmark` cap
      64/256/1024) takes the same path: 1.01x / 0.96x / 0.98x against
      registering as before, same JVM code, 5 forks per arm alternating
- [ ] stage 2 of specs/source-merge-via-ready.md: not run in this
      landing — the bar above (no fork > 225 us, no arm slower than the
      shared channel) was not met twice; the operator then accepted a
      1.06x mean for one mechanism and asked for the hybrid wait first
      (sprint: ready-merge-chunk-forward, next landing)

**Results (2026-09-28).** Rows in
`src/jmh/history.d/2026-09-27T201816Z-ready-merge-chunk-forward-probe.tsv`.
On the chunked ring road, `okayChunked` k=16, 10 forks per arm:

| arm | slow forks (>225 us) | reg / with-work / wakes / parks per op, slow fork | mean |
|---|---|---|---|
| one-shot registration (before) | 3/10 at 252-257 | 205 / 160 / 99 / 8 | 212.9 |
| poll-then-park, PollSpins 0 | 4/10 at 235-255 | 28-45 / 0.0 / 19-31 / 9-14 | 213.3 |
| PollSpins 100 | 2-4/10 at 215-247 | 2.5-3.5 / 0.0 / 2.5-3.3 / 1.1-2.0 | 205-211 |
| PollSpins 1000 | 1-3/10 at 230-244 | 0.0-1.5 / 0.0 / 0.0-1.3 / 0.0-0.6 | 203.6-212.5 |
| shared channel (control, same session) | 0/10, 195-204 | — | 200.0 ± 2.0 |

The registration storm is gone and the fast ring forks (188-197)
beat every shared-channel fork; the tail is a caught-up consumer
waiting on the producers (2000-3000 polls per op against ~300),
plus plain fork variance (one 228-us fork at 570 polls). REFUTED by
a counter: the merge living on the sending producer's thread after a
park (0.0 of 4000 elements per op on a virtual thread, every fork).
`onSpinWait` or `Thread.yield` between polls narrowed the tail
(4/10 → 2/10) within noise and did not land: `PollSpins = 100` with
a plain poll is the frozen shape, cross-platform. The elementwise
road, `MergeCapBenchmark`, poll-then-park against
`-Dokay.merge.poll=off` in the same JVM: cap 64 84.4 vs 83.6, cap
256 65.9 vs 68.6, cap 1024 60.0 vs 61.1 — kept.

## Stage: the hybrid wait, and the chunked roads onto the ring (ready-merge-chunk-forward, second landing, 2026-09-28)

The operator's decision after the first landing: a 1.06x mean with a
1.15-1.2x tail in 2-4 forks of 10 is ACCEPTED for one merge mechanism,
and the consumer's wait becomes the HYBRID first — spin, then
`Thread.yield` with a poll each, then `parkNanos` with a poll each,
then register and block. Each rung is the consumer's own cost; the
producer's one check (`wakeOne` on an empty waiter queue) stays, and it
pays only when the consumer truly blocks. Measured on this box:
`Thread.yield` 125 ns, `parkNanos(1)` 10-12 us on a platform thread
and 10 us on a virtual one (the timer's floor — the argument stops
mattering below ~10 us), `parkNanos(100 000)` 150-158 us. One brief
park is the window in which two producers make ~50 chunks, so a merge
that slept wakes into batches: the "merge that runs behind on
purpose" of the first landing's reopen condition, for free. `Wait` is a
platform seam: JVM and Native climb the rungs; JS, with no producer
threads, registers at once.

- [x] the rungs, as a CYCLE (the operator's refinement of the straight
      ladder): (`PollSpins` polls, `PollYields` yields with a poll each,
      one brief park) × `PollSleeps`, then register — after a park the
      data has most likely arrived or is about to, and a spin catches it
      at 41 ns where a second park would cost 10 us more. Laws by poll
      COUNT, not time: a side answering on the 121st poll is told on the
      first cycle's yield rung without a registration; one that never
      answers a poll is registered after 3 + 4 × (101 + 50) polls
- [x] JS: a dry ring registers at once (`Pause.threads` false); the
      cross laws unchanged
- [x] THE WAIT IS A GIVEN (operator, 2026-09-28: "вынеси в тайпкласс и
      имплисит чтобы можно было ее менять", then "можно вообще Wait а
      не только Merge"): `trait Wait { def until(ready: () => Boolean):
      Boolean }` — not the merge's, ANY consumer's: asked to wait for a
      condition before it blocks, it polls `ready` on its own schedule
      and answers whether the condition came (false: the caller
      registers and parks). `Wait.Register` never polls (JS's shape,
      and the road before the hybrid); `Wait.Spin(polls)`;
      `Wait.Cycle(spins, yields, cycles)` — the operator's `(spin 100 →
      yield 50 → parkNanos) × 4`. The default `given` in the companion
      is `Cycle(100, 50, 4)` where producers are threads and `Register`
      on JS (the platform seam behind it is private); `ReadyMerge`,
      `Source.mergeReady`, `merge`, `mergeFlushing` and `either` take it
      `using`, so a caller swaps it with one `given` in scope and every
      existing call compiles unchanged. Laws: `Register` given → the JVM
      polls 3 times and registers, as JS does; `Spin(10)` → 3 + 10
      polls; the default → 3 + 4 × (100 + 50).
- [x] THE MERGE MECHANISM IS A GIVEN TOO (operator, 2026-09-28:
      "стратегию самого Merge тоже можно вынести в тайпкласс и имплиситы
      — Ready vs Channel vs whatever else"): `trait Merge` with the three
      shapes `merge` has — `elements(l, r, capacity)`, `chunks(l, r,
      slots, size, within)`, `flushing(l, r, slots, size, within)` —
      and two objects: `Merge.Ready` (a channel per side joined on the
      ring, the default `given`) and `Merge.Shared` (one queue two
      producers feed: `Channel.merge`, `Channel.mergeChunked`,
      `Channel.mergeFlushing` — one mode in every fork, a consumer that
      never catches up). `Source.merge`, `mergeFlushing` and `either`
      (built on `merge`) dispatch to the given; `Channel.merge*` stay
      callable by name. This is specs/own-or-standard.md's shape for a
      choice between two of OUR mechanisms. Laws: the chunked failure
      law (`TestChannelFailure`, failAfterTail) under both givens; the
      elementwise merge's multiset law under `Merge.Shared`. The
      benchmark's control arms become `given Merge = Merge.Shared`. Literature: this is LMAX
      Disruptor's `WaitStrategy` (BusySpin / Yielding / Sleeping /
      Blocking) made a value the caller chooses, and Karlin et al.'s
      spin-then-block bound on any fixed choice.
- [x] the chunked ring road (f0f355bd4's, rebuilt) on the hybrid:
      `okayChunked` k=16, 10 forks — slow forks, polls, parks per fork
      against the first landing's rows (spin-only: 2-4/10 at 215-247,
      mean 205-211); does the tail move, and does the fork that waits
      2000-3000 polls now sleep instead
- [~] STAGE 2, the bar as accepted (PARTLY, Results below): `okayChunked`, `okayChunkedFlush`,
      `okayChunkedFlushShort` at k = 16/256/1024, ring road against
      today's shared-channel road, 5 forks per arm alternating — mean
      within 1.06x at every lane, no arm's tail worse than 1.2x
- [x] the shared-channel chunked road STAYS, as a door by choice
      (operator, 2026-09-28: "пусть останутся опционально на выбор"):
      `Channel.mergeChunked` and `Channel.mergeFlushing` keep the one
      queue two producers feed — one mode in every fork, a consumer
      that never catches up — and `Source.merge(chunked = true)`,
      `mergeFlushing`, `either` go by default through
      `ReadyMerge[Chunk[A]]` over a chunk channel per side
      (`chunkedSideOf` / `chunkedSideFlushing`) and `Writer.expand`;
      `chunkedMerge` is not deleted; docs name both doors and when each
      is the one to take; `TestChannelFailure`'s chunked laws (the
      failAfterTail ones) green on both
- [x] `ChunkFlushBenchmark` keeps the shared road as a permanent arm
      (`okayChunkedShared`, `Channel.mergeChunked` called directly) —
      the control beside `okayChunked`, no switch in the library
- [~] the elementwise road on the hybrid (not re-measured before the landing, the operator's ask): `MergeCapBenchmark` cap
      64/256/1024 against the first landing's frozen rows (84.8 / 68.3 /
      63.0), no regression

**Results (2026-09-28, the second landing).** Rows in
`src/jmh/history.d/…-ready-merge-chunk-forward-hybrid.tsv`. The
straight ladder against the cycle, `okayChunked` k=16, 10 forks each:
ladder 200.4 ± 3.8 with no fork above 225; cycle 208.0 ± 7.8 with 3 of
10 at 220-247 — the ladder is the default `Wait`, the cycle a choice.
The bar, ring (`Merge.Ready`, `Wait.Ladder(100, 50, 4)`) against shared
(`Merge.Shared`), 5 forks per arm alternating:

| lane | ring | shared | ratio | accepted 1.06x |
|---|---|---|---|---|
| `okayChunked` | 209.7 ± 6.3 | 202.8 ± 4.0 | 1.03 | yes |
| `okayChunkedFlush` (1000 ms) | 238.1 ± 13.8 | 212.0 ± 7.0 | **1.12** | **no** |
| `okayChunkedFlushShort` (1 ms) | not run | 277.4 ± 90.9 | — | timing-bound |

The flushing road on the ring runs a flusher fiber PER SIDE where the
shared road runs one, and reads 12% over with wide bars in one round;
the operator asked for the landing with the numbers as they are, so
the default for `flushAfter` merges stays `Merge.Ready` with this gap
named: backlog `merge-flush-on-ring-gap` (re-measure with more forks;
if it holds, one flusher for both sides, or `Merge.Shared` by default
for the flushing shape). The elementwise lanes on the ladder were not
re-measured before the landing (the operator's ask); the first
landing's spin-100 rows stand as the last reading (84.8 / 68.3 / 63.0),
and the ladder differs from it only on a dry ring, which the
elementwise road reaches ~1-2 times per op. The open door — the
runners honouring `Await.poll` — is the lane `drive-poll-then-park`.
## Stage: poll-then-park at the runners (drive-poll-then-park, 2026-09-28)

The open door of the second stage, opened on the operator's question
("а что там насчет полл и вейт у шаред мержа?"): `Await.poll` was
honoured by `ReadyMerge` alone, so `Merge.Shared`'s consumer — a
`drained` over one queue, run by the plain drive — took `Wait` and
`Pause` and used neither, as did every `drained` consumed without a
merge. `Wait` and `Pause` move DOWN to okay-async, where the runners
live (the `PlatformPause` seam with them; okay-async gains a
`scala-js` source dir). Two runners, two rules:

- the BLOCKING runner (`Async.run`, the `Handler[Async]` under
  `runWith`, `toLazyList`): its thread is its own, so an Await with a
  poll is asked by the given `Wait` on it and parks only when the wait
  gave up (`Async.pollThenBlock`);
- the CALLBACK drive (`runAsync`, fibers): after its first callback it
  runs on whoever woke it — a producer's thread as often as not — and
  a wait there would stall the very producer it waits for. It polls
  ONCE, takes an answer in place, and otherwise registers as always.

- [ ] blocking runner: a poll answered on the yield rung is taken with
      nothing registered (121 polls); one never answered climbs the whole
      ladder (100/50/4 rungs on a counting platform, 154 polls) and then
      parks; `Register` parks at once (0 polls), `Spin(10)` after 10
- [ ] callback drive: one poll — an answer in place is taken (0
      registrations), a miss registers (1 poll, 1 registration)
- [ ] a drained channel under the blocking runner takes what was sent
      before its wait ended without a registration
- [ ] the ring merge's own laws unchanged (its wait stays in
      `ReadyMerge`; the runner's poll never sees a side's Await, which the
      merge holds)
- [ ] MEASURE: `okayChunkedShared` (its consumer, `toLazyList`, is the
      blocking runner) with this against the second landing's row —
      expected parity, its consumer never catches up; `bufferDrained`
      (one `buffer(1024)(s).drained`, no merge) with and without — the
      lane where a catch-up was a registration

## Stage: the collector as the last door (abandoned-lazylist-releases-nothing, 2026-09-28)

A merge read through `toLazyList` (Stream.scala:83, `LazyList.unfold`,
each step its own `runWith`) and ABANDONED partway never ends: no
drive and no fiber handler sees a program end, so the scope entered in
front is never released and the feeder fibers stay parked on full
channels for good — every road, not one mechanism (found by
merge-scopes-everywhere's review). A close the caller can reach is
`runFoldUntil` (the program ends, the drive releases); what a dropped
LazyList needs is a door nobody has to call: the COLLECTOR. A
`CancelScope` registers itself with `Unreachable` at construction — a
`java.lang.ref.Cleaner` on the JVM, nothing on Native and JS, which
have no such API — and the release runs ONCE through whichever door
comes first (`CancelScope.Once`: the drive's end or cancel, the fiber's
handler, the Cleaner). The action holds `once`, never the scope, so the
scope can become unreachable. What keeps a live program's scope
reachable is the program itself: the LazyList's tail thunk holds the
rest, the rest holds the `Exit` node, the node holds the scope. Timing
is the collector's, not ours — a backstop against a leak, not a
deterministic stop. Literature: Boehm, "Destructors, finalizers, and
synchronization" (POPL 2003) on why a finalizer must not touch what
the program still touches and must be idempotent against the other
doors; JEP 421 deprecating `finalize` in favour of `Cleaner`, whose
action runs on its own thread and cannot resurrect its object.

- [x] take 5 of a merged `toLazyList`, drop it, `System.gc()` until the
      release is counted (≤ 10 s) — on `Merge.Ready` and `Merge.Shared`;
      RED with `Unreachable.onCollected` a no-op (the mutant), green with
      the Cleaner
- [x] a program that ends normally releases exactly once, still (the
      existing "a full run releases nothing" law and the early-stop laws)

**Results (2026-09-28).** The door worked on the first try for a plain
source, a buffered channel and a bare scope, and NOT for the merge —
a probe of four cases told them apart in one run. TWO WAYS A SCOPE
STAYS REACHABLE FROM ITS OWN CLEANER, both met here:
1. `ReadyMerge`'s release was `() => { cancelAll(); release() }` — a
   closure over the run, which holds the scope. So the collector gets
   its own door, `onCollected`, given the channels' close alone; a run
   the collector can reach has no registration left to cancel.
2. A lambda written in the class body — `() => once.run(onCollected)` —
   reads `once` and `onCollected` as FIELDS, through `this`, and the
   first fix collected nothing at all, not even the bare scope. The
   action is built by a companion helper from its own parameters
   (`CancelScope.arm`), so nothing in it names the scope.
Both are Boehm's point made concrete: a finalizer's closure is a root.
- [ ] cost: one `Cleaner.register` per scope — per merge RUN, not per
      element (a PhantomReference and a queue entry); measured on
      `okaySourceMerge` beside master if it moves the number


## Stage: the flushing road's gap (merge-flush-on-ring-gap, 2026-09-28)

The second landing left one lane over the accepted 1.06x:
`okayChunkedFlush` (`flushAfter = 1000 ms`, a timer that never fires
in a 240 us op) read 238.1 ± 13.8 on the ring against 212.0 ± 7.0 on
the shared road — 1.12x, five forks per arm, one round. WHERE THE
COST IS, read from the code rather than the earlier guess ("a flusher
per side against one" — `chunkedMerge` runs a flusher per source
too): a side WITH a window has two senders, its feed and its flusher,
so `chunkedSide` builds it `forProducers(2, …)` — `SentinelChannel`
over an `AdaptiveFifo` of two eager parts — and every look the
consumer takes at it is `hasReadyScanning` over both parts and
`popManyScanning` with a claim CAS, where a side without a window is
the single-producer ring `buffer` uses. The shared road has ONE such
two-part channel; the ring road has one PER SIDE and looks at both on
every turn and every poll of the wait ladder.

- [ ] RE-MEASURE before anything moves: `okayChunkedFlush` against
      `okayChunkedFlushShared`, 10 rounds of one fork per lane
      (`-f 1`, the arms alternating A/B/A/B across rounds), the
      unwindowed pair `okayChunked`/`okayChunkedShared` in every round
      as the control (the box moved ⇒ the round is out), through
      `scripts/jmh-lane.sh`, k = 16 pinned — mean and per-fork spread
      per arm; the gap HOLDS if the 10-fork means stay ≥ 1.06x apart
      with the control pair within its own 1.03x
- [ ] `okayChunkedFlushShort` (1 ms) on BOTH arms, 5 rounds, with
      jmh-lane's noise discard raised to 100%: its shared arm read
      ± 91 us on 277 — a 1 ms flusher inside a ~250 us op makes the
      lane timing-bound by construction, so the row says so (and
      what the ring arm reads beside it) rather than pretending a ratio
- [ ] the elementwise road on `Wait.Ladder`, unmeasured since the
      second landing: `MergeCapBenchmark.sourceMergeAtCapacity` at
      64 / 256 / 1024, 5 forks, against the frozen spin-100 reading
      (84.8 / 68.3 / 63.0) — no regression past the bars
- [ ] IF the gap holds, the decision, with the candidates the backlog
      item ordered and the reason each is or is not taken written into
      Decisions: (a) `Merge.Shared`'s shape for the windowed join;
      (b) the flusher signals the feed (refused before it is built: a
      feed parked in its source's pull answers late, the window is not
      honoured); (c) the consumer takes the partial chunk itself when
      its ring runs dry (changes the batching a slow producer sees —
      every element its own chunk once the consumer outruns it — so
      not a flush bound but an eager flush; refused unless measured
      otherwise); the SPSC-keeping variants (the flusher PUBLISHES into
      the `ChunkBuffer` cell and a parked consumer is woken through a
      one-shot registration with a late-answer slot) costed here as
      design, built only if (a) is refused
- [ ] IF the gap does not hold at 10 forks: the item closes as a
      five-fork reading, the rows say so, nothing in the library moves
