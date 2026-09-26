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

- [ ] each source's elements arrive in exactly the order it told them
- [ ] nothing lost, nothing invented: the multiset of outputs is the
      union of the inputs
- [ ] all sources always ready (no `Await` anywhere) ⇒ strict
      round-robin: s0, s1, …, s(n-1), s0, …; an ended source drops out
      and the turn passes on — a DETERMINISTIC interleave
- [ ] a parked source does not hold the others: a source waiting on a
      timer/channel does not delay elements another source has ready
- [ ] when every live source is parked, the merge itself parks ONCE
      (one `Await`) and resumes on the first wake-up; woken sources
      rejoin the ring in wake order
- [ ] a callback that fires DURING its own registration (synchronous
      answer) is not lost and does not recurse
- [ ] `Async.Run` is performed in the source's own turn, in place
- [ ] a failing source (a `Left` answer, a throwing `Run`) fails the
      merged program, and the other parked sources' registrations are
      cancelled
- [ ] cancelling the merged program while it is parked cancels every
      parked source's registration
- [ ] stack-safe: 10^6 elements, and 10^5 synchronous wake-ups
- [ ] parallelism by buffering: a `Channel.buffer`ed side keeps its
      order and merges with an unbuffered one
- [ ] the merged source is consumed by an iteratee (`Take`/`through`)
- [ ] cross-platform: the laws that need no thread run on JS too
- [ ] the numbers: `MergeBenchmark` 2x500, see Results

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
   (its turn continues); `Await(reg)` + `k` → register a callback that
   on `Right(x)` sets `slot(i) = k(x)`, clears `cancels(i)`, enqueues
   `i`, fires the waker; on `Left(e)` stores the failure and fires.

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
- **Early stop is not cancellation** — a consumer that stops reading
  (`take`, `runFoldUntil`) drops the merged program without running it,
  so a PARKED source's registration stays registered and its answer is
  dropped when it fires. `Source.merge` has the same property (its
  fibers keep feeding a channel nobody reads). A source whose
  registration consumes data (a channel receive, whose canceller is
  `() => ()`) loses that element either way; recorded, not solved here.

## Results

(filled by the lane)
