# A partitioned channel routes by producer, not by thread

Status: in progress, 2026-09-30. Owner lane: `channel-route-per-producer`.

## Why

A partitioned buffer (`AdaptiveFifo`, what `forProducers(n > 1)` builds)
gives every producer a part and keeps each producer's order because a
part is a FIFO. "Producer" there means THREAD: `route()` is a
ThreadLocal home. On Loom a fiber is a thread, so that was the same
thing. On `own`/`adaptive` it is not — a fiber parks and is resumed on
another thread — and a feed that moved wrote its next run into another
part, read out of order. resume-late-withdraw (2026-09-29) met it as a
red `TestMergeOrder` when resumed fibers were sent home, and withdrew
the handoff that had taken the cap-64 elementwise merge from 1.12x Loom
to 0.92x.

## The design

- `Buffer.claimRoute(): Int` — a route for a producer that is NOT a
  thread, claimed once and carried by the caller. Default: `route()`
  (a buffer with one order has one route). `AdaptiveFifo` claims a
  part the way a new thread's home does.
- `Channel` gains, `private[okay]`, `claimRoute(): Int` (default -1:
  no route) and `offerFrom(route, a)` / `sendFrom(route, a)` (default:
  `offer` / `send`). `SentinelChannel` pushes and parks on the route
  it is given instead of the thread's.
- The library's own multi-producer channels route their feeds by
  producer: `Channel.merge`'s two feeds, `chunkedMerge`'s (the shared
  chunked road) and `mergeFlushing`'s each claim a route at fork; a
  side's flusher sends on its feed's route, so a side is one FIFO
  whoever pushes; `chunkedSide` with a window likewise.
- A user's own senders into a partitioned channel still route by
  thread, as documented; nothing about `offer`/`send` changes.
- Then the foreign-resume handoff returns (`DriveTask.resumeLate`,
  `fork`), since no library channel's order depends on the thread any
  more.

## Behaviour

- [ ] LAW, red first: a producer that writes from two different
      threads through a claimed route keeps its order in a
      `forProducers(2)` channel; through `offer`/`send` from those same
      threads it does not
- [ ] TestMergeOrder green at OKAY_MERGE_ROUNDS=2000 WITH the handoff
      back (red on the handoff without the routes: the reason)
- [ ] the channel, merge and zip laws unchanged; the cap-64 elementwise
      merge back under Loom, zip cap 7/64 and the chunked lanes not worse
