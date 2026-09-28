## adaptive-merge-early-stop-livelock - a sender queued behind another's waiter parks; it no longer spins for a fiber only its own thread could run

- With `adaptive` as the given, TestReadyMerge never finished: one worker
  at 100% CPU in `SentinelChannel.attemptSend` for 32 minutes, on the
  early-stop law over `Source.merge(..., capacity = 4)` (scheduler-
  default-rerun, 2026-09-28). Loom on the same tree: 29/29.
- THE MECHANISM, read from the dump and reproduced: the shape is ONE ring
  fed by two producers (Merge.Shared) both parked on it full. The consumer
  pops and wakes the first; on a callback drive (`own`, `adaptive`) the
  wake resumes that producer's fiber INLINE on the consumer's thread.
  Its next `send` meets the second producer's waiter at the head of the
  queue, sees room, and the else branch's recheck — enqueue, room, claim
  our own waiter back, remove, retry — looped until the head left. The
  head is a fiber only the consumer's next wake could resume, and the
  consumer is the thread we are spinning on. On Loom the wake is an
  unpark and the sender ahead runs on its own carrier, so the loop was
  short there and never seen.
- THE FIX, in `SentinelChannel.attemptSend`, in two halves that only
  hold together. (1) The else branch's recheck retries only when our
  waiter has become the HEAD of the queue (`q.peek() eq w`): that is the
  one lost wakeup it exists for — the queue drained between the `isEmpty`
  test and our enqueue, so the freed slot's wake found nobody and only we
  are left to take it. With a waiter still ahead, that slot's wake is in
  flight to it and every later pop wakes one more head, so we park.
  (2) A SPENT WAKE IS OWNED: a sender that claimed its own waiter back on
  seeing room pushes at once (`own = true`), like a resumed one. Before,
  it looped back to the `isEmpty` gate, met a waiter that had arrived
  BEHIND it, and went to the else branch instead of pushing — the slot's
  wake had already found that waiter claimed and moved on, so nobody held
  it. The old retry-until-the-head-leaves hid that: two senders each
  re-enqueued behind the other's waiter until one caught the queue
  momentarily empty. Half (1) alone turned that into a deadlock —
  TestChannelLaws' "TWO producers each arrive in the order they sent"
  hung three whole okay-stream runs in three (both producers parked in
  `sendBlocking`, the consumer in `receiveBlocking`, in the fork's dump),
  while the pristine tree ran 439/439; with (2) beside it the whole
  module is green again. FIFO among parked senders is unchanged.
- RED FIRST: TestReadyMerge "Merge.Shared on a callback scheduler: a woken
  producer that meets another's waiter parks, it does not spin (own and
  adaptive)" — 20 rounds each, a join bounded at 10 s. Before the fix:
  `adaptive round 13: the merge never answered` (own passed its 20).
  After: 30/30 under the Loom given, and 30/30 with
  `-Dokay.scheduler=adaptive` in okay-stream's test fork — the run that
  hung. okayStreamJVM's whole suite and okay-clojure's channel laws
  (test->test on the shared suite) are the gate.
- NOT AbruptChannel, though its else branch has the same shape: half (1)
  alone over its waiter LIST hung the same law for AbruptChannel twice —
  the same deadlock, found there first, before half (2) was understood.
  Reverted there; filed as backlog okay-core/abrupt-sender-head-recheck
  with the two-half rule to port. No merge runs on it.
- NOT DONE HERE: the default flip. specs/schedulers.md's rule says the
  fix touches the channel's send path, so TCP, Wrocław and the fork/join
  rows are re-run before `Schedulers.auto` moves — a JMH lane on the
  operator's box. okay2's twin channels carry the same else branch;
  filed there as okay2/backlog.d/modules/okay2-sender-head-recheck.md.
