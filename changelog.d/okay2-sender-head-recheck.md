## okay2-sender-head-recheck - okay2's two bounded channels park a sender behind another's waiter too

- okay2-stream's `SentinelChannel.attemptSend` and `AbruptChannel.attemptSend`
  carried the busy-wait okay removed the same day (adaptive-merge-early-
  stop-livelock, abrupt-sender-head-recheck): a sender kept from pushing by
  another sender's waiter re-enqueued, saw room, took its waiter back and
  retried until the head left, which under a callback drive only the
  spinning thread could make happen.
- RED FIRST: okay's TestSendBehindWaiter, ported over okay2's
  SchedulerFamily (loom, drive, own, own.forShortTasks, own.forLongTasks,
  adaptive). On the old code 10 of 12 failed at the 10 s bound, both
  channels on every callback scheduler; Loom passed both, which is why
  nobody saw it.
- THE PORT is okay's two halves: a sender BEHIND another's waiter takes
  room back only as the head (`q.peek() eq w`; the oldest unclaimed of the
  list in AbruptChannel), a sender refused by a FULL ring rechecks as
  before, and a waiter taken back owns its wake (`own`) and pushes on the
  next turn without asking the queue.
- okay2Stream/test 240/240, twice.
