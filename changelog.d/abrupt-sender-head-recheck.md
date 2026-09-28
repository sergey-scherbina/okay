## abrupt-sender-head-recheck - AbruptChannel's sender parks behind another's waiter too; the send rule is one rule on every bounded channel

- `AbruptChannel.attemptSend` carried the busy-wait adaptive-merge-early-
  stop-livelock removed from `SentinelChannel`: a sender kept from pushing
  by another sender's waiter enqueued, saw room, took its waiter back and
  retried until the head left. Under a callback drive the head could only
  move on the thread spinning. RED FIRST, new suite TestSendBehindWaiter:
  two producers feeding a ring of 4 the way `Channel.merge`'s feed does,
  a batch consumer (`drained`), 20 rounds, 10 s bound — AbruptChannel
  never answered on `own` nor on `adaptive`; SentinelChannel (already
  fixed) green on both.
- THE PORT keeps SentinelChannel's two halves, and names the case its one
  branch used to fold together: `pushed` says whether this attempt got to
  push at all. A FULL ring (it pushed and was refused) rechecks as it
  always did. A sender BEHIND another's waiter takes room back only when
  its waiter is the oldest still waiting (`oldest`: the last UNCLAIMED of
  the list, since `enqueue` conses at the head; a waiter another thread
  already took back is its owner's, not ahead of us). And a waiter taken
  back OWNS the wake it was skipped for: `own` makes the next turn push
  without asking the queue. The head rule alone had deadlocked
  TestChannelLaws' two-producer law for this channel; with both halves it
  holds.
- Gate: TestSendBehindWaiter 4/4 and TestChannelLaws 111/111; the whole
  okayStreamJVM suite twice; TestSendBehindWaiter, TestChannelLaws and
  TestReadyMerge under `-Dokay.scheduler=adaptive`; okay-clojure's
  channel laws.
