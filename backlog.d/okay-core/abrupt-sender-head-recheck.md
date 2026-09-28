- [ ] abrupt-sender-head-recheck — port adaptive-merge-early-stop-livelock's
      rule to `AbruptChannel.attemptSend` (2026-09-28). Its else branch is
      SentinelChannel's old busy-wait: a sender queued behind another's
      waiter rechecks room and retries until the head leaves, which under
      a callback drive is a spin on the thread that would wake the head.
      The rule that fixed SentinelChannel has TWO halves and only holds
      with both: (1) retry only when our waiter is the HEAD of the queue
      (`q.peek() eq w`; the list's OLDEST here — `enqueue` conses at the
      head, `wakeOne` takes `last`); (2) a sender that claimed its own
      waiter back on seeing room OWNS that wake and pushes at once, never
      re-asking the `isEmpty` gate. Half (1) alone, ported as
      `senders.get.lastOption.exists(_ eq w)`, deadlocked TestChannelLaws'
      "TWO producers each arrive in the order they sent — AbruptChannel"
      twice (both producers parked in `sendBlocking`, the consumer in
      `receiveBlocking`, the fork's dump), which is how half (2) was found.
      AbruptChannel folds the full-ring park and the queued-behind case
      into one else branch (`(granted || isEmpty) && ring.push(a)` false
      falls through), so `own` must gate the push there the same way. No
      merge runs on this channel (its own header), so nothing hits the
      spin today. Red first: TestReadyMerge's callback-scheduler law's
      shape over two producers into an AbruptChannel on
      `Schedulers.adaptive.build`. (2026-09-28)
