- [ ] okay2-sender-head-recheck — port adaptive-merge-early-stop-livelock
      and abrupt-sender-head-recheck (okay, 2026-09-28) to okay2-stream's
      twins, `SentinelChannel.attemptSend` (:195) and
      `AbruptChannel.attemptSend` (:75). Both carry okay's old else branch:
      a sender kept from pushing by another sender's waiter enqueues, sees
      room, takes its waiter back and retries until the head leaves — under
      a callback drive (`own`, `adaptive`) a spin on the thread that alone
      could move the head (one worker at 100% for 32 minutes in okay, on a
      Merge.Shared `merge` of capacity 4). okay's rule has TWO halves and
      only holds with both: (1) a sender BEHIND another's waiter takes room
      back only as the oldest waiter still waiting (`q.peek() eq w` on the
      queue; the last UNCLAIMED of the list in AbruptChannel), while a
      sender refused by a FULL ring rechecks as before; (2) a waiter taken
      back OWNS the wake and pushes on the next turn without asking the
      queue. Half (1) alone deadlocks TestChannelLaws' two-producer law.
      Red first: okay's TestSendBehindWaiter, ported (two producers into a
      ring of 4, a batch consumer, own and adaptive, 10 s bound).
      (2026-09-28)
