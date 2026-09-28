- [ ] okay2-sender-head-recheck — port adaptive-merge-early-stop-livelock
      (okay, 2026-09-28): `SentinelChannel.attemptSend`'s else branch
      (a sender queued behind
      another's waiter) rechecks room and retries; under a callback
      drive that is a busy-wait for the sender ahead, which only the
      thread we spin on could wake — one worker at 100% for 32 minutes
      on a Merge.Shared `merge` of capacity 4. okay retries only when
      its own waiter has become the HEAD of the queue (`q.peek() eq w`;
      okay2-stream's twin (`SentinelChannel.scala:225`) carries the same
      lines and the same exposure on `own`/`adaptive`. NOT AbruptChannel:
      the same rule over its list hung okay's TestChannelLaws (okay backlog
      abrupt-sender-head-recheck says how). Red first:
      TestReadyMerge's law "Merge.Shared on a callback scheduler …",
      ported. (2026-09-28)
