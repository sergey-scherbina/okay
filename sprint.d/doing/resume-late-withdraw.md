- [ ] resume-late-withdraw — `TestMergeOrder` "Channel.merge: each side
      arrives in exactly the order it sent" RED on master since f50932cbe
      (a sibling bisected it: 5da64e406 400/400 green, f50932cbe red at
      round 9). Cause: `AdaptiveFifo.route()` is per THREAD, and a
      producer's per-side order leans on it writing from one thread;
      `DriveTask.resumeLate` moved each resumed producer to another worker.
      Withdraw the handoff (inline, as before), TestMergeOrder at
      OKAY_MERGE_ROUNDS=400 green; file the per-producer route as the way
      back to the cap-64 win. (2026-09-29)
