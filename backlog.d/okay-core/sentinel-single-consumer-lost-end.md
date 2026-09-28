- [ ] sentinel-single-consumer-lost-end — PRIORITY: MEDIUM since 2026-09-28
      (was HIGH: "a liveness defect, not a flake" — no sighting has yet
      shown a PARKED consumer; see the end of this entry). The ci-runner's whole build of 2026-09-25
      19:27 (range 42861945..041e25ac, gate log okay-gate.qziCWtKoMA)
      failed TestChannelLaws "the end is delivered when close races
      offers on six channels at once — SentinelChannel/single-consumer":
      "a consumer never saw the end of a closed channel: runner 5 round
      42 (finished=true)". The channel SAYS it is finished, and its
      consumer is still waiting. That is the shape of a lost wakeup
      (memories adaptive-seal-race, parked-workers-refute-exhaustion).
      The same law passed for every other SentinelChannel mode in that
      run. THE LANE: reproduce with the law in a loop under load (the
      runner's box was loaded), dump from inside the wait, find the path
      where close publishes the end and the single-consumer park misses
      it. Found by stack-safety-catch-up-okay2's session, not caused by
      it. (2026-09-25)
      RECURRED 2026-09-27 in op-map-constructors' whole-build gate
      (okay-gate.1ya5OvIuUv, a State-only change): "SentinelChannel: a
      consumer never saw the end of a closed channel: runner 2 round 190
      (finished=true)". The same shape as 2026-09-25.
      WORKED 2026-09-27 (operator: flaky gates). NOT reproduced alone:
      the law in a loop, 12 runners, 20 000 rounds each (240 000 closes
      racing offers), every core burning beside it: 0 hangs. Both
      sightings were inside a whole-build JVM with many suites at once,
      and `finished=true` means the consumer had drained everything and
      needed only the end mark. That fits a starved RUNNABLE virtual
      thread as well as a lost wakeup. The law now tells them apart: at
      5 s it records the consumer's thread state (WAITING = parked on
      the channel, RUNNABLE = starved), its stack, and
      `SentinelChannel.debugState` (closing/endPending/ended/marks/size/
      receivers). It then waits up to 60 s more. LATE is logged to stderr
      and does not fail. LOST fails, carrying the diagnosis. NEXT: when it
      fails again, read the message. `endPending=true` with `size=0` and
      thread WAITING is the placeEnd-never-retried path (close could not
      seal a full ring and nothing re-tried after the last pop); anything
      else points at the handoff.
      SIGHTING 2026-09-28 (ready-merge-chunk-forward), a different
      channel, the same law: `TestCoreAsyncChannelLaws` "the end is
      delivered when close races offers on six channels at once —
      CoreAsyncChannel" hung to munit's 30 s timeout (the law's own
      5 s + 60 s wait ran to 65.1 s) TWICE, alone and in the whole
      build, on a branch based on master 3826e2e8c — while that base
      alone, in a fresh worktree, was green, and the same branch rebased
      onto 09036dd58 (after adaptive-outside-long-fibers-serial's
      scheduler fix, "outside forks that waited a tick wake parked
      workers") was green. The branch touches nothing on that law's path
      (offer/receiveBlocking/close on a core.async-backed channel). So
      the hang depends on scheduling, not on the channel: a starved
      consumer, the RUNNABLE reading this entry's diagnosis predicts —
      and it can reproduce deterministically for a given class layout.
      If it recurs after that fix, the six-runner law itself is the
      reproducer: it hung 2/2 on one tree and 0/3 on its neighbours.
      A THIRD sighting the same quarter hour (supervised-waits-on-failure's
      whole-build gate, 02:19, base BEFORE 09036dd58; 3/3 green alone on
      that tree; the law drives raw `Thread.ofVirtual`/`ofPlatform`, no
      Scheduler, no cancel, so nothing in that lane's diff reaches it) —
      consistent with the scheduling reading above. The CoreAsync variant
      carries no LATE/LOST diagnosis yet — it fails as a bare munit
      timeout — so the diagnosis the SentinelChannel variants got belongs
      in the SHARED law, not in one channel's suite.
      WORKED 2026-09-28 (operator: fix the flakes; changelog.d/
      sentinel-single-consumer-lost-end.md). What was wrong with every
      sighting: `ChannelLawsSuite` had munit's 30 s timeout and the law
      waits 65 s, so each red was a bare TimeoutException and the
      diagnosis never surfaced — fixed (3 min), and the diagnosis is per
      implementation now (`describe` hook, `CoreAsyncChannel.debugState`).
      `LateOrLost` tells a STARVED consumer (RUNNABLE at the last
      deadline: a wakeup it has, a carrier it lacks) from a LOST one
      (parked): the law logs the first and fails the second. The sibling's
      2/2 tree (a9aa44111) ran 2/2 green with all that in — the box, not
      the tree. The "placeEnd never retried after the last pop" path
      predicted above is refuted by reading: `Ring.push` decides fullness
      by the slot's STAMP, which `pop` publishes after moving the head,
      and every consumer path that frees a slot calls `placeEnd`. NEXT:
      the next red of this law is a `Lost` carrying two thread snapshots
      and the channel's flags — read those; a `Starved` line on stderr in
      a whole-build log is the box and needs nothing.
