- [ ] sentinel-single-consumer-lost-end — PRIORITY: HIGH (a liveness
      defect, not a flake). The ci-runner's whole build of 2026-09-25
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
