- [ ] okay2-joinwithin-settle-flake — TestSourceJoinWithin's "a finite side
      against an endless one" asserts the endless side SETTLES with a
      `Thread.sleep(50)` between two reads of its production counter
      (TestSourceJoinWithin.scala:58). Red once in a full okay2 gate on a
      loaded box (the gate had waited 300 s on a benchmark), green alone
      the same hour (okay2-shift-merge, 2026-10-02; the lane touched no
      stream code). A sleep is not a settle: assert the feeder PARKED (its
      buffer full and the producer blocked, read from the stage's own
      state) instead of a counter standing still for 50 ms, or tag the
      suite as timing-dependent. (2026-10-02)
