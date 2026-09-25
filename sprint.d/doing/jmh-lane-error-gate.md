- [ ] jmh-lane-error-gate — `scripts/jmh-lane.sh` accepts a lane when the
      box was quiet at its start and its end, and that misses a sibling's
      gate that starts and ends INSIDE the lane: channel-default-adaptive
      (2026-09-25) accepted rows of +-60% that way until its own driver
      added "accept only at error <= 10% of the score", after which the
      rows it kept were tight and the verdict stood. THE LANE: jmh-lane.sh
      reads its run's JMH result rows and treats any primary row (not a
      `:secondary` metric) with error above JMH_LANE_MAX_ERR percent of
      its score (default 10; 0 turns it off) as contaminated — discard,
      retry, like a busy box. Selftest cases for noisy-then-tight, the
      switch off, and a noisy secondary metric ignored; sh and bash.
