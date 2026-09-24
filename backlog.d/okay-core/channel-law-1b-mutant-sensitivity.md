- [ ] channel-law-1b-mutant-sensitivity — TestChannelLaws law 1b was
      written to catch a `Ring.pushDeciding` that reads the closing flag
      BEFORE winning its position (channel-law-racing-offers: red at
      rounds 24, 179, 3 when it landed). Re-applied on 2026-09-24
      (channel-close-wakeup-core), that mutant PASSED law 1b in a full
      300-round run on this box — with the law as it stood (a random
      pause before close) and with two variants tried (close after a
      counted number of accepted offers, targets 4096 and 48). The
      window is the few nanoseconds between the flag read and the CAS,
      and whether 300 rounds hit it depends on the box. A law whose
      mutant passes is a guard that cannot fail: find a shape that
      catches it every run (widen the window deterministically — e.g. a
      test hook between decide and claim, or many more producers against
      a ring kept non-full), and run the mutant as part of the lane.
      (2026-09-24)
