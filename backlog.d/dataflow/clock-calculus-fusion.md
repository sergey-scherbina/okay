- [ ] clock-calculus-fusion — PRIORITY: LOW (reading lane; trigger).
      Biernacki, Colaço, Hammon & Pouzet, "Clock-directed modular code
      generation for synchronous data-flow languages" (LCTES 2008; the
      Lustre/Scade compiler) uses a type of CLOCKS to decide which
      stages run at the same rate and therefore fuse into one loop,
      and where a rate change needs a buffer. That is the question
      specs/strymonas-zip-fusion.md answers case by case for `zip`
      (lockstep sources), and the one stage-pipeline and
      event-time-windows answer at run time. THE LANE: read the
      clock calculus against those three and write down whether a
      clock annotation on `Gen`/stream stages would decide fusion
      statically. The verdict is a spec note either way. TRIGGER: the
      next fusion lane that meets two rates. Source:
      biernacki-literature, 2026-09-24.
