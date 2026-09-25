- [ ] stacked-k-requirements — PRIORITY: LOW (nothing has asked).
      `Delim.Stacked` types a captured continuation by the stack its
      code was written under, so ICFP 2011's own shift0 example (`S0
      k1. S0 k2. "A cat" ++ k1 (k2 ".")`: k1 called where its prompt p1
      is consumed) is refused — sound, not complete (TestStackedShift0
      "CONSERVATIVE, pinned"), and stacked `control0` is absent for the
      same reason (its bare segment runs where p is gone). The paper
      accepts the example because k1's SEGMENT never captures to p1.
      THE ROAD: a per-continuation requirement, `k: A => Under[F, R,
      Needs]` with `Needs` the prompts the segment's own captures name
      (a second index, inferred from the body's captures the way `Has`
      is found now), which would also admit stacked `control0`. Not
      worth a lane until a program needs to call a continuation past
      its own consumed prompt; the unstacked doors do it at run time
      today. Literature: Materzok & Biernacki ICFP 2011 (the rule),
      PPDP 2021 (the reduction theory). Source: specs/shift0-dollar.md
      Decisions (shift0-dollar-close, 2026-09-25).
