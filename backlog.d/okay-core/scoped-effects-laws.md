- [ ] scoped-effects-laws — PRIORITY: HIGH. the scoped (higher-order) operations are
      half done. `Reader.local` does not exist (Reader.scala has `ask`,
      `read`, `lift` only); `Throws.recover` exists and NOTHING pins
      what it does to the effects around it — whether a `State` change
      made inside a recovered block is rolled back or kept, which is
      decided by handler ORDER (transactional under `State.run` inside,
      global outside) and is today whatever the walkers happen to do.
      Literature, in order: Wu, Schrijvers & Hinze, "Effect handlers in
      scope" (Haskell 2014, the question itself); Piróg, Schrijvers, Wu
      & Jaskelioff, "Syntax and semantics for operations with scopes"
      (LICS 2018); Yang, Paviotti, Wu, van den Berg & Schrijvers,
      "Structured handling of scoped effects" (ESOP 2022); Bach Poulsen
      & van der Rest, "Hefty algebras" (POPL 2023, scoped ops as a
      second layer over algebraic ones). THE LANE: `Reader.local(f)(p)`
      (the environment changed for `p` only, correct under a capture
      that re-enters `p` — Kiselyov, Shan & Sabry, "Delimited dynamic
      binding", ICFP 2006, is the trap: a captured continuation must
      carry its dynamic environment); the scoped laws as tests —
      `recover` × `State` in both handler orders, `local` × `shift`,
      `Logic.cut`'s scope — each answer stated in the spec, not
      discovered; docs with the two orders side by side. Found while
      surveying the literature for gaps (2026-09-23).
