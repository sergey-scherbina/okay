- [ ] atm-beyond-state-docs — PRIORITY: MEDIUM. answer-type modification reads in the
      docs as "the thing PState does", and it is the general property
      of `Cont[A, S, R]`'s `shift` (the continuation answers `S`, the
      block `R`): `PState` is one use, `Stage.phased`'s typestate is
      another, `Prog` is the same idea on `Free`. The operator asked
      whether it is "only for state" (2026-09-23) — the docs let that
      question arise. THE LANE, docs with tested examples: Asai's typed
      `printf`/`sprintf` (Asai, "On typing delimited continuations:
      three new solutions to the printf problem", HOSC 2009) as the
      canonical non-state use, a protocol whose PHASES are answer
      types, and one sentence in theory ch. 2 placing PState, phased
      and Prog on the one mechanism; Danvy & Filinski, "Abstracting
      control" (1990) for where the typing comes from. Every snippet
      in a gated test. (2026-09-23)
