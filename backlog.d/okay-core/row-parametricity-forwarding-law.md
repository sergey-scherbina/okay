- [ ] row-parametricity-forwarding-law — PRIORITY: LOW (a law test).
      Biernacki, Piróg, Polesiuk & Sieczkowski, "Handle with care:
      relational interpretation of algebraic effects and handlers"
      (POPL 2018) proves a free theorem: code polymorphic in a row `F`
      cannot interact with `F`'s operations, and a handler has to
      forward them unchanged. In okay that is the forwarding arm of
      `Effects.handle` (and of `relay`/`translate`). specs/scoped-
      effects-laws.md relies on it without testing it directly. THE
      LANE: a law that runs a row-polymorphic handler under a spy
      handler of an unrelated signature and asserts it sees the same
      operations, in the same order, with the same answers returned.
      "Abstracting algebraic effects" (same authors, POPL 2019) is the
      follow-up for when the forwarding must also HIDE an effect
      (see effect-instances-tunnelling). DONE WHEN: the law is in the
      gate and a handler mutant that swallows or reorders a foreign
      operation fails it. Source: biernacki-literature, 2026-09-24.
