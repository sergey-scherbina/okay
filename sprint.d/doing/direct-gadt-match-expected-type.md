- [ ] direct-gadt-match-expected-type — PRIORITY: MEDIUM. Since
      `Free[F, +A]` (free-answer-upcast) a `match` in a `direct`
      block's tail at an abstract `X` whose FIRST arm is concrete
      types at that arm (`Long`) and the later arms fail; the user
      writes `(e match …): X` (TestDirectOnce, the one user-visible
      price of the covariance). The macro knows the block's answer
      type and could ascribe it to a tail `match` itself before
      typing the arms — the "expected type reaches the arms" rule the
      `transduce` signature comment already relies on. THE LANE: the
      macro ascribes; TestDirectOnce loses its `: X`; a test with a
      concrete first arm and an abstract answer. (2026-09-23)
