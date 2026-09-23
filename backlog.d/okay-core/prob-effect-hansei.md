- [ ] prob-effect-hansei — PRIORITY: MEDIUM. probabilistic programming as an effect, the
      Hansei design: Kiselyov & Shan, "Embedded probabilistic
      programming" (DSL 2009) — `dist(choices)` is an operation, exact
      inference is a HANDLER that explores every branch with its weight
      (multi-shot continuations, which this library has natively and
      the one-shot runtimes measured in docs/benchmarks.md cannot
      follow), `fail` prunes, and lazy/importance sampling are other
      handlers over the SAME program. Nothing of it exists here (no
      module, no spec). Sits on `Logic`/`Choose` + weights, or on
      `Delim` directly; `Once` gives memoised sub-models for free
      (call-time choice, Fischer–Kiselyov–Shan 2009). THE LANE:
      `okay-prob` (or `Prob` in core if it is small): `dist`, `observe`
      (condition), exact enumeration answering a `Map[A, Double]`,
      rejection and importance sampling as two more handlers; the
      classic grass/rain model and a small HMM as tests against
      hand-computed posteriors. NUMBER: exact inference over the
      textbook models against a hand-written enumerator — the price of
      multi-shot per branch. A showcase of what only a real multi-shot
      runtime can do; docs with the paper. (2026-09-23)
