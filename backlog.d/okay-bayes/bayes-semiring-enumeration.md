- [ ] bayes-semiring-enumeration — operator 2026-10-03 (typeclass survey,
      point 5). One discrete model, several answers by the semiring its
      handler sums in: log-sum-exp gives exact marginals (what `Mixture`
      does for one choice), max-plus the most probable explanation (MAP,
      Viterbi). An exact enumerating handler over `Model` for finite
      discrete sites, parameterised by the semiring; the core's `Prob`
      (exact enumeration) is the precedent. Oracle: a small HMM — forward
      algorithm and Viterbi in closed form.
