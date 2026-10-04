- [ ] bayes-map-laplace — operator 2026-10-03 (the "what next" list; "запиши
      в беклог"). `Bayes.mode(model)` / `Smooth.mode`: the most probable
      point by gradient ascent on the unconstrained log density (L-BFGS
      over the AD `Target`), and the Laplace approximation around it — a
      Gaussian from the Hessian (finite differences of the AD gradient).
      Uses: a fast approximate posterior; a GENERAL start for NUTS, which
      replaces Dark Worlds' hand-written coarse search (`DarkWorlds.start`)
      and answers the local-mode trap found there (specs/okay-bayes.md 5d)
      only where the mode search itself escapes local modes — so several
      starts, the best taken. The "max instead of sum" of the typeclass
      survey in practical form. Oracle: conjugates (mode and curvature in
      closed form), Challenger against the grid's mode.
