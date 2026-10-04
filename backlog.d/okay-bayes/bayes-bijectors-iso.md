- [ ] bayes-bijectors-iso — operator 2026-10-03 (typeclass survey, point 8).
      `Support.constrain` is an isomorphism ℝ ↔ (0, ∞) / (a, b) carrying a
      log-Jacobian — what TensorFlow Probability calls a bijector. As an
      okay-optics `Iso` plus its log |det J|: user-declared transforms for
      NUTS (a site's own unconstrained space), and an honest
      `Distribution.map` along a bijection with the density recomputed
      (log-normal = Normal mapped by exp). Oracle: the transformed density
      against its closed form; NUTS through a user bijector against the
      built-in one.
