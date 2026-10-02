## okay-bayes-nuts - the No-U-Turn sampler

okay-bayes stage 3a (specs/okay-bayes.md; operator: "Both: AD optional").
`Target` (a dimension, log density and gradient on ℝᵈ), `Target.finite`
(central differences) and `Nuts.sample`: Hoffman & Gelman's Algorithm 6
with dual averaging and a diagonal mass matrix learnt in warmup (Stan's
regularised variance). `Distribution.support` (`Support`: Real, Positive,
Interval, Discrete) with `constrain`/`unconstrain` and the log Jacobian;
`Bayes.nuts(model, ...)` samples an ordinary model in unconstrained space,
refusing a discrete site, a support that moves with another site, or a
structure that changes with a draw. Oracles: three conjugates through the
three transforms (TestNuts, JVM/JS/Native), Challenger against the grid and
the ch.3 mixture against importance sampling (TestHackersNuts) — zero
divergences, Challenger included, on the raw temperatures PyMC's NUTS
diverged on. AD gradients (stage 3b) plug into the same `Nuts.sample`.
