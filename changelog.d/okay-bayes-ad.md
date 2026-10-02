## okay-bayes-ad - reverse-mode automatic differentiation for NUTS

okay-bayes stage 3b (specs/okay-bayes.md; operator: "Both: AD optional").
`Real`, a number recorded on a `Tape` (arithmetic, exp, log, log1p, sqrt,
pow, softplus, sigmoid, lgamma with digamma, log-sum-exp; `into`, so a
`Double` is a constant), and one backward sweep for the whole gradient.
The `Grad` effect (`param(name, prior)` answering a `Real`, moving
unconstrained through the prior's support; `score`), differentiable
densities in `Smooth`, and `Smooth.target` / `Smooth.nuts` — the same
`Nuts.sample` the finite-difference target uses. TestSmooth on JVM,
Scala.js and Native: every derivative against a central difference, a model
written both ways equal in density and gradient, conjugates by AD NUTS;
Challenger over `Grad` against the grid (TestHackersNuts). One gradient at
d = 100: 37x cheaper than central differences on the JVM, 102x on
Scala.js, 191x on Native.
