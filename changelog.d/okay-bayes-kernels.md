## okay-bayes-kernels - samplers as combinators

okay-bayes stage 7a (specs/okay-bayes.md; operator, after the typeclass
survey). `Kernel`: a step on a model's `Trace` that leaves the posterior
invariant — `Kernel.site(name)`, `sites`, `everySite` (random-walk MH,
Wingate's structure correction kept), `nuts(names*)` (a NUTS transition on
a block of continuous sites, the others held, unconstrained through their
supports), combined by `>>>`, `mixture(w -> k, …)` and `times(n)`; each
tunes during burn-in only. `Bayes.sample(p, kernel, …)` runs one per chain.
The NUTS transition moved out of `Nuts.sample` into `Nuts.Dynamics`, with
`DualAveraging` and `Adapting` (a kernel's transition, doubling metric
windows) beside it — `Nuts.sample`'s numbers unchanged to the last digit.
TestKernels (JVM, Scala.js, Native): invariance tested directly, one step
of each kernel and combination from 20 000 exact posterior draws, moments
unchanged; TestKernelsHackers: the texting model by nuts(λ1, λ2) >>>
site(τ) against the exact posterior (ESS(λ1) 8000 of 8000), Challenger by
a mixture kernel against the grid.
