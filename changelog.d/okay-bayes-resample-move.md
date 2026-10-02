## okay-bayes-resample-move - resample-move SMC on the kernels

okay-bayes stage 7b (specs/okay-bayes.md). `smc(p, particles, seed, move
= Some(kernel))`: after each resampling every particle is moved by a
`Kernel` whose target is the posterior given the observations so far — the
model re-run with its sites held and stopped after the same factor
(Gilks & Berzuini 2001; Chopin 2002). `Trace` now carries how to re-run
its model, so a kernel moves a whole model in a chain and a particle's
prefix in SMC alike. SMC rewritten over one interpreter, `runFactors`
(advance a particle by one factor, or re-run a prefix to the k-th), every
random draw in the old order: TestSmc's numbers unchanged. TestResampleMove
(JVM, Scala.js, Native): 400 flips one at a time, 259 of 1000 distinct
values without the move, 968 with it, the posterior and log evidence
against the closed form.
