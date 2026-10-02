## okay-bayes-smc - sequential Monte Carlo and the model evidence

okay-bayes stage 2b (specs/okay-bayes.md): `Bayes.smc(p, particles, seed)`.
Each particle is the program suspended at its next `Factor`; particles are
reweighed at every factor and resampled (systematic) when the ESS falls
under half — a particle chosen twice resumes one continuation twice
(multi-shot), and each copy draws its own future. Answers `Particles`
(values, sites, weights, `expect`, `mean`, `ess`) and `logEvidence`, the
estimate of log p(data) that a Bayes factor needs and MCMC does not give.
`observeEach` observes one factor per element.

TestSmc, green on JVM, Scala.js and Native: a 50-step random walk against
the Kalman filter (E[x49 | y] 1.9735 vs 1.9735, log p(y) -101.706 vs
-101.616), Beta–Bernoulli posterior mean and evidence against closed forms,
a log Bayes factor (0.182 vs 0.145), and copies sharing a continuation.
