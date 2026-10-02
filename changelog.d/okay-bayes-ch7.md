## okay-bayes-ch7 - Bayesian Methods for Hackers ch.7: A/B testing by revenue

okay-bayes stage 5c (specs/okay-bayes.md): `Dirichlet` (sample, log
density, mean, covariance — a posterior helper until vector sites) and
`AbTest`: purchase tiers with a Dirichlet posterior per variant, posterior
draws of the revenue per visitor, P(B beats A) and the lift. TestAbTest on
JVM, Scala.js and Native: Dirichlet's moments against closed forms; revenue
per visitor against the exact posterior mean and sd (A 5.173 vs 5.176, B
6.397 vs 6.399); P(B beats A) 0.981 in two independent runs.
