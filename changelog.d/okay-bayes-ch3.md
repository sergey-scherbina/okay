## okay-bayes-ch3 - Bayesian Methods for Hackers ch.3: a mixture and its convergence

okay-bayes stage 2c (specs/okay-bayes.md): `Distribution.Mixture`, a
weighted mixture whose density sums the components out (log-sum-exp; the
assignments are never sampled), and TestHackersCh3 on the book's
`mixture_data.csv`. Four `adaptive` chains agree (split R-hat 1.0005–1.0020)
and agree with an independent oracle, self-normalised importance sampling
from a multivariate Student-t (200 000 draws, ESS 29 564): centres 120.19 /
199.60 against 120.16 / 199.53, sds 30.19 / 22.84 against 30.16 / 22.88,
p 0.376 against 0.375; P(first cluster | x = 175) 0.149 against 0.147. The
book's convergence lesson, measured: R-hat 1.70 over the first 100 draws
from prior starts, 1.002 after burn-in.
