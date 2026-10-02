## okay-bayes-ch2 - Bayesian Methods for Hackers ch.2, adaptive Metropolis

okay-bayes stage 2a (specs/okay-bayes.md): `Bayes.adaptive`, Metropolis
with Haario's adaptation — after half of burn-in the continuous sites move
jointly under the learnt covariance x 2.38²/d (Cholesky, so the proposal
stays symmetric), the scale tuned toward acceptance 0.23; discrete sites
keep single-site moves. `metropolis` is unchanged.

TestHackersCh2 runs the book's chapter 2 against exact answers: the A/B
test (P(pA > pB) 0.8220 vs the exact Beta integral 0.8217) and the
Challenger O-ring regression on the book's data against a 2400 x 2400
grid posterior (E[β] 0.2677 vs 0.2693, p(31°F) 0.9869 vs 0.9874). On
Challenger, ESS(β) over 120 000 draws: 151 single-site, 6879 adaptive.

Found: PyMC 6.3.2's NUTS on the book's Challenger model as written diverges
on all 5000 draws and answers β ≈ -0.26 (the wrong sign), though its logp
and gradient are right; α and β are almost collinear on raw temperature.
The Live oracle (TestHackersPyMC) samples the same posterior through a
linear change of variables and agrees with the grid (β 0.2701, p31 0.9874).

Corrected by okay-bayes-sampler: the collinearity is NOT the cause. PyMC's
NUTS handed okay's density for the same raw-temperature model samples it
with no divergence; the fault is in PyMC's own model graph here.
