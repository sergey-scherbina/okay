## okay-bayes-ch6 - Bayesian Methods for Hackers ch.6: Thompson sampling

okay-bayes stage 2d (specs/okay-bayes.md): `Bandit`, Thompson sampling over
Bernoulli arms — a Beta posterior per arm, `choose` draws once from each and
pulls the largest, `observe` is the conjugate update; immutable, a run is a
fold (`Bandit.play`). TestBandit, on JVM, Scala.js and Native: the choice
frequencies equal P(arm is best) by numerical integration (0.2541 / 0.6103 /
0.1355 against 0.2546 / 0.6098 / 0.1357); on the book's arms the mean
regret is 14.3 at 1000 pulls and 20.9 at 10 000 (random play 1167), the
best arm's share 0.983. The regret sits below the Lai–Robbins rate at these
horizons, which the bound (asymptotic) allows; the spec says so.
