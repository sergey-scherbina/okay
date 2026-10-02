## okay-bayes - Bayesian inference without Python (stage 1)

A new module (specs/okay-bayes.md): distributions with densities, draws,
symmetric proposals and `coerce` (Normal, Exponential, Gamma, Beta,
Uniform, Poisson, Bernoulli, Binomial, DiscreteUniform); the `Model` effect
— named `sample`, `observe` as a likelihood factor; handlers `prior`,
`weighted` (likelihood weighting) and `metropolis` (lightweight MH over
named traces, with the trans-dimensional correction, PyMC's tuning rule,
several chains); `Summary` (mean, sd, quantiles, HDI, ESS, split R-hat).
Against three oracles: closed-form conjugate posteriors, the EXACT
posterior of *Bayesian Methods for Hackers*' chapter-1 texting model on
the book's data (E[λ1] 17.758 exact, 17.747 here, 17.757 PyMC 6.3.2), and
PyMC itself through okay-py (Live). Stages 2–4 (SMC, HMC/NUTS, the
Inference facade) follow.
