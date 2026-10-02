# okay-bayes

> Bayesian inference without Python: a model is a program that draws
> named random quantities and weighs its run by what it observed, and
> inference — a forward draw, likelihood weighting, Metropolis–Hastings —
> is a handler over it. The book *Bayesian Methods for Hackers* is the
> yardstick: its models, its data, its answers.

Depends on: `okay` (core). Pure Scala — cross-built for JVM, JS and
Native. Spec: `specs/okay-bayes.md`. PyMC appears only as a test oracle.

## Guide

**A model is a program.** `sample(name, dist)` draws a quantity — the
NAME is its address, which is how Metropolis–Hastings knows which draw it
changed between two runs, as PyMC names its variables for the same
reason. `observe(dist, x)` weighs the run by the likelihood of `x`
(`factor(dist.logPdf(x))`), it does not filter it. The program's own value
is whatever it yields — a tuple, a case class — and the posterior comes
back typed: `Vector[A]`, no cast.

The book's chapter 1, inferring a change in someone's texting from 74
days of message counts, is the model line for line as the book writes it
in PyMC:

```scala
val texting = for
l1 <- sample("lambda_1", Exponential(alpha))
l2 <- sample("lambda_2", Exponential(alpha))
tau <- sample("tau", DiscreteUniform(0, n - 1))
_ <- observeAll(counts.zipWithIndex)((_, day) => Poisson(if day < tau then l1 else l2), _._1)
yield (l1, l2, tau)
val post = metropolis(texting, samples = 30000, burn = 5000, chains = 2)
```

**Inference is a handler.** `prior` runs the program once forward;
`weighted` is likelihood weighting; `metropolis` is LIGHTWEIGHT
Metropolis–Hastings (Wingate, Stuhlmüller & Goodman 2011): every iteration
proposes a symmetric move at each site of the current trace, re-runs the
program reusing every other site's value, and accepts by the ratio of the
joint densities — with the correction for sites that appear or vanish, so
a model whose STRUCTURE depends on a draw is sampled correctly. Proposal
scales are tuned per site during burn-in by PyMC's own Metropolis rule.

**Reading the posterior.** `post.site("lambda_1")` is a site's chain,
`post.draws` the program's values; `Summary` has the mean, sd, quantiles,
the highest-density interval, the effective sample size (Geyer's initial
positive sequence) and split R-hat over chains — ArviZ's definitions, so a
number reads as PyMC's would.

## How it is known to be right

Three oracles, none of them "close to the book":

| | exact | okay-bayes |
|---|---|---|
| ch.1 texting, E[λ1] | 17.758 | 17.747 |
| ch.1 texting, E[λ2] | 22.689 | 22.724 |
| ch.1 texting, P(τ = 44), P(τ = 45) | 0.365, 0.486 | 0.368, 0.489 |
| Beta–Bernoulli, mean ± sd | 0.6765 ± 0.0791 | 0.6726 ± 0.0786 |
| Gamma–Poisson | 3.6364 ± 0.5750 | 3.6367 ± 0.5821 |
| Normal–Normal | 1.5278 ± 0.2827 | 1.5293 ± 0.2813 |
| a draw choosing which sites exist, P(coin) | 0.4295 | 0.4316 |

The chapter-1 "exact" is exact: an `Exponential(α)` prior is `Gamma(1,
α)`, conjugate to the Poisson, so both rates integrate out and τ's
posterior is a finite sum — computed in the test, independently of the
sampler. The conjugate rows are closed forms. And PyMC itself, provisioned
by `uv` through okay-py, samples the same model as a Live test.

## API reference

| | |
|---|---|
| `Normal, Exponential, Gamma(shape, rate), Beta, Uniform, Poisson, Bernoulli, Binomial, DiscreteUniform(lo, hi)` | `logPdf`, `sample`, a symmetric `propose`, `coerce` (a trace value back as its own type) |
| `sample(name, d)`, `observe(d, x)`, `observeAll(xs)(d, value)`, `factor(logW)` | the `Model` effect |
| `prior(p, rng)`, `weighted(p, n, rng)`, `metropolis(p, samples, burn, thin, chains, seed)` | handlers: a forward run, likelihood weighting, lightweight MH |
| `Posterior`: `draws`, `site(name)`, `rhat(name)`, `acceptance` | the posterior, typed |
| `Summary.mean / sd / quantile / hdi / ess / rhat` | reading it |

## Literature

- Davidson-Pilon, *Probabilistic Programming and Bayesian Methods for
  Hackers* (2015; MIT) — the models, the data, the answers this module is
  measured against.
- Wingate, Stuhlmüller, Goodman, *Lightweight Implementations of
  Probabilistic Programming Languages via Transformational Compilation*
  (AISTATS 2011) — single-site MH over named traces, with the
  trans-dimensional correction.
- Kiselyov, Shan, *Embedded Probabilistic Programming* (DSL 2009) — a
  model as a program, inference as a handler: the core's `Prob`.
- Marsaglia, Tsang, *A Simple Method for Generating Gamma Variables* (ACM
  TOMS 2000); Hörmann, *The Transformed Rejection Method for Generating
  Poisson Random Variables* (1993) — the samplers.
- Gelman et al., *Bayesian Data Analysis*, 3rd ed., §11.4–11.5 — split
  R-hat and the effective sample size.
