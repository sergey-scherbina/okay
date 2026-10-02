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

**Correlated parameters: `adaptive`.** Chapter 2's Challenger model — the
probability of O-ring damage as a logistic function of launch temperature —
is two parameters that are almost the same parameter: on raw temperature α
and β correlate at about -0.99, and a walk that moves one at a time barely
moves. `adaptive` is Metropolis with Haario's adaptation (2001): after half
of burn-in it learns the continuous sites' covariance and from then moves
them JOINTLY, its scale tuned toward the textbook acceptance; a discrete
site still moves alone.

```scala
val challenger = for
beta <- sample("beta", Normal(0, sd))
alpha <- sample("alpha", Normal(0, sd))
_ <- observeAll(flights)(f => Bernoulli(p(f._1, alpha, beta)), _._2)
yield (alpha, beta, p(31, alpha, beta))
val post = adaptive(challenger, samples = 60000, burn = 20000, chains = 2)
```

Same budget, effective sample size of β: 151 with `metropolis`, 6879 with
`adaptive`. PyMC's NUTS, given the book's model exactly as written, diverges
on every draw here and answers β with the wrong sign; it agrees only after
temperature is standardised by hand — yet handed okay's density for the
same model (`import okay.bayes.PyMC.given`, below) it samples cleanly, so
the fault is in PyMC's model graph, not the geometry. `adaptive` needs no
rewrite.

**Mixtures: `Mixture`.** Chapter 3's 300 points come from two clusters, and
which point belongs to which is unknown. `Mixture(Vector(w1 -> d1, w2 ->
d2))` is a distribution whose density sums the components out (log-sum-exp),
so the model samples five parameters, not 300 assignments:

```scala
def clusters(p: Double, c0: Double, c1: Double, s0: Double, s1: Double): Mixture[Double] =
Mixture(Vector(p -> Normal(c0, s0), (1 - p) -> Normal(c1, s1)))
_ <- observeAll(data)(_ => clusters(p, c0, c1, s0, s1), identity)
lazy val post = adaptive(mixture, samples = 25000, burn = 10000, chains = 4)
```

The posterior (centres 120.2 and 199.6, sds 30.2 and 22.8, p 0.376) agrees
with an independent importance sampler, and a point's chance of belonging
to the first cluster is an expectation over the draws: 0.149 at x = 175.
Convergence reads the same way as in the book: split R-hat over four chains
is 1.70 over their first 100 draws and 1.002 after burn-in.

**Hamiltonian Monte Carlo: `nuts`.** The No-U-Turn sampler (Hoffman &
Gelman 2014) follows the gradient of the log density along trajectories
that stop when they turn back, with its step size and a diagonal mass
matrix tuned in warmup — PyMC's and Stan's default. Each distribution
declares its `Support`, and sites move in unconstrained space (log for a
positive parameter, a scaled logit for an interval), so no step leaves the
support. On an ordinary model the gradient is by finite differences:

```scala
val post = nuts(Ch2.challenger, samples = 4000, burn = 2000, chains = 2)
```

Challenger's E[β] 0.2697 against the grid's 0.2693, zero divergences, on
the raw temperatures where PyMC's NUTS diverges on every draw. A discrete
site is refused by name; sample it with `metropolis`. Underneath is
`Nuts.sample(target, ...)` over any `Target` — a dimension, a log density
and its gradient on ℝᵈ — so a gradient from elsewhere plugs in unchanged.

**Exact gradients: `Smooth`.** Finite differences run the program 2d + 1
times per gradient. A model written over the `Grad` effect instead draws
`param`s that are `Real`s — numbers recorded on a tape — and one backward
sweep gives the whole gradient: reverse-mode automatic differentiation, the
same as Stan's and PyMC's. The model reads as before, with `Smooth`'s
densities:

```scala
beta <- param("beta", Smooth.Normal(0, sd))
alpha <- param("alpha", Smooth.Normal(0, sd))
_ <- observeAll(Ch2.flights)(f => Smooth.BernoulliLogit(-(beta * f._1 + alpha)), _._2)
val post = Smooth.nuts(challenger, samples = 4000, burn = 2000, chains = 2)
```

`Smooth.target(p)` is the same `Target` `nuts` uses, so the sampler is the
same one. At 100 parameters one gradient costs 0.13 ms against 4.9 ms by
differences on the JVM, and 0.59 ms against 112 ms on Native.

**The sampler is a choice.** `nuts` runs on the `Sampler` in scope: ours
by default, PyMC's own NUTS after one import (JVM, with okay-py on the
classpath). PyMC cannot read a Scala program, so it is handed the
program's `Target` — a black-box log density and gradient it calls back
into — and the call sites stay as they are:

```scala
import PyMC.given
val post = Smooth.nuts(bb, samples = 2000, burn = 1000)
```

That is how ours is checked against the reference on one and the same
density. `Samplers.byName("okay" | "pymc")` picks one from a config value.

**Evidence: `smc`.** Sequential Monte Carlo runs many copies of the program
side by side, each SUSPENDED at its next `observe`; at every observation the
copies are reweighed and, when a few carry most of the weight, resampled —
a copy chosen twice resumes ONE continuation twice, and each copy draws its
own future. Besides the posterior it answers `logEvidence`, log p(data),
which MCMC cannot: the number that compares two models. Observations go in
one at a time with `observeEach` (to MCMC it is the same as `observeAll`).
A random walk tracked through noisy readings, whose exact answer is the
Kalman filter's:

```scala
def track(ys: Vector[Double]): Double ! Model =
def step(t: Int, x: Double): Double ! Model =
if t == ys.length then okay.pure[Model, Double](x)
else sample(s"x$t", Normal(x, q)).flatMap(x1 => observe(Normal(x1, r), ys(t)).flatMap(_ => step(t + 1, x1)))
step(0, 0.0)
val ps = smc(track(ys), particles = 4000)
```

`ps.expect(identity)` is E[x(49) | y] (1.9735, the Kalman filter's 1.9735)
and `ps.logEvidence` is log p(y) (-101.706 against -101.616). Two models'
evidences subtract to a log Bayes factor: a coin with p ~ Uniform against a
fair one, 0.182 against the closed form's 0.145.

**Bandits: Thompson sampling.** Chapter 6 asks which of several slot
machines pays best while playing them. `Bandit` keeps a Beta posterior per
arm; `choose` draws once from each and pulls the largest, so an arm is
pulled exactly as often as it is likely to be the best — exploration that
fades as the evidence comes in. A bandit is a value, and a run is a fold:

```scala
val b1 = b0.observe(1, true).observe(1, false).observe(1, true)
assertEquals(b1.posterior(1), Distribution.Beta(3, 2))
val plays = Vector.tabulate(runs)(r => Bandit.play(Bandit(3), 10000, Random(r))(a => world.nextDouble() < p(a)))
```

On the book's arms (0.85, 0.60, 0.75) the mean regret is 14.3 after 1000
pulls and 20.9 after 10 000 — logarithmic growth — where playing at random
costs 1167; the best arm takes 98.3% of the pulls.

**Ranking: `Rank`.** Chapter 4 sorts comments by votes. One upvote of one
has a perfect ratio and almost no evidence; `Rank.sort` orders by a low
quantile of each item's Beta posterior instead — how good it is AT LEAST,
plausibly — computed exactly (`Beta.cdf` / `Beta.quantile`, the
incomplete beta function):

```scala
val ranked = Rank.sort(items)(_._2).map(_._1)
```

999 of 1000 comes first and one of one last. The book's quick formula
(mean − 1.65 sd) is optimistic by 0.054 for a single vote and right to 3e-4
by 700.

**From a posterior to a decision: `Decision`.** Chapter 5's point is that
the posterior mean is the right answer only under squared loss.
`Decision.action(draws, lo, hi)(loss)` finds the decision with the least
expected loss over the draws — `Loss.squared` gives the mean,
`Loss.absolute` the median, `Loss.pinball(τ)` the τ-quantile, and the
book's Price-is-Right showdown, where overbidding loses everything, bids
far under the estimate:

```scala
val bids = Seq(30000.0, 60000.0, 90000.0, 120000.0, 150000.0).map(r => r -> Decision.action(draws, 5000, 40000)(showdown(r)))
```

At a risk of 30 000 the best bid is 14 549, at 150 000 it is 10 703, while
the posterior mean of the price is 19 876.

**A/B testing by revenue: `AbTest`.** Chapter 7 judges two page variants
by what a visitor is worth, not by whether they bought: each variant's
purchase tiers get a Dirichlet posterior, and the revenue per visitor is a
draw per Dirichlet draw.

```scala
val a = Variant(values, Vector(10, 46, 80, 864))
val (p1, diff) = AbTest.compare(a, b, 100000, Random(1))
```

`p1` is P(B's revenue per visitor beats A's), 0.981 here; `diff` is the
posterior of the lift (1.23 per visitor). `Variant.exact` gives the
posterior mean and sd in closed form.

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
| ch.2 A/B, P(pA > pB) | 0.8217 | 0.8220 |
| ch.2 Challenger, E[β] (exact grid) | 0.2693 | 0.2677 |
| ch.2 Challenger, sd(β) | 0.1163 | 0.1150 |
| ch.2 Challenger, p(damage at 31°F) | 0.9874 | 0.9869 |
| ch.3 mixture, centre 1 (importance sampling) | 199.53 | 199.60 |
| ch.3 mixture, P(first cluster at x = 175) | 0.147 | 0.149 |

The chapter-1 "exact" is exact: an `Exponential(α)` prior is `Gamma(1,
α)`, conjugate to the Poisson, so both rates integrate out and τ's
posterior is a finite sum — computed in the test, independently of the
sampler. The conjugate rows are closed forms, the A/B row an exact Beta
integral, and the Challenger rows a 2400 x 2400 grid over (α, β) whose edge
carries under a millionth of the mass. And PyMC itself, provisioned
by `uv` through okay-py, samples the same model as a Live test.

## API reference

| | |
|---|---|
| `Normal, Exponential, Gamma(shape, rate), Beta, Uniform, Poisson, Bernoulli, Binomial, DiscreteUniform(lo, hi), Mixture(components)` | `logPdf`, `sample`, a symmetric `propose`, `coerce` (a trace value back as its own type) |
| `sample(name, d)`, `observe(d, x)`, `observeAll(xs)(d, value)`, `factor(logW)` | the `Model` effect |
| `prior(p, rng)`, `weighted(p, n, rng)`, `metropolis(p, samples, burn, thin, chains, seed)` | handlers: a forward run, likelihood weighting, lightweight MH |
| `adaptive(p, samples, burn, thin, chains, seed)` | MH with joint moves under a learnt covariance (Haario 2001) |
| `Bandit(arms)`, `choose(rng)`, `observe(arm, won)`, `posterior(arm)`, `Bandit.play(b, pulls, rng)(pull)` | Thompson sampling over Bernoulli arms |
| `nuts(p, samples, burn, chains, seed, delta)` | the No-U-Turn sampler, unconstrained through each site's `Support`, finite-difference gradient |
| `Target`, `Target.finite(d)(f)`, `Nuts.sample(target, samples, burn, seed, delta, maxDepth)` | NUTS over any log density on ℝᵈ |
| `Real`, `Tape`, `Real.exp / log / log1p / sqrt / pow / softplus / sigmoid / lgamma / logSumExp` | reverse-mode AD |
| `Smooth.param(name, prior)`, `observe`, `observeAll`, `score`; `Smooth.Normal, HalfNormal, Exponential, Gamma, Beta, Uniform, Bernoulli, BernoulliLogit, Poisson, Mixture` | a model over `Grad`, differentiable densities |
| `Smooth.target(p)`, `Smooth.nuts(p, samples, burn, chains, seed, delta)` | its exact-gradient `Target`, and NUTS on it |
| `Sampler`, `Sampler.Okay` (default), `PyMC.given` (JVM, optional okay-py), `Samplers.byName` | which NUTS runs `nuts` |
| `Beta.cdf`, `Beta.quantile`, `Distribution.incompleteBeta`; `Rank.lowerBound / approxLowerBound / sort` | exact Beta tails; ranking by evidence |
| `Decision.action(draws, lo, hi)(loss)`, `Decision.expectedLoss`; `Loss.squared / absolute / pinball(τ)` | the Bayes action |
| `Dirichlet(alpha)` (`sample`, `logPdf`, `mean`, `covariance`); `AbTest.Variant`, `revenue`, `compare` | A/B testing by expected revenue |
| `smc(p, particles, seed)`, `observeEach(xs)(d, value)` | sequential Monte Carlo: `Particles` with `expect`, `mean(site)`, `ess`, `logEvidence` |
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
- Haario, Saksman, Tamminen, *An Adaptive Metropolis Algorithm*
  (Bernoulli 7(2), 2001) — the learnt covariance and the 2.38²/d scale
  (Gelman, Roberts, Gilks 1996).
- Dalal, Fowlkes, Hoadley, *Risk Analysis of the Space Shuttle:
  Pre-Challenger Prediction of Failure* (JASA 1989) — the O-ring data.
- Hoffman, Gelman, *The No-U-Turn Sampler: Adaptively Setting Path
  Lengths in Hamiltonian Monte Carlo* (JMLR 2014); Neal, *MCMC Using
  Hamiltonian Dynamics* (Handbook of MCMC, 2011); the Stan Reference
  Manual, "MCMC Sampling" — warmup windows and the regularised metric.
- Griewank, Walther, *Evaluating Derivatives* (SIAM, 2nd ed. 2008) —
  reverse mode and the tape; Carpenter et al., *The Stan Math Library:
  Reverse-Mode Automatic Differentiation in C++* (2015).
- Del Moral, Doucet, Jasra, *Sequential Monte Carlo Samplers* (JRSS B
  2006); Wood, van de Meent, Mansinghka, *A New Approach to Probabilistic
  Programming Inference* (AISTATS 2014) — SMC as the inference of a
  program suspended at its observations.
- Berger, *Statistical Decision Theory and Bayesian Analysis* (Springer
  1985) — loss, risk and the Bayes action.
- Thompson, *On the Likelihood that One Unknown Probability Exceeds
  Another* (Biometrika 1933); Agrawal, Goyal, *Analysis of Thompson Sampling
  for the Multi-armed Bandit Problem* (COLT 2012); Lai, Robbins,
  *Asymptotically Efficient Adaptive Allocation Rules* (Adv. Appl. Math.
  1985) — the bandit, its analysis, the bound.
- Kiselyov, Shan, *Embedded Probabilistic Programming* (DSL 2009) — a
  model as a program, inference as a handler: the core's `Prob`.
- Marsaglia, Tsang, *A Simple Method for Generating Gamma Variables* (ACM
  TOMS 2000); Hörmann, *The Transformed Rejection Method for Generating
  Poisson Random Variables* (1993) — the samplers.
- Gelman et al., *Bayesian Data Analysis*, 3rd ed., §11.4–11.5 — split
  R-hat and the effective sample size.
