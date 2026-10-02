# okay-bayes — Bayesian inference as effects, without Python

Status: stage 1 landed (2026-10-02); stage 2a (ch.2, adaptive Metropolis) 2b (SMC), 2c (ch.3), 2d (ch.6), 3a (NUTS) and 3b (AD) 2026-10-02; specification 2026-10-02 (operator ask: "Bayesian Methods for
Hackers ... make okay-bayes so we can do without Python; is it very
hard?"). Builds on the core's `Prob` effect (specs/prob-effect-hansei.md:
discrete `dist`, boolean `observe`, exact enumeration, rejection).

## 1. Why

A Bayesian model is a program with two extra operations — draw a named
random quantity, weigh the run by the likelihood of what was observed —
and INFERENCE is a handler over it (Kiselyov & Shan's Hansei; Wingate,
Stuhlmüller & Goodman's lightweight MH; the effect-handler PPLs). The core
already has that shape for finite discrete choices. What *Bayesian
Methods for Hackers* (Davidson-Pilon; PyMC) needs beyond it: continuous
and count distributions with densities, `observe` that WEIGHS rather than
filters, a sampler that works in continuous space (Metropolis–Hastings,
later HMC/NUTS), and the summaries a posterior is read through.

## 2. Interface (stage 1)

```scala
package okay.bayes

trait Distribution[A]:                        // pure: density, draw, a symmetric proposal
  def logPdf(a: A): Double
  def sample(rng: Random): A
  def propose(a: A, scale: Double, rng: Random): A
  def coerce(v: Any): Option[A]               // a trace value back as A, by the distribution's own knowledge
Normal(mu, sigma); Exponential(rate); Gamma(shape, rate); Beta(a, b); Uniform(lo, hi)
Poisson(rate); Bernoulli(p); Binomial(n, p); DiscreteUniform(lo, hi)   // hi inclusive, as PyMC

enum Model[+A] derives Effect:
  case Sample[A](name: String, dist: Distribution[A]) extends Model[A]
  case Factor(logWeight: Double) extends Model[Unit]

object Bayes:
  def sample[A](name: String, d: Distribution[A]): A ! Model
  def observe[A](d: Distribution[A], x: A): Unit ! Model          // = factor(d.logPdf(x))
  def factor(logWeight: Double): Unit ! Model
  def prior[A](p: A ! Model, rng): Run[A]                          // one forward draw: value, trace, log prior, log likelihood
  def weighted[A](p: => A ! Model, n, rng): Vector[(A, Double)]    // likelihood weighting
  def metropolis[A](p: A ! Model, samples, burn, thin, chains, seed): Posterior[A]   // single-site
  def adaptive[A](p: A ! Model, samples, burn, thin, chains, seed): Posterior[A]     // + joint moves, learnt covariance
  def smc[A](p: A ! Model, particles, seed): Particles[A]          // weighted particles + log evidence
  def observeEach[X, A](xs)(d, value): Unit ! Model               // one factor per observation (observeAll: one in all)

final case class Posterior[A](chains: Vector[Chain[A]]):   // Chain(draws: Vector[A], sites: Vector[Map[String, Double]], acceptance)
  def draws: Vector[A]; def site(name: String): Vector[Double]; def rhat(name: String): Double
object Summary: mean, sd, quantile, hdi(xs, mass), ess(xs), rhat(chains)
```

## 3. Behavior

Stage 1 — distributions, the effect, MH, summaries:
- [x] every distribution's `logPdf` against closed forms and its sampler
      against its mean and variance (within Monte Carlo error)
- [x] conjugate posteriors as exact oracles: Beta–Bernoulli, Gamma–Poisson,
      Normal–Normal (known σ) — MH's posterior mean and sd within error of
      the closed form
- [x] *Bayesian Methods for Hackers* ch.1, the texting model, on the book's
      `txtdata.csv`: τ concentrated at 44–45, λ1 ≈ 18, λ2 ≈ 23 — the book's
      published posterior
- [x] the posterior of the program's OWN value is typed (`Vector[A]`), no
      cast: a trace value comes back through its distribution's `coerce`
- [x] lightweight MH handles a model whose STRUCTURE depends on a draw
      (sites that appear in one run and not another), with the
      fresh/stale correction
- [x] summaries: HDI, quantiles, ESS (Geyer's initial positive sequence),
      split R-hat over chains; a correlated chain reads a lower ESS
- [x] PyMC as an oracle (Live: okay-py, a `uv`-provisioned PyMC): the texting
      model's posterior means agree within MC error

Stage 2a — *Bayesian Methods for Hackers* ch.2:
- [x] the A/B test (the book's simulated 1500 + 750 visitors): pA's
      posterior and P(pA > pB) against the exact Beta posteriors
      (numerical integration)
- [x] the Challenger O-ring logistic regression on the book's data, against
      an exact 2400 x 2400 grid posterior: E[α], E[β], sd(β), p(31°F)
- [x] adaptive Metropolis (Haario, Saksman & Tamminen 2001): after the
      first half of burn-in the continuous sites move JOINTLY with the
      learnt covariance x 2.38²/d, the scale tuned toward acceptance 0.23;
      discrete sites keep single-site moves. On Challenger it raises ESS(β)
      from 151 to 6879 in the same budget
- [x] PyMC as an oracle for Challenger (Live) — see Results for why the
      book's own parametrisation cannot be that oracle

Stage 2b — SMC (Del Moral, Doucet & Jasra 2006; the probabilistic-programming
form of Wood, van de Meent & Mansinghka 2014): `smc(p, particles, seed)`.
Each particle is the program SUSPENDED at its next `Factor`; the samples
before it are drawn from their priors on the way. At every factor the
particles are reweighed, and when the effective sample size falls under
half they are resampled (systematic): a particle chosen twice resumes ONE
continuation twice — multi-shot, sound because a program value is
immutable. The result is the weighted particles and an unbiased estimate of
the log EVIDENCE, log p(data) — what MCMC does not give and what a Bayes
factor needs.
- [x] a state-space model (a Gaussian random walk observed with noise),
      sampled step by step: the filtering mean and the log evidence against
      the Kalman filter's exact answers
- [x] a static model observed one point at a time (`observeEach`):
      Beta–Bernoulli posterior mean and log evidence against the closed
      forms
- [x] the evidence COMPARES models: two hypotheses on one data set, the
      log Bayes factor against the closed form
- [x] a resampled particle shares its continuation with its copies, and
      they diverge afterwards (each draws its own future)

Stage 2c — *Bayesian Methods for Hackers* ch.3, a mixture and its
convergence:
- [x] `Mixture(components)`: a weighted mixture of distributions, its log
      density the log-sum-exp of the components' (PyMC's `Mixture`); the
      assignments are summed out, not sampled
- [x] the book's two-cluster model on its `mixture_data.csv` (p, two
      centres, two sds): four `adaptive` chains agree (split R-hat < 1.01)
      and agree with an INDEPENDENT oracle — importance sampling from a
      multivariate Student-t, whose estimate is unbiased whatever its
      proposal and whose error is its own ESS
- [x] the book's per-point question, P(x belongs to cluster 1), as a
      posterior expectation over the draws
- [x] the book's convergence lesson, measured: chains kept from their
      first draw (each starts at a prior draw, far from the others) read
      a split R-hat well above 1; the same chains after burn-in, under 1.01

Stage 2d — *Bayesian Methods for Hackers* ch.6, Thompson sampling
(Thompson 1933; Agrawal & Goyal 2012): `Bandit` — Bernoulli arms, each a
Beta posterior; `choose` draws once from every arm's posterior and pulls the
largest, `observe(arm, won)` is the conjugate update. Immutable: a bandit is
a value, a run a fold over pulls.
- [x] Thompson pulls each arm with the probability that it is the best —
      against P(arm is best) by numerical integration over the Betas
- [x] regret: on the book's arms (0.85, 0.60, 0.75) the mean regret over
      many runs grows like log T and stays under three times the
      Lai–Robbins rate Σ Δᵢ / KL(pᵢ, p*) · log T; uniform random play
      grows linearly
- [x] the posterior concentrates on the best arm: after T pulls its share of
      pulls tends to 1 Stage 3 — HMC/NUTS (operator, 2026-10-02: "Both: AD optional" — ONE
sampler, the gradient from a source):
- 3a. `Target` (a dimension, log density and gradient on ℝᵈ) and
  `Nuts.sample(target, ...)`: the No-U-Turn sampler (Hoffman & Gelman 2014,
  Algorithm 6: slice NUTS with dual-averaging step size), a diagonal mass
  matrix learnt in warmup. Each distribution declares its `Support` (ℝ,
  positive, an interval, discrete) and a model's sites move in
  UNCONSTRAINED space (log for positive, scaled logit for an interval, the
  Jacobian added), so no step leaves the support. `Bayes.nuts(model, ...)`
  builds the target from an ordinary `Double` model, the gradient by
  central finite differences (2d re-runs of the program per gradient);
  a discrete site, or a support that moves with another site, is refused
  by name.
  - [x] conjugates (Normal–Normal on ℝ, Gamma–Poisson through the log,
        Beta–Bernoulli through the logit) against their closed forms
  - [x] the ch.3 mixture against the importance-sampling oracle
  - [x] Challenger against the grid, divergences counted and reported
  - [x] a discrete site is refused by name
- 3b. Automatic differentiation (reverse mode; Griewank & Walther,
  *Evaluating Derivatives*, 2008): `Real`, a number recorded on a tape —
  arithmetic, exp, log, log1p, sqrt, pow, lgamma (digamma as its
  derivative), log-sum-exp — and a backward sweep giving every input's
  adjoint in one pass. A model written for it is a program over the `Grad`
  effect: `param(name, prior)` answers a `Real` (moving unconstrained
  through the prior's `Support`, the Jacobian on the tape), `observe` and
  `score` add differentiable log weights. `Smooth.target(p)` is a `Target`
  whose gradient costs ONE run of the program plus the sweep, against
  finite differences' 2d + 1; `Smooth.nuts(p, ...)` is `Nuts.sample` on it.
  - [x] every operation's derivative against a central difference
  - [x] the same model written both ways (Challenger) has the same log
        density at every point, and the AD gradient equals the finite one
        to its truncation error
  - [x] conjugates through the three transforms, via `Smooth.nuts`
  - [x] the cost: at d = 100 a gradient by AD against one by differences,
        measured (one run against 201) Stage 4 — the sampler as a facade (specs/own-or-standard.md). What
PyMC or Stan cannot do is read a Scala program; what they CAN do is sample
a log density they call back into. So the facade sits on `Target`, the
format both sides share: `Sampler` (cross) with `run(target, samples,
burn, seed, delta)`, OURS the default given (`Sampler.Okay`, `Nuts.sample`),
and `PyMC` (JVM) behind `import okay.bayes.PyMC.given` over an OPTIONAL
okay-py: PyMC's own NUTS on a `pm.Potential` whose log density and
gradient are a black-box pytensor Op calling back into okay
(`okay.call("logp")`, `okay.call("grad")` — PyMC's documented black-box
likelihood). `Bayes.nuts` and `Smooth.nuts` take the `Sampler` in scope,
so a caller's code changes by an import only. `Samplers.byName("okay" |
"pymc")` for a config value; `PyMC.missing` names okay-py when it is
absent. Stan needs its own model language and a C++ toolchain: no road from
a `Target` short of a Stan plugin, recorded, not built.
- [ ] the default is ours: `nuts` with no import is `Sampler.Okay`
- [ ] PyMC behind the import samples OUR target and agrees with the closed
      form (a conjugate) and with the grid (Challenger, over `Grad`) —
      the two samplers proven on one target, the facade's "reads the
      other's output"
- [ ] `Samplers.byName`: the two names, and a third refused naming them

## 4. Decisions

1. **A new module, not the core's `Prob`.** `Prob` is discrete and exact
   by design (multi-shot enumeration); densities, continuous space and MCMC
   are a library over the core, not the core.
2. **Named sites.** MH must know WHICH draw it changes between runs; PyMC
   names its variables for the same reason. The name is the address.
3. **No cast from the trace.** A trace holds values of many types; each
   distribution takes its own value back (`coerce`) by matching what it
   produces.
4. **Proposals are symmetric** (a random walk for continuous and integer
   sites, a flip for Bernoulli), tuned during burn-in toward an acceptance
   of about 0.44 per site, as PyMC's Metropolis tunes.

5. **Adaptive Metropolis as a second sampler, not a replacement.**
   `metropolis` stays single-site (it is what handles a draw that changes
   the model's structure, and its numbers are stage 1's); `adaptive` adds
   joint moves for the continuous sites of a fixed-structure model, where
   strongly correlated parameters (α and β on raw temperature, ρ ≈ -0.99)
   leave a single-site walk nearly still. Joint proposals are Gaussian with
   the Cholesky factor of the burn-in covariance, so they stay symmetric.

## 5. Results

Stage 1 (2026-10-02). Every number below is from the suites (TestBayes,
TestHackersCh1, TestHackersPyMC), MH at 20–30k sweeps after 2–5k burn-in:

| | exact | okay-bayes | PyMC 6.3.2 |
|---|---|---|---|
| ch.1 E[λ1] | 17.758 | 17.747 | 17.757 |
| ch.1 E[λ2] | 22.689 | 22.724 | 22.710 |
| ch.1 P(τ = 44), P(τ = 45) | 0.365, 0.486 | 0.368, 0.489 | 0.376, 0.473 |
| Beta–Bernoulli mean ± sd | 0.6765 ± 0.0791 | 0.6726 ± 0.0786 | |
| Gamma–Poisson | 3.6364 ± 0.5750 | 3.6367 ± 0.5821 | |
| Normal–Normal | 1.5278 ± 0.2827 | 1.5293 ± 0.2813 | |
| structure by a draw, P(coin) | 0.4295 | 0.4316 | |

The ch.1 "exact" is exact: Exponential(α) is Gamma(1, α), conjugate to the
Poisson, so the rates integrate out and τ's posterior is a finite sum,
computed in the test apart from the sampler. R-hat(λ1) 1.0002 over two
chains; tuned acceptance 0.28–0.33. Found on the way: pytensor (PyMC's
backend) passes `-ld64` to the linker, which a current macOS clang reads as
"library d64" — the oracle runs pytensor's pure-Python backend
(`PYTENSOR_FLAGS=cxx=`); okay-bayes needs no compiler at all.

Stage 2a (2026-10-02), TestHackersCh2 and TestHackersPyMC:

| | exact | okay-bayes | PyMC 6.3.2 |
|---|---|---|---|
| A/B E[pA] | 0.05393 | 0.05388 | |
| A/B P(pA > pB) | 0.8217 | 0.8220 | |
| Challenger E[α] (grid) | -17.511 | -17.404 (adaptive) | -17.555 |
| Challenger E[β] | 0.2693 | 0.2677 (adaptive), 0.2539 (single-site) | 0.2701 |
| Challenger sd(β) | 0.1163 | 0.1150 | |
| Challenger p(31°F) | 0.9874 | 0.9869 | 0.9874 |
| ESS(β) of 120 000 draws | | 6879 adaptive, 151 single-site | |

The grid's edge carries < 1e-6 of the mass, so it is exact to well
inside the tolerances. The single-site chain's ESS of 151 is the book's
own experience (it burns 100 000 and still sees autocorrelation); joint
acceptance after tuning 0.39.

PyMC's NUTS on the book's model AS WRITTEN is wrong here: every one of
5000 draws divergent, answer α -8.27, β -0.26 (the wrong sign). Its model
is right — PyMC's own logp is -19.1 at the grid's mean against -439.5 at
its answer, and its gradient matches a finite difference to 1e-6. The
geometry is what fails: on raw temperature α and β are almost collinear.
The same posterior through a linear change of variables (standardised
temperature; constant Jacobian; the book's priors kept as a Potential)
samples with no divergence and agrees with the grid, and that is the
oracle the Live test runs. okay-bayes needs no reparametrisation: the
adaptive sampler learns the correlation itself.

Stage 2b (2026-10-02), TestSmc — the same on JVM, Scala.js and Native:

| | exact | okay-bayes SMC (4000 particles) |
|---|---|---|
| random walk, 50 steps: E[x49 given y] | 1.9735 (Kalman) | 1.9735 |
| random walk: log p(y) | -101.616 (Kalman) | -101.706 (27 resamplings) |
| Beta(2, 2)–Bernoulli, 40 flips: E[p] | 0.6364 | 0.6370 |
| Beta–Bernoulli: log p(y) | -27.2845 | -27.2978 |
| log Bayes factor, Uniform p against a fair coin | 0.1446 | 0.1823 |

A model with no draw is weighed exactly (its evidence is the likelihood
to 1e-9). Resampling after a factor of -50x² leaves 144 values of x held
by several particles, up to 11 copies of one, and every copy drew its own
y: a shared continuation, separate futures. No rejuvenation step yet: for a
static parameter the particles only ever hold values drawn from the prior,
so the posterior is as rich as 4000 prior draws can make it (resample-move,
Chopin 2002, is the next step when a model needs it).

Stage 2c (2026-10-02), TestHackersCh3 — four `adaptive` chains, 25 000
draws each after 10 000 burn-in; the oracle 200 000 importance draws (ESS
29 564):

| ch.3 | adaptive (± posterior sd) | importance sampling |
|---|---|---|
| p | 0.376 ± 0.049 | 0.375 |
| centre 0 | 120.19 ± 5.52 | 120.16 |
| centre 1 | 199.60 ± 2.58 | 199.53 |
| sd 0 | 30.19 ± 4.01 | 30.16 |
| sd 1 | 22.84 ± 1.89 | 22.88 |
| P(first cluster at x = 150, 175, 220) | 0.722, 0.149, 0.007 | 0.719, 0.147, 0.007 |

Split R-hat over the four chains 1.0005–1.0020; the same sampler's first
100 draws with no burn-in, from prior starts: 1.70. The book samples each
point's assignment as its own Categorical site; summed out by `Mixture`
the model has five sites instead of 305, and the assignment posterior is
the expectation above, not a sample.

Stage 2d (2026-10-02), TestBandit — JVM, Scala.js and Native:

| ch.6 | exact | okay-bayes |
|---|---|---|
| P(best) for Beta(4, 8), Beta(6, 8), Beta(3, 8) | 0.2546, 0.6098, 0.1357 | Thompson's choice frequency 0.2541, 0.6103, 0.1355 (200 000 draws) |
| regret on (0.85, 0.60, 0.75), mean of 30 runs, T = 1000 / 10 000 | Lai–Robbins rate 29.8 / 39.8 | 14.3 / 20.9 |
| uniform random play, T = 10 000 | 1167 | |
| best arm's share of pulls, T = 10 000 | | 0.983 |

The measured regret is BELOW the Lai–Robbins figure, which is no
contradiction: the bound is asymptotic, a statement about the coefficient
of log T as T grows, and the 0.75 arm (KL 0.034 from the best) is far from
that regime at 10 000 pulls. What the test holds is the shape: ten times
the pulls cost 1.46 times the regret, not ten.

Stage 3a (2026-10-02), TestNuts (JVM, Scala.js, Native) and
TestHackersNuts (JVM), finite-difference gradients throughout:

| | exact | NUTS (ESS of draws) |
|---|---|---|
| Normal–Normal | 1.4286 ± 0.4364 | 1.4363 ± 0.4417 (1178 / 3000) |
| Gamma–Poisson, through the log | 3.3750 ± 0.6495 | 3.3751 ± 0.6427 (1458 / 3000) |
| Beta–Bernoulli, through the logit | 0.6765 ± 0.0791 | 0.6740 ± 0.0812 (1171 / 3000) |
| Challenger E[β], sd(β), p31 (grid) | 0.2693, 0.1163, 0.9874 | 0.2697, 0.1168, 0.9870 (512 / 8000) |
| ch.3 centre 1 (importance sampling) | 199.535 | 199.593 (706 / 4000) |

Zero divergences on every model, Challenger included — on raw
temperature, the parametrisation PyMC 6.3.2's NUTS diverged on at every
draw (stage 2a). Per draw NUTS buys far more than single-site MH (ESS(β)
512 of 8000 against 151 of 120 000) and less than `adaptive` per second on
a two-parameter model; its case is dimension, which finite differences
tax (2d + 1 program runs per gradient) and 3b's AD removes.

Stage 3b (2026-10-02), TestSmooth (JVM, Scala.js, Native) and
TestHackersNuts. Every operation's tape derivative equals a central
difference to 1e-5 relative; a logistic regression written over `Grad` has
the log density of the same model written with `Distribution` to 1e-9 at
20 random points, and its gradient is the finite one. Challenger over
`Grad`: E[β] 0.2727 (grid 0.2693), sd(β) 0.1181 (0.1163), p31 0.9878
(0.9874), zero divergences. Conjugates: Normal–Normal 1.4363 ± 0.4417,
Gamma–Poisson 3.3892 ± 0.6227, Beta–Bernoulli 0.6740 ± 0.0812, each within
error of the closed form.

The cost, one gradient at d = 100 (best of 7):

| | AD | central differences | ratio |
|---|---|---|---|
| JVM | 0.13 ms | 4.89 ms | 37x |
| Scala.js | 0.22 ms | 22.35 ms | 102x |
| Native | 0.59 ms | 111.95 ms | 191x |

Found on the way: the digamma series started at x = 6 was good to 9e-12
only; started at 10 it is good to the double.

## 6. Open questions

- Vectorised sites (`sample("x", Normal(0, 1), size = n)`)? Stage 1 names
  each scalar; a vector site when a model's runtime asks.
