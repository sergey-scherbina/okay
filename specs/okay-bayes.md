# okay-bayes — Bayesian inference as effects, without Python

Status: stage 1 landed (2026-10-02); stage 2a (ch.2, adaptive Metropolis) 2b (SMC), 2c (ch.3), 2d (ch.6), 3a (NUTS), 3b (AD) and 4 (the sampler facade), 5a (ch.4), 5b (ch.5), 5c (ch.7), 5d (Dark Worlds), §6 vectors, 6 (streams and Bulk), 7 (kernels, resample-move) 2026-10-02; specification 2026-10-02 (operator ask: "Bayesian Methods for
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
- [x] the default is ours: `nuts` with no import is `Sampler.Okay`
- [x] PyMC behind the import samples OUR target and agrees with the closed
      form (a conjugate) and with the grid (Challenger, over `Grad`) —
      the two samplers proven on one target, the facade's "reads the
      other's output"
- [x] `Samplers.byName`: the two names, and a third refused naming them

Stage 5 — the rest of the book, one lane per chapter:
- 5a. ch.4, the law of large numbers and RANKING: `Beta.cdf` and
  `Beta.quantile` exact (the regularized incomplete beta by Lentz's
  continued fraction, the quantile by safeguarded Newton), and `Rank` —
  items ordered by the lower bound of their Beta posterior, not by their
  raw ratio, the book's answer to "1 upvote of 1 beats 999 of 1000".
  - [x] `Beta.cdf` against closed forms (Beta(1, 1), Beta(a, 1) = xᵃ,
        Beta(2, 2)) and symmetry; `quantile` its inverse to 1e-10
  - [x] the book's approximate lower bound against the exact quantile:
        close for many votes, wrong by how much for few — measured
  - [x] the ranking: a small sample cannot outrank a large one with a
        slightly lower ratio, and ties in ratio order by evidence
  - [x] the law of large numbers as the book shows it: the spread of a
        mean of n draws falls as 1/√n
- 5b. ch.5, loss functions and the BAYES ACTION: `Decision.action(draws,
  lo, hi)(loss)` — the decision minimising the posterior expected loss,
  over the draws, by a grid then golden-section refinement; `Loss.squared`,
  `absolute`, `pinball(τ)`.
  - [x] the known answers: squared loss → the posterior mean, absolute →
        the median, pinball(τ) → the τ-quantile, of the same draws
  - [x] The Price is Right on the book's numbers: the posterior of the
        true price against its closed form (a linear-Gaussian model)
  - [x] the book's showdown loss: the best bid falls as the risk of
        overbidding grows, and stays under the posterior mean
- 5d. ch.5's data-heavy example, Kaggle's *Observing Dark Worlds*: a
  dark-matter halo of unknown position and mass shears the ellipticity of
  the galaxies around it tangentially, as m / max(r, 240); the book's
  single-halo model on its skies. The data is a SIMULATION, so the truth
  is known.
  - [x] Sky 3 (578 galaxies) by `adaptive` and by AD NUTS, against an exact
        3-D grid posterior over (x, y, mass)
  - [x] the ten single-halo training skies: how often the true halo lies
        inside the 95% posterior region, and the distance from posterior
        mean to truth against the posterior's own spread — measured
  Measured (TestDarkWorlds): Sky 3's grid posterior x 2324.1 ± 24.8, y
  1123.5 ± 42.2, mass 145.2 ± 11.2; `adaptive` 2324.7 / 1124.1 / 145.5, AD
  NUTS 2325.1 / 1123.5 / 144.6; the true halo (2315.8, 1082.0) 1.04
  posterior sds from the mean. The ten skies: the truth inside the 95%
  region in 10 of 10, median distance 39 (of a 4200-wide sky).
  FOUND: this posterior has local modes, and NUTS from a random start
  stays on the hill it lands on — Sky 3 at x 3229 with ESS 3, and 4 of the
  10 skies off by 2000 or more, while `adaptive` from a prior draw found
  the mode. Started at a coarse search's best point (`Smooth.nuts(...,
  init = ...)`, added here) it is right on every sky. The grid's coarse
  pass is the same search, so the start costs nothing new.
  REWRITTEN OVER BULK (darkworlds-bulk, operator: "why Vectors, not streams
  or better Bulk?"): a sky is read by `Bulk.csv`, mapped to galaxies and
  cached; both model forms observe it with `observeBulk`, written over any
  `Bulk[D]`. The grid oracle keeps its own Vector reading, and the Bulk
  model's density equals it in both forms at points across the sky. After
  the rewrite: Sky 3 by AD NUTS x 2323.9 (grid 2324.1); ten skies 10 of
  10, median distance 42. A stream is the wrong shape here: the
  likelihood is an unordered sum NUTS re-reads per gradient, which is what
  a Bulk aggregate is; the online filter is the stream example.
  AND THE ORACLE (darkworlds-oracle-bulk, operator: "sky(n) as a Bulk, not
  a Vector"): the grid oracle reads through a Bulk too, keeping its
  independence by its own parser — the lines split by hand, columns by
  position — against `Bulk.csv`'s rows by name in the model. Every sum over
  galaxies, the coarse search and the 129 600-point fine grid alike, is ONE
  `aggregate` whose accumulator holds a sum per point. Same numbers to the
  last digit (grid x 2324.1 ± 24.8, mass 145.2 ± 11.2).
- 5c. ch.7, A/B testing by EXPECTED REVENUE: a visitor buys one of
  several tiers or nothing; the tier probabilities have a Dirichlet
  posterior (flat prior + counts), and the revenue per visitor is Σ vᵢ pᵢ.
  `Dirichlet` (sample, log density, mean, covariance — a posterior helper,
  not yet a model site: that waits on vector sites, §6) and `AbTest`:
  posterior draws of each variant's revenue, P(A beats B), the lift.
  - [x] Dirichlet's moments against its closed forms
  - [x] each variant's revenue: posterior mean and sd against the exact
        Σ vᵢ E[pᵢ] and √(vᵀ Cov v)
  - [x] P(B beats A) stable between two independent runs to MC error, and
        the decision it supports printed beside the naive one

Stage 6 — okay-bayes over okay's streams and Bulk (operator, 2026-10-02:
"does our bayes work with our streams and Bulk?" — it did not):
- STREAMS. `Online.filter(start, step, particles, seed)`: a particle
  filter as a VALUE — particles of a state S, pushed one observation at a
  time through a `step(s, o): S ! Model` (the bootstrap filter of Gordon,
  Salmond & Smith 1993), reweighed, resampled under half ESS, the log
  evidence accumulated. `filter.stage` is a `Stage[O, Particles[S], …]`
  (`Stage.mapAccumulate`): a stream of observations in, a posterior per
  observation out.
  - [x] a Gaussian random walk observed through a stream: the filtering
        mean at EVERY step and the final log evidence against the Kalman
        filter
- BULK. `Bayes.observeBulk(rows)(logLik)` and
  `Smooth.observeBulk(rows, params)(f)` over any `Bulk[D]` (Chunks in one
  JVM, Spark, Flink): the log-likelihood is ONE `aggregate` per run of the
  model — a commutative `Aggregator` summing per-row log densities, and
  for AD per-row gradients (a small tape per row, the parameters' values
  as its inputs), entered on the model's own tape as one linear node with
  that value and gradient. NUTS pays one pass over the data per gradient.
  - [x] the same model over a Bulk and over a Vector: the same log density
        and the same gradient at random points
  - [x] Normal mean and sd from 20 000 rows by AD NUTS over `Bulk[Chunks]`,
        against the large-n posterior (wide priors: μ ~ N(ȳ, s²/n), σ ~
        N(s, s²/2n)); and `Bayes.observeBulk` by `adaptive` on a small bulk

Stage 7 — samplers as COMBINATORS (operator, 2026-10-02, after the
typeclass survey: "kernels and resample-move first").
- 7a. KERNELS. A Markov kernel is a step on a model's trace that leaves
  the posterior invariant; kernels that do are closed under SEQUENCE and
  MIXTURE (Tierney 1994), so a sampler is built, not chosen. `Kernel[A]`
  over a model `A ! Model`, steps on `Trace[A]` (the value, the sites,
  the log joint):
  - `Kernel.site(name)` — single-site random-walk MH on one site
    (Wingate's, the correction for a structure change kept);
    `Kernel.sites(names*)` and `Kernel.everySite` — a sweep;
  - `Kernel.nuts(names*)` — a NUTS transition on those continuous sites
    with every other site HELD (Metropolis-within-Gibbs with a NUTS
    block), unconstrained through each site's `Support`, gradient by
    finite differences;
  - `k1 >>> k2` — one then the other; `Kernel.mixture(w1 -> k1, ...)` —
    one of them, chosen by weight; `k.times(n)`.
  Step sizes and proposal scales are tuned during burn-in only (a kernel
  tuned while sampling is not invariant), as `metropolis` already does.
  `Bayes.sample(p, kernel, samples, burn, chains, seed): Posterior[A]`.
  - [x] invariance, the defining property, tested directly: start from
        EXACT posterior draws (a conjugate), apply each kernel and each
        combination once, the moments unchanged
  - [x] a mixed model by a composed kernel: the book's ch.1 texting model
        (τ discrete, λ1 and λ2 continuous) by `nuts("lambda_1",
        "lambda_2") >>> site("tau")` against the exact posterior
  - [x] a mixture kernel on Challenger (single-site and joint NUTS by
        weight) against the grid
  Measured (TestKernels on JVM, Scala.js and Native; TestKernelsHackers):
  from 20 000 exact posterior draws of a Normal mean and a Poisson rate,
  one step of each kernel — sites, everySite, nuts(m, l), nuts(m) >>>
  site(l), a mixture, site(l).times(3) — moves 48–96% of them and leaves
  both means within 4 standard errors and both sds within 3% (nuts(m, l):
  m 1.4285 against 1.4286). The texting model by nuts(λ1, λ2) >>>
  site(τ): E[λ1] 17.752 (exact 17.758), E[λ2] 22.705 (22.689), P(τ ∈ {44,
  45}) 0.864 (0.851), ESS(λ1) 8000 of 8000. Challenger by mixture(
  everySite, nuts(α, β)): E[β] 0.2663 (grid 0.2693), p31 0.9908 (0.9874).
  The NUTS transition was taken out of `Nuts.sample` into a `Dynamics`
  both use, every random draw in the same order: `Nuts.sample`'s numbers
  are unchanged to the last digit.
- 7b. RESAMPLE-MOVE SMC (Gilks & Berzuini 2001; Chopin 2002): after each
  resampling, every particle is moved by a kernel that targets the
  posterior GIVEN THE OBSERVATIONS SO FAR — the model re-run with its
  sites held, stopped at the same factor — so a static parameter's
  particles are no longer only prior draws. `smc(p, particles, seed,
  move = kernel)`.
  - [x] Beta–Bernoulli observed one at a time: the number of distinct
        particle values, without and with the move, measured; the mean,
        sd and log evidence against the closed form with the move
  Measured (TestResampleMove, JVM/Scala.js/Native): 400 flips, 1000
  particles — 259 distinct values of p without the move, 968 with
  `Kernel.site("p").times(3)`; with it E[p] 0.6638, sd 0.0242 (exact
  0.6634 ± 0.0235), log p(y) −257.6169 (exact −257.6057). A NUTS block
  as the move: E[p] 0.6642, 143 distinct of 500 (frozen at its first
  step size, untuned). The move costs a re-run of the prefix, O(factors
  so far) per particle, which is why it runs only after a resampling.
  At 60 flips the weights resample once and 544 of 1000 survive anyway:
  degeneracy needs a posterior that narrows a lot, and the first cut of
  this test asked for it where there was none.
  SMC itself was rewritten over one interpreter (`runFactors`, which
  also re-runs a prefix) with every random draw in the old order: the
  Kalman, Beta–Bernoulli and Bayes-factor numbers are unchanged.

Stage 8 — models whose structure is KNOWN BEFORE THEY RUN (operator,
2026-10-03, after the typeclass survey). A model `A ! Model` already has
Functor, Applicative, Selective and Monad — the core's `Monad[Free[F, *]]`
— but derived from `flatMap`, so its sites show only by running it. The
core's `Static[F, A]` is the FREE Selective (Capriotti & Kaposi 2014;
Mokhov et al. 2019): `leaves` lists every operation it MAY perform. The
price is that an operation's argument is fixed when the program is built:
a prior cannot depend on another draw, and an observation's likelihood,
which depends on the draws, cannot be an operation. So a DECLARED model is
two parts — the PARAMETERS, a `Static[Model, P]` of independent draws
(combined applicatively, branched by `select`/`ifS`), and the LIKELIHOOD,
a pure `P => Double`. Most of the book fits (Challenger, the mixture, Dark
Worlds); a hierarchical model fits NON-CENTERED, θ = μ + τ·z, z ~ N(0, 1).
`Declared.sample`, `both`, `all`, `sampleN`; `Declared.sites(params)` —
name, distribution, and whether under a branch — with nothing run;
`Declared.model(params)(logLik)`, an ordinary `P ! Model` every sampler
takes; `Declared.nuts(...)`, which refuses a discrete site, a site under a
branch or a repeated name BEFORE sampling.
  - [ ] the sites are listed without a single draw (a distribution whose
        `sample` throws does not stop it), branches marked
  - [ ] the coin model of TestBayes written with `ifS`: P(coin) against
        the exact 0.4295 by `metropolis`, and `Declared.nuts` refusing it
        by name before running
  - [ ] eight schools non-centered by `Declared.nuts`, against the exact
        posterior of TestVector
  - [ ] a repeated site name refused at declaration

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
its answer, and its gradient matches a finite difference to 1e-6. This
paragraph first blamed the geometry (on raw temperature α and β are almost
collinear); stage 4 REFUTED that: PyMC's own NUTS, handed okay's target for
the same model on the same raw temperatures, samples it with no divergence
and agrees with the grid. The sampler and the geometry are fine; what fails
is PyMC's model graph for the book's model, here (pytensor's pure-Python
backend, `cxx=`, is the one difference left untested). The same posterior through a linear change of variables (standardised
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

Stage 4 (2026-10-02), TestNuts / TestSamplers (default gate) and
TestPyMCSampler (Live): with `import okay.bayes.PyMC.given`, the same
`Smooth.nuts` calls run PyMC 6.3.2's NUTS on okay's target —

| PyMC on okay's target | exact | PyMC |
|---|---|---|
| Beta–Bernoulli mean, sd | 0.6765, 0.0791 | 0.6780, 0.0764 (ESS 817) |
| Challenger E[β], sd(β), p31 (grid) | 0.2693, 0.1163, 0.9874 | 0.2788, 0.1138, 0.9932 (ESS 238) |

zero divergences both. The second row is the finding that refutes stage
2a's geometry explanation (corrected there): the same NUTS on the same
raw-temperature posterior is fine when the density is okay's.

Stage 5a (2026-10-02), TestRank — JVM, Scala.js and Native. The book's
5% lower bound (mean − 1.65 sd) against the exact Beta quantile:

| votes | exact | the book's | off by |
|---|---|---|---|
| 1 up, 0 down | 0.2236 | 0.2778 | +0.054 |
| 5 up, 2 down | 0.4003 | 0.4207 | +0.020 |
| 50 up, 20 down | 0.6176 | 0.6206 | +0.003 |
| 500 up, 200 down | 0.6853 | 0.6855 | +0.0003 |

The approximation is always optimistic, and most where it matters — few
votes, exactly the items the bound exists to hold back. Ranked by ratio:
one of one first; by the exact bound: 999 of 1000, 750 of 1000, 75 of 100,
3 of 4, one of one. The spread of a mean of n Poisson(4.5) draws:
0.655 / 0.218 / 0.066 at n = 10 / 100 / 1000 against √(4.5/n) 0.671 /
0.212 / 0.067.

Stage 5b (2026-10-02), TestDecision: the squared, absolute and pinball(0.9)
actions equal the mean, median and 0.9-quantile of 2001 draws. The Price is
Right: true price 19 876 ± 3 767 by `adaptive` against the exact 19 899 ±
3 712. The showdown's best bid: 14 549 / 13 077 / 11 856 / 10 905 / 10 703 at
risks 30 000 / 60 000 / 90 000 / 120 000 / 150 000, against a posterior mean
of 19 876 — the book's shape, a bid far under the estimate and falling with
the risk.

Stage 5c (2026-10-02), TestAbTest — JVM, Scala.js and Native. Tiers $79 /
$49 / $25 / nothing; A 10 / 46 / 80 / 864 of 1000 visitors, B 45 / 84 / 200
/ 1671 of 2000. Revenue per visitor: A 5.173 ± 0.450 (exact 5.176 ±
0.451), B 6.397 ± 0.365 (exact 6.399 ± 0.365). P(B beats A) 0.9809 and
0.9812 in two independent runs; the lift 1.23 per visitor, its 95% HDI
from 0.10 to 2.37.

Stage 6 (2026-10-02), TestStreams (JVM, Scala.js, Native) and
TestBulkNuts (JVM). The online filter over 50 observations through a
`Stage`, 4000 particles: the worst step's filtering mean 0.078 Kalman sds
off, log p(y) −101.508 against −101.616. Over a Bulk and over a Vector the
same model's log density and gradient agree to 1e-9 at ten random points.
`Bayes.observeBulk` by `adaptive` on 2000 rows: μ 3.0746 against the
large-n 3.0720 ± 0.0430. AD NUTS over a `Bulk[Chunks]` of 20 000 rows:
μ 3.0156 ± 0.0144 (large-n 3.0150 ± 0.0142), σ 2.0044 ± 0.0101 (2.0040 ±
0.0100), 17.7 s on the JVM — one aggregate per gradient. Spark and Flink
run the same `Aggregator`; that it serialises there is not yet tested.

## 6. Open questions

- Vectorised sites (`sample("x", Normal(0, 1), size = n)`)? ANSWERED
  (2026-10-02, okay-bayes-vector): no new kind of site. A vector is n
  named scalars, `x[0]` … `x[n-1]`, so every sampler (MH, adaptive, NUTS,
  AD, SMC) takes it unchanged; what was missing was only the writing and
  the reading — `Bayes.sampleN(name, d, n)`, `Smooth.paramN(name, prior,
  n)`, `Posterior.vector(name)`. A distribution over vectors as ONE site
  (a Dirichlet, an LKJ correlation) would need a vector-valued trace and a
  simplex transform: not built, and nothing in the book needs it as a site.
  - [x] `sampleN` / `paramN` name the elements `name[i]` and answer them
        in order; `Posterior.vector` reads them back per element
  - [x] eight schools (Rubin 1981) with τ fixed — jointly Gaussian, so the
        posterior of μ and of every θⱼ is exact — by `adaptive` and by
        AD NUTS. Measured (TestVector, JVM/JS/Native): μ 4.33 by adaptive,
        4.27 by AD NUTS, exact 4.34; θ by adaptive 6.8 5.2 3.7 4.7 3.0 3.8
        7.0 5.0 against the exact 6.7 5.1 3.7 4.8 3.1 3.8 7.1 4.9 —
        every one within 4 standard errors, every sd within 10%
