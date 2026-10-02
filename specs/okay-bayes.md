# okay-bayes — Bayesian inference as effects, without Python

Status: stage 1 landed (2026-10-02); specification 2026-10-02 (operator ask: "Bayesian Methods for
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
  def metropolis[A](p: A ! Model, samples, burn, thin, seed): Posterior[A]

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

Stage 2 — SMC (multi-shot), mixtures and convergence (ch.3), Thompson
sampling for bandits (ch.6). Stage 3 — HMC/NUTS with automatic
differentiation. Stage 4 — `Inference` as a facade (specs/own-or-standard.md):
ours by default, PyMC/Stan behind an import over an optional dependency.

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

## 6. Open questions

- Vectorised sites (`sample("x", Normal(0, 1), size = n)`)? Stage 1 names
  each scalar; a vector site when a model's runtime asks.
