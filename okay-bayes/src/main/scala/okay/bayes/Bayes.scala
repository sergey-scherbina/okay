package okay.bayes

import scala.annotation.tailrec
import scala.util.Random
import okay.{!, Effect, Free, effect}

/**
 * A BAYESIAN MODEL IS A PROGRAM (specs/okay-bayes.md): it draws named
 * random quantities and weighs its run by what it observed. Inference is
 * a HANDLER over it — the program is written once, and a forward draw,
 * likelihood weighting and Metropolis–Hastings are three readings.
 */
enum Model[+A] derives Effect:
  /** draw the quantity named `name` from `dist` — the name is its address across runs */
  case Sample[A](name: String, dist: Distribution[A]) extends Model[A]
  /** weigh this run by e^logWeight — an observation's likelihood */
  case Factor(logWeight: Double) extends Model[Unit]

/** one forward run: the program's value, every site's numeric value, the log prior and log likelihood */
final case class Run[A](value: A, sites: Map[String, Double], logPrior: Double, logLik: Double):
  def logJoint: Double = logPrior + logLik

/** one chain of draws: the program's values and every site's numeric value, per kept iteration */
final case class Chain[A](draws: Vector[A], sites: Vector[Map[String, Double]], acceptance: Map[String, Double]):
  def site(name: String): Vector[Double] = sites.flatMap(_.get(name))

/** the posterior: every chain; the values pooled, a site pooled, its R-hat across chains */
final case class Posterior[A](chains: Vector[Chain[A]]):
  def draws: Vector[A] = chains.flatMap(_.draws)
  def site(name: String): Vector[Double] = chains.flatMap(_.site(name))
  def rhat(name: String): Double = Summary.rhat(chains.map(_.site(name)))
  /** a vector drawn by `sampleN` / `Smooth.paramN`: per element `name[i]`, in index order, its pooled draws */
  def vector(name: String): Vector[Vector[Double]] =
    val prefix = name + "["
    val indices = chains.iterator.flatMap(_.sites.headOption.toList).flatMap(_.keysIterator)
      .collect { case k if k.startsWith(prefix) && k.endsWith("]") => k.substring(prefix.length, k.length - 1).toIntOption }
      .flatten.toVector.distinct.sorted
    indices.map(i => site(s"$name[$i]"))
  def acceptance: Map[String, Double] =
    chains.flatMap(_.acceptance).groupMapReduce(_._1)(_._2)(_ + _).view.mapValues(_ / chains.length).toMap

/**
 * what SMC ends with: each particle's value and sites, its normalized
 * weight, and the estimate of the log evidence log p(data)
 */
final case class Particles[A](values: Vector[A], sites: Vector[Map[String, Double]], weights: Vector[Double], logEvidence: Double, resamplings: Int):
  /** E[f(value)] under the weighted particles */
  def expect(f: A => Double): Double = values.iterator.zip(weights).map((a, w) => f(a) * w).sum
  /** a site's weighted mean, over the particles that drew it */
  def mean(name: String): Double =
    val held = sites.iterator.zip(weights).collect { case (s, w) if s.contains(name) => (s(name), w) }.toVector
    held.map((x, w) => x * w).sum / held.map(_._2).sum
  /** the effective number of particles, 1 / Σ w² */
  def ess: Double = 1 / weights.map(w => w * w).sum

object Bayes:
  /** draw `name` from `d` */
  inline def sample[A](name: String, d: Distribution[A]): A ! Model = effect(Model.Sample(name, d))

  /** `n` draws from `d`, each its own site `name[i]` — a vector as n named scalars, so every sampler takes it */
  def sampleN[A](name: String, d: Distribution[A], n: Int): Vector[A] ! Model =
    // recursion deferred into the program's flatMap: one step per element, trampolined by the handler loop
    def from(i: Int, acc: Vector[A]): Vector[A] ! Model =
      if i == n then okay.pure[Model, Vector[A]](acc) else sample(s"$name[$i]", d).flatMap(x => from(i + 1, acc :+ x))
    from(0, Vector.empty)

  /** weigh the run by e^logWeight */
  inline def factor(logWeight: Double): Unit ! Model = effect(Model.Factor(logWeight))

  /** OBSERVE `x` as a draw of `d`: the run is weighed by its likelihood */
  inline def observe[A](d: Distribution[A], x: A): Unit ! Model = factor(d.logPdf(x))

  /** observe every element, each under the distribution `d` gives it */
  def observeAll[X, A](xs: Iterable[X])(d: X => Distribution[A], value: X => A): Unit ! Model =
    factor(xs.iterator.map(x => d(x).logPdf(value(x))).sum)

  /**
   * observe a BULK — rows too many for one place (Chunks in one JVM, an RDD
   * on Spark): the log-likelihood is one `aggregate` of `logLik` over the
   * rows, a commutative sum, so it runs where the rows are
   */
  def observeBulk[D[_], R](rows: D[R])(logLik: R => Double)(using bulk: okay.Bulk[D]): Unit ! Model =
    factor(bulk.aggregate(rows)(Bayes.sumOf(logLik)))

  /** Σ f(row), as an Aggregator */
  private[bayes] def sumOf[R](f: R => Double): okay.Aggregator[R, Double, Double] = new okay.Aggregator[R, Double, Double]:
    def init: Double = 0.0
    def add(acc: Double, r: R): Double = acc + f(r)
    def merge(a: Double, b: Double): Double = a + b
    def present(acc: Double): Double = acc

  /** observe every element as its OWN factor — SMC reweighs between them; to MCMC it is `observeAll` */
  def observeEach[X, A](xs: Iterable[X])(d: X => Distribution[A], value: X => A): Unit ! Model =
    val v = xs.toVector
    // recursion deferred into the program's flatMap: one step per element, trampolined by the handler loop
    def from(i: Int): Unit ! Model =
      if i == v.length then okay.pure[Model, Unit](()) else observe(d(v(i)), value(v(i))).flatMap(_ => from(i + 1))
    from(0)

  /** a site's value and its distribution, held at ONE type — so a value is
   * proposed and re-scored by its own distribution, with no cast */
  private[bayes] final class Site[X](val dist: Distribution[X], val value: X):
    val logp: Double = dist.logPdf(value)
    def numeric: Double = dist.numeric(value)
    def proposed(scale: Double, rng: Random): Site[X] = Site(dist, dist.propose(value, scale, rng))
    def raw: Any = value

  /**
   * RUN the program once: each `Sample` takes its value from `fixed` when
   * the site is there and the value lies in the distribution's support
   * (`reused`), else draws it fresh. Answers the value, the trace, the log
   * likelihood, and which sites were drawn fresh.
   */
  private[bayes] final case class Pass[A](value: A, trace: Map[String, Site[?]], logLik: Double, fresh: Set[String]):
    def logPrior: Double = trace.valuesIterator.map(_.logp).sum
    def logJoint: Double = logPrior + logLik

  private[bayes] def pass[A](p: A ! Model, fixed: Map[String, Any], rng: Random): Pass[A] =
    var trace = Map.empty[String, Site[?]]
    var fresh = Set.empty[String]
    var logLik = 0.0
    def answer[X](op: Model[X]): X = op match
      case Model.Sample(name, d) =>
        val reused = fixed.get(name).flatMap(d.coerce).filter(v => d.logPdf(v) > Distribution.NegInf)
        val v = reused.getOrElse { fresh += name; d.sample(rng) }
        trace = trace.updated(name, Site(d, v))
        v
      case Model.Factor(w) =>
        logLik += w
        ()
    @tailrec def loop(x: A ! Model): A = (x.resume: @unchecked) match
      case Free.Return(a) => a
      case Free.Inject(op: Model[A] @unchecked) => answer(op)
      case Free.Bind(Free.Inject(op: Model[x] @unchecked), k) => loop(k(answer(op)))
    val a = loop(p)
    Pass(a, trace, logLik, fresh)

  /** ONE FORWARD DRAW from the prior, weighed by the observations */
  def prior[A](p: A ! Model, rng: Random): Run[A] =
    val r = pass(p, Map.empty, rng)
    Run(r.value, r.trace.view.mapValues(_.numeric).toMap, r.logPrior, r.logLik)

  /** LIKELIHOOD WEIGHTING: `n` forward draws, each with its normalized weight */
  def weighted[A](p: => A ! Model, n: Int, rng: Random): Vector[(A, Double)] =
    val runs = Vector.fill(n)(pass(p, Map.empty, rng))
    val top = runs.iterator.map(_.logLik).max
    val ws = runs.map(r => math.exp(r.logLik - top))
    val total = ws.sum
    runs.zip(ws).map((r, w) => (r.value, w / total))

  /**
   * LIGHTWEIGHT METROPOLIS–HASTINGS (Wingate, Stuhlmüller & Goodman,
   * AISTATS 2011), one SWEEP per iteration: every site of the current
   * trace, in random order, gets a symmetric proposal from its own
   * distribution, the program is RE-RUN with the other sites' values
   * reused, and the move is accepted with
   *   log α = log p(x') − log p(x) + log(|x| / |x'|) + log p(stale) − log p(fresh)
   * — the last three terms the correction for a model whose sites depend on
   * a draw (zero when the structure does not change). Proposal scales are
   * tuned per site during burn-in, PyMC's Metropolis rule: toward an
   * acceptance near 0.2–0.5. `chains` independent chains, seeds `seed + i`.
   */
  def metropolis[A](p: A ! Model, samples: Int, burn: Int = 1000, thin: Int = 1, chains: Int = 1, seed: Long = 42L): Posterior[A] =
    Posterior(Vector.tabulate(chains)(i => chain(p, samples, burn, thin, new Random(seed + i))))

  /**
   * ADAPTIVE METROPOLIS (Haario, Saksman & Tamminen, Bernoulli 2001) for
   * the CONTINUOUS sites: single-site moves for the first half of burn-in
   * while their draws are collected, then ONE joint move per iteration from
   * N(x, (2.38²/d) Σ + εI), Σ the collected covariance, re-estimated through
   * the rest of burn-in and frozen after it. Discrete sites keep their
   * single-site moves. Where two parameters lie along a ridge (the book's
   * Challenger regression: α and β correlated near −0.99), a joint step
   * moves along it and single-site steps cannot.
   */
  def adaptive[A](p: A ! Model, samples: Int, burn: Int = 2000, thin: Int = 1, chains: Int = 1, seed: Long = 42L): Posterior[A] =
    Posterior(Vector.tabulate(chains)(i => chain(p, samples, burn, thin, new Random(seed + i), joint = true)))

  /**
   * RUN a suspended program to its next `Factor`, drawing every `Sample`
   * on the way from its prior: answers what is left of the program (its
   * value when it ended), the factor's log weight, and the sites so far.
   */
  /** a program run up to some factor: what is left of it (or its value), its sites, the log likelihood so far */
  private[bayes] final case class Prefix[A](rest: Either[A, A ! Model], trace: Map[String, Site[?]], logLik: Double, fresh: Set[String], factors: Int,
    added: Double = 0.0):
    def pass: Pass[Unit] = Pass((), trace, logLik, fresh)

  private[bayes] def start[A](p: A ! Model): Prefix[A] = Prefix(Right(p), Map.empty, 0.0, Set.empty, 0)

  /**
   * RUN from `from` until `stopAfter` more factors have been weighed (or
   * the program ends): each `Sample` takes its value from `fixed` when it
   * can, as `pass` does, else draws it. The one interpreter behind SMC's
   * advance (nothing fixed, one factor) and resample-move's re-run of a
   * particle's prefix (its sites fixed, back to the same factor).
   */
  private[bayes] def runFactors[A](from: Prefix[A], fixed: Map[String, Any], stopAfter: Int, rng: Random): Prefix[A] =
    var trace = from.trace
    var fresh = from.fresh
    var logLik = from.logLik
    var factors = from.factors
    var added = 0.0
    val goal = from.factors + stopAfter
    def sampleSite[X](name: String, d: Distribution[X]): X =
      val reused = fixed.get(name).flatMap(d.coerce).filter(v => d.logPdf(v) > Distribution.NegInf)
      val v = reused.getOrElse { fresh += name; d.sample(rng) }
      trace = trace.updated(name, Site(d, v))
      v
    def last(op: Model[A]): A = op match
      case Model.Sample(name, d) => sampleSite(name, d)
      case Model.Factor(w) =>
        logLik += w
        added += w
        factors += 1
        ()
    def step[X](op: Model[X], k: X => A ! Model): Either[A ! Model, A ! Model] = op match
      case Model.Sample(name, d) => Left(k(sampleSite(name, d)))
      case Model.Factor(w) =>
        logLik += w
        added += w
        factors += 1
        if factors >= goal then Right(k(())) else Left(k(()))
    @tailrec def loop(x: A ! Model): Either[A, A ! Model] = (x.resume: @unchecked) match
      case Free.Return(a) => Left(a)
      case Free.Inject(op: Model[A] @unchecked) => Left(last(op))
      case Free.Bind(Free.Inject(op: Model[x] @unchecked), k) => step(op, k) match
        case Left(more) => loop(more)
        case Right(rest) => Right(rest)
    from.rest match
      case Left(_) => from.copy(added = 0.0)
      case Right(x) =>
        val rest = loop(x)
        Prefix(rest, trace, logLik, fresh, factors, added)

  private def logSumExp(xs: Vector[Double]): Double =
    val top = xs.max
    if top == Distribution.NegInf then top else top + math.log(xs.iterator.map(x => math.exp(x - top)).sum)

  /**
   * SEQUENTIAL MONTE CARLO (Del Moral, Doucet & Jasra 2006; as a
   * probabilistic-programming inference, Wood, van de Meent & Mansinghka
   * 2014). Each particle is the program SUSPENDED at its next `Factor`,
   * the samples before it drawn from their priors on the way; at every
   * factor the particles are reweighed, and when the effective sample size
   * falls under half they are resampled (systematic resampling). A particle
   * chosen twice RESUMES ONE CONTINUATION TWICE — multi-shot, sound because
   * a program value is immutable: each copy draws its own future. Answers
   * the weighted particles and log p(data), the product of the mean
   * incremental weights (unbiased for the evidence).
   */
  def smc[A](p: A ! Model, particles: Int, seed: Long = 42L, move: Option[Kernel] = None): Particles[A] =
    require(particles > 0, "smc: at least one particle")
    val rng = new Random(seed)
    val n = particles
    var parts = Vector.fill(n)(start(p))
    var lw = Vector.fill(n)(0.0)
    var logZ = 0.0
    var resamplings = 0
    while parts.exists(_.rest.isRight) do
      parts = parts.map(runFactors(_, Map.empty, 1, rng))
      lw = lw.zip(parts).map((l, q) => l + q.added)
      val total = logSumExp(lw)
      require(total > Distribution.NegInf, "smc: every particle has weight zero — the observations rule out all of them")
      val ws = lw.map(l => math.exp(l - total))
      if parts.exists(_.rest.isRight) && 1 / ws.map(w => w * w).sum < n / 2.0 then
        logZ += total - math.log(n)
        // systematic: one uniform, n evenly spaced pointers into the cumulative weights
        val u = rng.nextDouble() / n
        val cum = ws.scanLeft(0.0)(_ + _).tail
        var j = 0
        val idx = Vector.tabulate(n) { i =>
          val at = u + i.toDouble / n
          while j < n - 1 && cum(j) < at do j += 1
          j
        }
        parts = idx.map(parts)
        lw = Vector.fill(n)(0.0)
        resamplings += 1
        // RESAMPLE-MOVE: each particle moved by a kernel whose target is the posterior given the observations so far
        move.foreach(k => parts = parts.map(moved(p, _, k, rng)))
    val total = logSumExp(lw)
    logZ += total - math.log(n)
    val values = parts.map(_.rest match
      case Left(a) => a
      case Right(_) => throw IllegalStateException("smc: a particle did not finish"))
    Particles(values, parts.map(_.trace.view.mapValues(_.numeric).toMap), lw.map(l => math.exp(l - total)), logZ, resamplings)

  /**
   * a particle's prefix MOVED by `k` (Gilks & Berzuini 2001; Chopin 2002):
   * the kernel sees the model re-run with its sites held and stopped after
   * the same number of factors — a model of the observations so far — so
   * its step leaves the current posterior invariant; the moved prefix is
   * re-run once more to pick up its continuation
   */
  private def moved[A](p: A ! Model, q: Prefix[A], k: Kernel, rng: Random): Prefix[A] =
    val run: (Map[String, Any], Random) => Pass[Unit] = (fixed, r) => runFactors(start(p), fixed, q.factors, r).pass
    val t = Trace(run, q.pass)
    val after = k.step(t, false, rng)
    if after eq t then q
    else runFactors(start(p), after.pass.trace.view.mapValues(_.raw).toMap, q.factors, rng).copy(added = q.added)

  /**
   * THE NO-U-TURN SAMPLER over an ordinary model (specs/okay-bayes.md
   * stage 3a): every site moves in unconstrained space through its
   * distribution's `Support`, the log Jacobian added, and the gradient is
   * by central finite differences — 2d + 1 runs of the program per
   * gradient, so this is for models of tens of parameters, not thousands.
   * A discrete site, a support that changes with another site's value, and
   * a structure that changes with a draw are refused by name: they need
   * `metropolis`. The acceptance map carries "(nuts accept)",
   * "(divergent)" (the share of draws whose trajectory diverged) and
   * "(tree depth)". The sampler is the `Sampler` in scope — ours unless an
   * import names another (`okay.bayes.PyMC.given`).
   */
  def nuts[A](p: A ! Model, samples: Int, burn: Int = 1000, chains: Int = 1, seed: Long = 42L, delta: Double = 0.8)(using sampler: Sampler): Posterior[A] =
    val first = pass(p, Map.empty, new Random(seed))
    val names = first.trace.keys.toVector.sorted
    val supports = names.map(n => n -> first.trace(n).dist.support).toMap
    for n <- names do
      require(supports(n).continuous, s"nuts: site '$n' is discrete (${first.trace(n).dist}) — sample it with metropolis")
    val quiet = new Random(0)
    def at(u: Array[Double]): Option[(Pass[A], Double)] =
      var logJ = 0.0
      val fixed = names.indices.map { i =>
        val (x, j) = supports(names(i)).constrain(u(i))
        logJ += j
        names(i) -> (x: Any)
      }.toMap
      val r = pass(p, fixed, quiet)
      if r.trace.keySet != names.toSet then
        throw IllegalArgumentException(s"nuts: the model's sites changed with a draw (${r.trace.keySet} against $names) — use metropolis")
      for n <- names do
        if r.trace(n).dist.support != supports(n) then
          throw IllegalArgumentException(s"nuts: site '$n' changed its support with another site's value — use metropolis")
      // a site drawn fresh was handed a value its distribution rules out: rounding at the edge of an interval
      if r.fresh.nonEmpty then None else Some((r, logJ))
    val target = Target.finite(names.length) { u =>
      at(u) match
        case Some((r, logJ)) if r.logJoint > Distribution.NegInf => r.logJoint + logJ
        case _ => Distribution.NegInf
    }
    Posterior(Vector.tabulate(chains) { c =>
      val ch = sampler.run(target, samples, burn, seed + c, delta)
      val runs = ch.draws.map(u => at(u.toArray).get._1)
      Chain(runs.map(_.value), runs.map(_.trace.view.mapValues(_.numeric).toMap),
        Map("(nuts accept)" -> ch.acceptance, "(divergent)" -> ch.divergences.toDouble / math.max(1, samples), "(tree depth)" -> ch.meanDepth))
    })

  /**
   * SAMPLE WITH A KERNEL built from `Kernel`'s parts (specs/okay-bayes.md
   * 7a): `kernel` is evaluated once per chain, so each chain tunes its own;
   * it tunes during `burn` and is frozen after. Chains start, as
   * `metropolis`'s do, at a prior draw the observations allow.
   */
  def sample[A](p: A ! Model, kernel: => Kernel, samples: Int, burn: Int = 1000, thin: Int = 1, chains: Int = 1, seed: Long = 42L): Posterior[A] =
    Posterior(Vector.tabulate(chains) { c =>
      val rng = new Random(seed + c)
      val k = kernel
      var cur = pass(p, Map.empty, rng)
      var tries = 1
      while cur.logJoint == Distribution.NegInf && tries < 10000 do { cur = pass(p, Map.empty, rng); tries += 1 }
      require(cur.logJoint > Distribution.NegInf, "sample: no prior draw the observations allow in 10 000 tries")
      var t = Trace((fixed, r) => pass(p, fixed, r), cur)
      val draws = Vector.newBuilder[A]
      val sites = Vector.newBuilder[Map[String, Double]]
      var i = 0
      while i < burn + samples * thin do
        t = k.step(t, i < burn, rng)
        if i >= burn && (i - burn) % thin == 0 then
          draws += t.value
          sites += t.sites
        i += 1
      Chain(draws.result(), sites.result(), k.acceptance)
    })

  /** the lower Cholesky factor of a symmetric positive-definite matrix */
  private def cholesky(m: Array[Array[Double]]): Array[Array[Double]] =
    val n = m.length
    val l = Array.ofDim[Double](n, n)
    for i <- 0 until n; j <- 0 to i do
      var s = m(i)(j)
      var k = 0
      while k < j do { s -= l(i)(k) * l(j)(k); k += 1 }
      l(i)(j) = if i == j then math.sqrt(math.max(s, 1e-12)) else s / l(j)(j)
    l

  private def chain[A](p: A ! Model, samples: Int, burn: Int, thin: Int, rng: Random): Chain[A] =
    chain(p, samples, burn, thin, rng, joint = false)

  private def chain[A](p: A ! Model, samples: Int, burn: Int, thin: Int, rng: Random, joint: Boolean): Chain[A] =
    // a start the observations do not rule out
    var cur = pass(p, Map.empty, rng)
    var tries = 1
    while cur.logJoint == Distribution.NegInf && tries < 10000 do { cur = pass(p, Map.empty, rng); tries += 1 }
    require(cur.logJoint > Distribution.NegInf, "metropolis: no prior draw the observations allow in 10 000 tries")
    val scale = scala.collection.mutable.HashMap.empty[String, Double].withDefaultValue(1.0)
    val tried = scala.collection.mutable.HashMap.empty[String, Int].withDefaultValue(0)
    val took = scala.collection.mutable.HashMap.empty[String, Int].withDefaultValue(0)
    val windowTried = scala.collection.mutable.HashMap.empty[String, Int].withDefaultValue(0)
    val windowTook = scala.collection.mutable.HashMap.empty[String, Int].withDefaultValue(0)
    val draws = Vector.newBuilder[A]
    val sites = Vector.newBuilder[Map[String, Double]]

    def tune(name: String): Unit =
      if windowTried(name) >= 100 then
        val rate = windowTook(name).toDouble / windowTried(name)
        val f = if rate < 0.001 then 0.1 else if rate < 0.05 then 0.5 else if rate < 0.2 then 0.9
          else if rate > 0.95 then 10.0 else if rate > 0.75 then 2.0 else if rate > 0.5 then 1.1 else 1.0
        scale(name) = scale(name) * f
        windowTried(name) = 0
        windowTook(name) = 0

    def move(name: String, tuning: Boolean): Unit = cur.trace.get(name).foreach { site =>
      val proposed = site.proposed(scale(name), rng)
      val fixed: Map[String, Any] = cur.trace.view.mapValues(_.raw).toMap.updated(name, proposed.raw)
      val next = pass(p, fixed, rng)
      val logAlpha =
        if next.logJoint == Distribution.NegInf then Distribution.NegInf
        else
          val stale = cur.trace.keySet -- next.trace.keySet
          val freshNew = next.fresh - name
          next.logJoint - cur.logJoint +
            math.log(cur.trace.size.toDouble / next.trace.size) +
            stale.iterator.map(cur.trace(_).logp).sum - freshNew.iterator.map(next.trace(_).logp).sum
      tried(name) += 1
      if tuning then windowTried(name) += 1
      if math.log(rng.nextDouble()) < logAlpha then
        cur = next
        took(name) += 1
        if tuning then windowTook(name) += 1
      if tuning then tune(name)
    }

    // the continuous sites, and what the adaptive step has learnt of them
    def continuous: Vector[String] =
      cur.trace.toVector.collect { case (n, site) if site.raw.isInstanceOf[Double] => n }.sorted
    val names = if joint then continuous else Vector.empty
    val d = names.length
    // an ArrayBuffer, not a Vector builder: the covariance is re-read while draws are still being added,
    // and a builder's state after `result()` is undefined
    val seen = scala.collection.mutable.ArrayBuffer.empty[Array[Double]]
    var factor: Option[Array[Array[Double]]] = None
    var jointScale = 1.0
    var jointTried = 0
    var jointTook = 0
    def learn(): Unit =
      val xs = seen
      if xs.length > 2 * d + 10 then
        val mean = Array.tabulate(d)(k => xs.iterator.map(_(k)).sum / xs.length)
        val cov = Array.tabulate(d, d)((a, b) => xs.iterator.map(x => (x(a) - mean(a)) * (x(b) - mean(b))).sum / (xs.length - 1))
        val sd = 2.38 * 2.38 / d
        factor = Some(cholesky(Array.tabulate(d, d)((a, b) => sd * cov(a)(b) + (if a == b then 1e-8 else 0.0))))
    def jointMove(l: Array[Array[Double]], tuning: Boolean): Unit =
      val now = names.map(n => cur.trace.get(n).map(_.raw))
      if now.forall(_.exists(_.isInstanceOf[Double])) then
        val x = now.map { case Some(v: Double) => v; case _ => 0.0 }
        val z = Array.fill(d)(rng.nextGaussian())
        val y = Array.tabulate(d)(a => x(a) + jointScale * (0 to a).iterator.map(b => l(a)(b) * z(b)).sum)
        val fixed = names.indices.foldLeft(cur.trace.view.mapValues(_.raw).toMap)((m, k) => m.updated(names(k), y(k)))
        val next = pass(p, fixed, rng)
        val logAlpha = if next.logJoint == Distribution.NegInf || next.fresh.nonEmpty then Distribution.NegInf else next.logJoint - cur.logJoint
        tried("(joint)") += 1
        val ok = math.log(rng.nextDouble()) < logAlpha
        if ok then { cur = next; took("(joint)") += 1 }
        if tuning then
          // the single-site rule, on the joint step's own window
          jointTried += 1
          if ok then jointTook += 1
          if jointTried >= 100 then
            val rate = jointTook.toDouble / jointTried
            jointScale *= (if rate < 0.001 then 0.1 else if rate < 0.05 then 0.5 else if rate < 0.2 then 0.9
              else if rate > 0.95 then 10.0 else if rate > 0.75 then 2.0 else if rate > 0.5 then 1.1 else 1.0)
            jointTried = 0
            jointTook = 0
    def record(): Unit =
      if d > 0 then
        val v = names.map(n => cur.trace.get(n).map(_.raw))
        if v.forall(_.exists(_.isInstanceOf[Double])) then seen += v.map { case Some(x: Double) => x; case _ => 0.0 }.toArray: Unit

    var i = 0
    while i < burn + samples * thin do
      val tuning = i < burn
      factor match
        case Some(l) if joint =>
          jointMove(l, tuning)
          rng.shuffle(cur.trace.keys.toVector.filterNot(names.contains)).foreach(move(_, tuning))
        case _ =>
          rng.shuffle(cur.trace.keys.toVector).foreach(move(_, tuning))
      if joint && tuning then
        record()
        if i >= burn / 2 && (factor.isEmpty || i % 500 == 0) then learn()
      if !tuning && (i - burn) % thin == 0 then
        draws += cur.value
        sites += cur.trace.view.mapValues(_.numeric).toMap
      i += 1
    Chain(draws.result(), sites.result(), tried.keys.map(n => n -> took(n).toDouble / math.max(1, tried(n))).toMap)
