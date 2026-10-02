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
  def acceptance: Map[String, Double] =
    chains.flatMap(_.acceptance).groupMapReduce(_._1)(_._2)(_ + _).view.mapValues(_ / chains.length).toMap

object Bayes:
  /** draw `name` from `d` */
  inline def sample[A](name: String, d: Distribution[A]): A ! Model = effect(Model.Sample(name, d))

  /** weigh the run by e^logWeight */
  inline def factor(logWeight: Double): Unit ! Model = effect(Model.Factor(logWeight))

  /** OBSERVE `x` as a draw of `d`: the run is weighed by its likelihood */
  inline def observe[A](d: Distribution[A], x: A): Unit ! Model = factor(d.logPdf(x))

  /** observe every element, each under the distribution `d` gives it */
  def observeAll[X, A](xs: Iterable[X])(d: X => Distribution[A], value: X => A): Unit ! Model =
    factor(xs.iterator.map(x => d(x).logPdf(value(x))).sum)

  /** a site's value and its distribution, held at ONE type — so a value is
   * proposed and re-scored by its own distribution, with no cast */
  private final class Site[X](val dist: Distribution[X], val value: X):
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
  private final case class Pass[A](value: A, trace: Map[String, Site[?]], logLik: Double, fresh: Set[String]):
    def logPrior: Double = trace.valuesIterator.map(_.logp).sum
    def logJoint: Double = logPrior + logLik

  private def pass[A](p: A ! Model, fixed: Map[String, Any], rng: Random): Pass[A] =
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
