package okay.bayes

import scala.util.Random
import okay.!

/** a model's current state in a chain: the program's value, every site, the log joint */
final class Trace[A] private[bayes] (private[bayes] val model: A ! Model, private[bayes] val pass: Bayes.Pass[A]):
  def value: A = pass.value
  def sites: Map[String, Double] = pass.trace.view.mapValues(_.numeric).toMap
  def logJoint: Double = pass.logJoint

/**
 * A MARKOV KERNEL on a model's traces (specs/okay-bayes.md stage 7a): one
 * step that leaves the posterior invariant. Invariant kernels are closed
 * under sequence and mixture (Tierney, *Markov Chains for Exploring
 * Posterior Distributions*, Ann. Stat. 1994), so a sampler is BUILT:
 * `Kernel.nuts("lambda_1", "lambda_2") >>> Kernel.site("tau")` is NUTS on
 * the rates and a random walk on the switch point, each holding the other.
 * A kernel tunes itself only while `tuning` (burn-in): one tuned while
 * sampling would not be invariant. A kernel holds that tuning, so a chain
 * takes its own (`Bayes.sample` builds one per chain).
 */
trait Kernel:
  def step[A](t: Trace[A], tuning: Boolean, rng: Random): Trace[A]
  /** proposals accepted, by what was moved */
  def acceptance: Map[String, Double]

  /** this, then `next` */
  final def >>>(next: Kernel): Kernel = Kernel.Sequence(this, next)
  /** this, `n` times in a row */
  final def times(n: Int): Kernel = Vector.fill(math.max(1, n))(this).reduce(_ >>> _)

object Kernel:
  /** single-site random-walk Metropolis–Hastings on `name` (Wingate et al. 2011, the structure correction kept) */
  def site(name: String): Kernel = SiteMove(name)

  /** a sweep: each site in turn */
  def sites(names: String*): Kernel = names.map(site).reduce(_ >>> _)

  /** a sweep over whatever sites the trace holds, in random order — a model whose structure changes included */
  def everySite: Kernel = EverySite()

  /** a NUTS transition on these continuous sites, every other site held (Metropolis-within-Gibbs with a NUTS block) */
  def nuts(names: String*): Kernel = NutsBlock(names.toVector, 0.8, 10)

  /** one of `parts`, chosen by weight each step */
  def mixture(parts: (Double, Kernel)*): Kernel =
    require(parts.nonEmpty && parts.forall(_._1 > 0), "Kernel.mixture: positive weights")
    Mixture(parts.toVector)

  private[bayes] final case class Sequence(first: Kernel, second: Kernel) extends Kernel:
    def step[A](t: Trace[A], tuning: Boolean, rng: Random): Trace[A] = second.step(first.step(t, tuning, rng), tuning, rng)
    def acceptance: Map[String, Double] = first.acceptance ++ second.acceptance

  private final case class Mixture(parts: Vector[(Double, Kernel)]) extends Kernel:
    private val cum = parts.map(_._1).scanLeft(0.0)(_ + _).tail.map(_ / parts.map(_._1).sum)
    def step[A](t: Trace[A], tuning: Boolean, rng: Random): Trace[A] =
      val u = rng.nextDouble()
      val i = cum.indexWhere(u < _)
      parts(if i < 0 then parts.length - 1 else i)._2.step(t, tuning, rng)
    def acceptance: Map[String, Double] = parts.flatMap(_._2.acceptance).toMap

  /** the random-walk move of `Bayes.metropolis`, as a kernel: PyMC's tuning rule on 100-proposal windows */
  private final class SiteMove(name: String) extends Kernel:
    private var scale = 1.0
    private var (tried, took, windowTried, windowTook) = (0, 0, 0, 0)
    def acceptance: Map[String, Double] = if tried == 0 then Map.empty else Map(name -> took.toDouble / tried)
    def step[A](t: Trace[A], tuning: Boolean, rng: Random): Trace[A] = t.pass.trace.get(name) match
      case None => t
      case Some(site) =>
        val cur = t.pass
        val proposed = site.proposed(scale, rng)
        val next = Bayes.pass(t.model, cur.trace.view.mapValues(_.raw).toMap.updated(name, proposed.raw), rng)
        val logAlpha =
          if next.logJoint == Distribution.NegInf then Distribution.NegInf
          else
            val stale = cur.trace.keySet -- next.trace.keySet
            val freshNew = next.fresh - name
            next.logJoint - cur.logJoint + math.log(cur.trace.size.toDouble / next.trace.size) +
              stale.iterator.map(cur.trace(_).logp).sum - freshNew.iterator.map(next.trace(_).logp).sum
        val ok = math.log(rng.nextDouble()) < logAlpha
        tried += 1
        if ok then took += 1
        if tuning then
          windowTried += 1
          if ok then windowTook += 1
          if windowTried >= 100 then
            val rate = windowTook.toDouble / windowTried
            scale *= (if rate < 0.001 then 0.1 else if rate < 0.05 then 0.5 else if rate < 0.2 then 0.9
              else if rate > 0.95 then 10.0 else if rate > 0.75 then 2.0 else if rate > 0.5 then 1.1 else 1.0)
            windowTried = 0
            windowTook = 0
        if ok then Trace(t.model, next) else t

  private final class EverySite extends Kernel:
    private val moves = scala.collection.mutable.HashMap.empty[String, SiteMove]
    def acceptance: Map[String, Double] = moves.valuesIterator.flatMap(_.acceptance).toMap
    def step[A](t: Trace[A], tuning: Boolean, rng: Random): Trace[A] =
      rng.shuffle(t.pass.trace.keys.toVector).foldLeft(t)((tr, n) => moves.getOrElseUpdate(n, SiteMove(n)).step(tr, tuning, rng))

  /**
   * NUTS on a block: the target is the log joint as a function of the
   * block's sites, unconstrained through their supports, with every other
   * site held at its current value — rebuilt each step, since the held
   * sites move between steps. Gradient by central differences.
   */
  private final class NutsBlock(names: Vector[String], delta: Double, maxDepth: Int) extends Kernel:
    private val nuts = Nuts.Adapting(delta, maxDepth)
    private var (tried, diverged) = (0, 0)
    private var statSum = 0.0
    def acceptance: Map[String, Double] =
      if tried == 0 then Map.empty
      else Map(s"(nuts ${names.mkString(",")})" -> statSum / tried, s"(divergent ${names.mkString(",")})" -> diverged.toDouble / tried)
    def step[A](t: Trace[A], tuning: Boolean, rng: Random): Trace[A] =
      val block = names.filter(t.pass.trace.contains)
      if block.isEmpty then t
      else
        val supports = block.map { n =>
          val s = t.pass.trace(n).dist.support
          require(s.continuous, s"Kernel.nuts: site '$n' is discrete — move it with Kernel.site")
          s
        }
        val held = t.pass.trace.view.mapValues(_.raw).toMap
        val quiet = new Random(0)
        def at(u: Array[Double]): Option[(Bayes.Pass[A], Double)] =
          var logJ = 0.0
          val fixed = block.indices.foldLeft(held) { (m, i) =>
            val (x, j) = supports(i).constrain(u(i))
            logJ += j
            m.updated(block(i), x)
          }
          val r = Bayes.pass(t.model, fixed, quiet)
          // a fresh draw is a value its distribution refused, or a structure the held sites did not have: not this block's
          if r.fresh.nonEmpty || r.trace.keySet != t.pass.trace.keySet then None else Some((r, logJ))
        val target = Target.finite(block.length)(u => at(u).fold(Distribution.NegInf)((r, logJ) => r.logJoint + logJ))
        val u0 = block.indices.map(i => supports(i).unconstrain(t.pass.trace(block(i)).numeric)).toArray
        val u1 = nuts.step(target, u0, tuning, rng)
        tried += 1
        statSum += nuts.lastStat
        if nuts.lastDiverged then diverged += 1
        at(u1).fold(t)((r, _) => Trace(t.model, r))
