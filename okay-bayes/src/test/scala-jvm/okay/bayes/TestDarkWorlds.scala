package okay.bayes

import scala.io.Source
import okay.{!, Bulk, Chunks}
import okay.testkit.Munit.Diagnosed
import Bayes.*
import Distribution.*

/**
 * Bayesian Methods for Hackers ch.5, Kaggle's "Observing Dark Worlds": a
 * dark-matter halo bends the light of the galaxies behind it, so their
 * ellipticities line up TANGENTIALLY around it, by m / max(r, 240). The
 * skies are a simulation, and the true halo positions come with them.
 *
 * The model reads a sky as a BULK and observes it with `observeBulk`: one
 * aggregate over the galaxies per run, wherever they are. Written over any
 * `Bulk[D]`, it runs on `Chunks` here and on an RDD with Spark's instance
 * in scope; the grid oracle reads the CSV by itself, as a Vector, so it
 * checks the Bulk road rather than sharing it.
 */
object DarkWorlds:
  final case class Galaxy(x: Double, y: Double, e1: Double, e2: Double)

  /** sky n through the Bulk in scope: its CSV, each row a galaxy, held for the many passes a sampler makes */
  def galaxies[D[_]](n: Int)(using bulk: Bulk[D]): D[Galaxy] =
    bulk.cache(bulk.map(bulk.csv(s"bmh/darkworlds/Training_Sky$n.csv"))(r =>
      Galaxy(r("x").toDouble, r("y").toDouble, r("e1").toDouble, r("e2").toDouble)))

  /** the oracle's own reading of sky n */
  def sky(n: Int): Vector[Galaxy] =
    Source.fromInputStream(getClass.getResourceAsStream(s"/bmh/darkworlds/Training_Sky$n.csv")).getLines().drop(1)
      .map(_.split(',')).map(r => Galaxy(r(1).toDouble, r(2).toDouble, r(3).toDouble, r(4).toDouble)).toVector

  /** the true halo position of sky n (the simulation's) */
  def truth(n: Int): (Double, Double) =
    Source.fromInputStream(getClass.getResourceAsStream("/bmh/darkworlds/Training_halos.csv")).getLines().drop(1)
      .map(_.split(',')).collectFirst { case r if r(0) == s"Sky$n" => (r(4).toDouble, r(5).toDouble) }.get

  /** the book's tau = 1/0.05 is a precision: sd √0.05 */
  val noise: Double = math.sqrt(0.05)

  /** the book's mean ellipticity of a galaxy under a halo at (hx, hy) of mass m: −m / max(r, 240) · (cos 2θ, sin 2θ),
   * θ = atan(dy/dx); cos 2θ and sin 2θ written without trigonometry, so AD needs none */
  def shear(g: Galaxy, hx: Double, hy: Double, m: Double): (Double, Double) =
    val (dx, dy) = (g.x - hx, g.y - hy)
    val r2 = dx * dx + dy * dy
    val f = m / math.max(math.sqrt(r2), 240)
    (-f * (dx * dx - dy * dy) / r2, -f * 2 * dx * dy / r2)

  /** one galaxy's log likelihood under a halo at (hx, hy) of mass m */
  def galaxyLogLik(g: Galaxy, hx: Double, hy: Double, m: Double): Double =
    val (m1, m2) = shear(g, hx, hy, m)
    Normal(m1, noise).logPdf(g.e1) + Normal(m2, noise).logPdf(g.e2)

  /** the oracle's: summed over its own Vector */
  def logLik(gs: Vector[Galaxy], hx: Double, hy: Double, m: Double): Double =
    gs.iterator.map(galaxyLogLik(_, hx, hy, m)).sum

  /** the book's model: mass ~ Uniform(40, 180), position ~ Uniform(0, 4200)²; the sky observed as a Bulk */
  def halo[D[_]](gs: D[Galaxy])(using Bulk[D]) = for
    m <- sample("mass", Uniform(40, 180))
    x <- sample("x", Uniform(0, 4200))
    y <- sample("y", Uniform(0, 4200))
    _ <- observeBulk(gs)(galaxyLogLik(_, x, y, m))
  yield (x, y, m)

  /** the same model over Grad, for AD NUTS: each galaxy's density over the parameters, its gradient summed over the Bulk */
  def haloAd[D[_]](gs: D[Galaxy])(using Bulk[D]) = for
    m <- Smooth.param("mass", Smooth.Uniform(40, 180))
    x <- Smooth.param("x", Smooth.Uniform(0, 4200))
    y <- Smooth.param("y", Smooth.Uniform(0, 4200))
    _ <- Smooth.observeBulk(gs, Vector(x, y, m)) { (p, g) =>
      val (dx, dy) = (g.x - p(0), g.y - p(1))
      val r2 = dx * dx + dy * dy
      val r = Real.sqrt(r2)
      val f = p(2) / (if r.value > 240 then r else Real.const(240))
      Smooth.Normal(-f * (dx * dx - dy * dy) / r2, noise).logPdf(g.e1) + Smooth.Normal(-f * 2.0 * dx * dy / r2, noise).logPdf(g.e2)
    }
  yield (x.value, y.value, m.value)

  /**
   * the EXACT posterior on a grid: a coarse pass over the whole sky finds
   * the mode, a fine 3-D grid around it gives the means and sds of x, y and
   * the mass, and the mass on the fine grid's edge says the window held it
   */
  final case class Grid(x: Double, y: Double, mass: Double, sdX: Double, sdY: Double, sdMass: Double, edge: Double)

  /** the highest point of a coarse search over the sky: every 50 units, 15 masses — where to START a sampler */
  def coarse(gs: Vector[Galaxy]): (Double, Double, Double) =
    val masses = Vector.tabulate(15)(i => 40 + 140.0 * i / 14)
    var best = (2100.0, 2100.0, 110.0, Double.NegativeInfinity)
    for i <- 0 to 84; j <- 0 to 84; m <- masses do
      // strictly inside the support: a start on its edge has no unconstrained value
      val (x, y, mm) = (math.min(math.max(i * 50.0, 1.0), 4199.0), math.min(math.max(j * 50.0, 1.0), 4199.0), math.min(math.max(m, 41.0), 179.0))
      val l = logLik(gs, x, y, mm)
      if l > best._4 then best = (x, y, mm, l)
    (best._1, best._2, best._3)

  def grid(gs: Vector[Galaxy]): Grid =
    val (cx, cy, _) = coarse(gs)
    val (n, half) = (60, 300.0)
    val xs = Vector.tabulate(n)(i => cx - half + 2 * half * i / (n - 1))
    val ys = Vector.tabulate(n)(j => cy - half + 2 * half * j / (n - 1))
    val ms = Vector.tabulate(36)(k => 40 + 140.0 * k / 35)
    val logs = for x <- xs; y <- ys; m <- ms yield (x, y, m, logLik(gs, x, y, m))
    val top = logs.map(_._4).max
    val w = logs.map(t => math.exp(t._4 - top))
    val z = w.sum
    def mean(f: ((Double, Double, Double, Double)) => Double) = logs.iterator.zip(w).map((t, wi) => f(t) * wi).sum / z
    val (mx, my, mm) = (mean(_._1), mean(_._2), mean(_._3))
    val edge = logs.iterator.zip(w).collect { case (t, wi) if math.abs(t._1 - cx) >= half - 1e-9 || math.abs(t._2 - cy) >= half - 1e-9 => wi }.sum / z
    Grid(mx, my, mm, math.sqrt(mean(t => (t._1 - mx) * (t._1 - mx))), math.sqrt(mean(t => (t._2 - my) * (t._2 - my))),
      math.sqrt(mean(t => (t._3 - mm) * (t._3 - mm))), edge)

class TestDarkWorlds extends Diagnosed:
  import DarkWorlds.*
  override val munitTimeout = scala.concurrent.duration.Duration(20, "min")

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  // the platform: Chunks in this JVM, the CSV read from the test resources
  given Bulk[Chunks] = Bulk.local(path => Source.fromResource(path).getLines())

  lazy val sky3 = galaxies[Chunks](3)
  lazy val exact = grid(sky(3))

  def agrees(xs: Vector[Double], mean: Double, sd: Double, what: String): Unit =
    val (m, s, ess) = (Summary.mean(xs), Summary.sd(xs), Summary.ess(xs))
    assert(math.abs(m - mean) < 4 * sd / math.sqrt(ess), f"$what: mean $m%.2f, grid $mean%.2f, ESS $ess%.0f")
    assert(math.abs(s - sd) < 0.15 * sd, f"$what: sd $s%.2f, grid $sd%.2f")

  test("the sky observed as a Bulk has the oracle's density: both model forms, at points across the sky") {
    val gs = sky(3)
    val t = Smooth.target(haloAd(sky3))
    val prior = math.log(1.0 / 140) + 2 * math.log(1.0 / 4200)
    for (x, y, m) <- Seq((2324.0, 1123.0, 145.0), (100.0, 4000.0, 50.0), (3000.0, 2000.0, 170.0)) do
      // the Grad form, in its unconstrained coordinates: its density less the Jacobian is prior + likelihood
      val u = Array(Support.Interval(40, 180).unconstrain(m), Support.Interval(0, 4200).unconstrain(x), Support.Interval(0, 4200).unconstrain(y))
      val logJ = Support.Interval(40, 180).constrain(u(0))._2 + Support.Interval(0, 4200).constrain(u(1))._2 + Support.Interval(0, 4200).constrain(u(2))._2
      val want = prior + logLik(gs, x, y, m)
      assertEqualsDouble(t.logp(u) - logJ, want, 1e-8 * math.abs(want))
      // the Model form: one forward run with the values fixed is the same sum
      assertEqualsDouble(Bayes.prior(observeBulk(sky3)(galaxyLogLik(_, x, y, m)), scala.util.Random(0)).logLik, logLik(gs, x, y, m), 1e-8 * math.abs(want))
  }

  test("Sky 3 by adaptive Metropolis and by AD NUTS, against the exact grid — and the true halo") {
    val g = exact
    val (tx, ty) = truth(3)
    assert(g.edge < 1e-4, s"the grid window must hold the posterior: edge weight ${g.edge}")
    report(f"Dark Worlds, Sky 3 (${sky(3).length} galaxies, observed as a Bulk): grid x ${g.x}%.1f ± ${g.sdX}%.1f, y ${g.y}%.1f ± ${g.sdY}%.1f, mass ${g.mass}%.1f ± ${g.sdMass}%.1f; the true halo ($tx%.1f, $ty%.1f)")
    val mh = adaptive(halo(sky3), samples = 8000, burn = 4000, chains = 2)
    // from a random start NUTS stays on whatever local hill it lands on (measured: x 3229, ESS 3); started at the coarse search's best point it does not
    val (sx, sy, sm) = coarse(sky(3))
    val nuts = Smooth.nuts(haloAd(sky3), samples = 1500, burn = 700, chains = 2, init = Map("x" -> sx, "y" -> sy, "mass" -> sm))
    for (by, post) <- Seq("adaptive" -> mh, "AD NUTS" -> nuts) do
      report(f"Sky 3 by $by: x ${Summary.mean(post.site("x"))}%.1f, y ${Summary.mean(post.site("y"))}%.1f, mass ${Summary.mean(post.site("mass"))}%.1f (ESS(x) ${Summary.ess(post.site("x"))}%.0f)")
      agrees(post.site("x"), g.x, g.sdX, s"$by x")
      agrees(post.site("y"), g.y, g.sdY, s"$by y")
      agrees(post.site("mass"), g.mass, g.sdMass, s"$by mass")
    // the simulation's truth: inside the posterior, a few sds at most
    val z = math.hypot((g.x - tx) / g.sdX, (g.y - ty) / g.sdY)
    report(f"Sky 3: the true halo is $z%.2f posterior sds from the posterior mean")
    assert(z < 3)
  }

  test("ten skies: the true halo against each posterior — inside the 95% region how often, how far") {
    val rows = (1 to 10).map { n =>
      val gs = galaxies[Chunks](n)
      val (sx, sy, sm) = coarse(sky(n))
      val post = Smooth.nuts(haloAd(gs), samples = 1000, burn = 500, chains = 1, seed = n.toLong, init = Map("x" -> sx, "y" -> sy, "mass" -> sm))
      val (xs, ys) = (post.site("x"), post.site("y"))
      val (mx, my) = (Summary.mean(xs), Summary.mean(ys))
      val (tx, ty) = truth(n)
      val dist = math.hypot(mx - tx, my - ty)
      // the 95% region by the draws' own distances from their mean
      val radius = Summary.quantile(xs.indices.map(i => math.hypot(xs(i) - mx, ys(i) - my)).toVector, 0.95)
      report(f"Sky $n%2d (${sky(n).length} galaxies): posterior mean ($mx%.0f, $my%.0f), truth ($tx%.0f, $ty%.0f), off by $dist%.0f; 95%% radius $radius%.0f${if dist <= radius then "" else "  — OUTSIDE"}")
      (dist, radius)
    }
    val inside = rows.count((d, r) => d <= r)
    report(f"ten skies: the truth inside the 95%% region in $inside of 10; median distance ${Summary.quantile(rows.map(_._1).toVector, 0.5)}%.0f")
    // at 95% each, 8 or more of 10 has probability 0.99: fewer would say the posterior is overconfident
    assert(inside >= 8, s"a calibrated posterior holds the truth in about 9.5 of 10 skies; got $inside")
  }
