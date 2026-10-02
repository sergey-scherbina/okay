package okay.bayes

import scala.util.Random
import okay.testkit.Munit.Diagnosed
import Bayes.*
import Distribution.*

/** specs/okay-bayes.md stage 5b: Bayesian Methods for Hackers ch.5 — loss functions and the Bayes action */
class TestDecision extends Diagnosed:

  def report(line: String): Unit =
    note(line); println(s"  okay-bayes | $line")

  test("the known answers: squared loss is the mean, absolute the median, pinball(τ) the τ-quantile") {
    val rng = Random(5)
    val draws = Vector.fill(2001)(Gamma(2, 0.5).sample(rng))
    val sorted = draws.sorted
    assertEqualsDouble(Decision.action(draws, 0, 30)(Loss.squared), draws.sum / draws.length, 1e-6)
    assertEqualsDouble(Decision.action(draws, 0, 30)(Loss.absolute), sorted(1000), 1e-6)
    // pinball's expected loss is flat between two order statistics: the action lies between them, and loses nothing to either
    val q = Decision.action(draws, 0, 30)(Loss.pinball(0.9))
    val k = (0.9 * draws.length).toInt
    assert(q >= sorted(k - 1) - 1e-6 && q <= sorted(k) + 1e-6, s"$q outside [${sorted(k - 1)}, ${sorted(k)}]")
    assert(Decision.expectedLoss(draws, q)(Loss.pinball(0.9)) <= Decision.expectedLoss(draws, sorted(k - 1))(Loss.pinball(0.9)) + 1e-9)
  }

  // The Price is Right, the book's numbers: a prior on the showcase's price from past shows, and a guess built from two prizes
  val showcase = for
    truth <- sample("true_price", Normal(35000, 7500))
    snowblower <- sample("prize_1", Normal(3000, 500))
    trip <- sample("prize_2", Normal(12000, 3000))
    _ <- observe(Normal(snowblower + trip, 3000), truth)
  yield truth

  /** linear-Gaussian, so exact: the prizes integrate out to N(15000, 500² + 3000² + 3000²) on the price */
  val (exactMean, exactSd) =
    val (p0, v0) = (35000.0, 7500.0 * 7500)
    val (p1, v1) = (15000.0, 500.0 * 500 + 2 * 3000.0 * 3000)
    val prec = 1 / v0 + 1 / v1
    ((p0 / v0 + p1 / v1) / prec, math.sqrt(1 / prec))

  lazy val post = adaptive(showcase, samples = 20000, burn = 5000, chains = 2)

  test("The Price is Right: the posterior of the true price against its closed form") {
    val xs = post.draws
    val (m, s, ess) = (Summary.mean(xs), Summary.sd(xs), Summary.ess(xs))
    report(f"The Price is Right: true price ${m}%.0f ± ${s}%.0f (exact ${exactMean}%.0f ± ${exactSd}%.0f), ESS $ess%.0f of ${xs.length}")
    assert(math.abs(m - exactMean) < 4 * exactSd / math.sqrt(ess))
    assert(math.abs(s - exactSd) < 0.1 * exactSd)
  }

  /** the book's showdown loss: overbid and lose the `risk`; within 250 under and win both showcases; else the distance */
  def showdown(risk: Double)(truth: Double, guess: Double): Double =
    if truth < guess then risk
    else if truth - guess <= 250 then -2 * truth
    else math.abs(guess - truth)

  test("the showdown loss: the best bid falls as the risk of overbidding grows, and stays under the posterior mean") {
    val draws = post.draws
    val mean = Summary.mean(draws)
    val bids = Seq(30000.0, 60000.0, 90000.0, 120000.0, 150000.0).map(r => r -> Decision.action(draws, 5000, 40000)(showdown(r)))
    report(bids.map((r, b) => f"risk $r%.0f → bid $b%.0f").mkString("showdown: ", ", ", f" (posterior mean $mean%.0f)"))
    assert(bids.map(_._2).zip(bids.map(_._2).tail).forall((a, b) => b <= a + 1e-6), "a larger risk never raises the bid")
    assert(bids.forall(_._2 < mean))
  }
