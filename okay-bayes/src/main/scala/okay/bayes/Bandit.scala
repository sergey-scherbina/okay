package okay.bayes

import scala.util.Random
import Distribution.Beta

/**
 * THOMPSON SAMPLING for Bernoulli arms (Thompson 1933; *Bayesian Methods
 * for Hackers* ch.6; Agrawal & Goyal, COLT 2012). Each arm's unknown
 * success rate has a Beta posterior; `choose` draws ONE value from every
 * arm's posterior and pulls the arm with the largest — so an arm is pulled
 * with exactly the probability that it is the best, exploring while that
 * is uncertain and exploiting once it is not. A bandit is a value:
 * `observe` answers the updated one, and a run is a fold over pulls.
 */
final case class Bandit(wins: Vector[Int], trials: Vector[Int], prior: (Double, Double) = (1.0, 1.0)):
  require(wins.length == trials.length && wins.nonEmpty, "Bandit: one wins and one trials count per arm")

  def arms: Int = wins.length

  /** arm i's posterior: Beta(a + wins, b + losses) */
  def posterior(arm: Int): Beta = Beta(prior._1 + wins(arm), prior._2 + trials(arm) - wins(arm))

  /** draw from every arm's posterior, pull the largest */
  def choose(rng: Random): Int =
    val draws = Vector.tabulate(arms)(posterior(_).sample(rng))
    draws.indices.maxBy(draws)

  /** the conjugate update after pulling `arm` */
  def observe(arm: Int, won: Boolean): Bandit =
    Bandit(if won then wins.updated(arm, wins(arm) + 1) else wins, trials.updated(arm, trials(arm) + 1), prior)

  /** each arm's posterior mean */
  def means: Vector[Double] = Vector.tabulate(arms) { i => val b = posterior(i); b.a / (b.a + b.b) }

object Bandit:
  /** `arms` arms with no pulls, under a uniform Beta(1, 1) prior */
  def apply(arms: Int): Bandit = Bandit(Vector.fill(arms)(0), Vector.fill(arms)(0))

  /**
   * PLAY `pulls` rounds against `pull` (the world: an arm's reward), each
   * round Thompson's choice: answers the final bandit and the arms pulled,
   * in order.
   */
  def play(start: Bandit, pulls: Int, rng: Random)(pull: Int => Boolean): (Bandit, Vector[Int]) =
    var b = start
    val chosen = Vector.newBuilder[Int]
    var i = 0
    while i < pulls do
      val arm = b.choose(rng)
      b = b.observe(arm, pull(arm))
      chosen += arm
      i += 1
    (b, chosen.result())
