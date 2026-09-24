package okay2

import Prob._

/** a hidden Markov model's two states and two observations, at top
 * level: a sealed family nested in a suite trips -Xlint's outer check */
object Hmm {
  sealed trait Weather
  case object Sunny extends Weather
  case object Rainy extends Weather
  sealed trait Obs
  case object Happy extends Obs
  case object Sad extends Obs
}

/**
 * Exact inference by multi-shot capture (Kiselyov & Shan, "Embedded
 * probabilistic programming", DSL 2009), against hand-computed
 * posteriors on two textbook models, plus rejection sampling on the
 * same programs — the Scala 3 core's TestProb.
 */
class TestProb extends munit.FunSuite {
  import Hmm._

  implicit val rng: scala.util.Random = new scala.util.Random(42)

  /** the textbook Bayes net: Rain, Sprinkler; WetGrass = Rain OR Sprinkler */
  def wetGrass: Boolean ! Dist =
    for {
      rain <- dist(true -> 0.3, false -> 0.7)
      sprinkler <- dist(true -> 0.4, false -> 0.6)
      _ <- observe(rain || sprinkler)
    } yield rain

  test("wet grass: P(Rain | WetGrass) by hand — 15/29, 14/29") {
    val post = !.run(runExact(wetGrass)).posterior
    assertEqualsDouble(post(true), 15.0 / 29.0, 1e-12)
    assertEqualsDouble(post(false), 14.0 / 29.0, 1e-12)
    assertEqualsDouble(post.values.sum, 1.0, 1e-12)
  }

  test("wet grass: the UNNORMALIZED joint before observe sums to less than 1 — mass was pruned") {
    val joint = !.run(runExact(wetGrass))
    assertEqualsDouble(joint.values.sum, 0.58, 1e-12)
  }

  test("wet grass: rejection sampling agrees with exact inference within statistical tolerance") {
    val sampled = runRejection(20000)(wetGrass)
    assert(sampled.nonEmpty, "every sample was rejected")
    assert(math.abs(sampled.getOrElse(true, 0.0) - 15.0 / 29.0) < 0.02, s"got ${sampled.getOrElse(true, 0.0)}")
  }

  def transition(s: Weather): Seq[(Weather, Double)] = s match {
    case Sunny => Seq(Sunny -> 0.7, Rainy -> 0.3)
    case Rainy => Seq(Sunny -> 0.4, Rainy -> 0.6)
  }

  def emission(s: Weather): Seq[(Obs, Double)] = s match {
    case Sunny => Seq(Happy -> 0.8, Sad -> 0.2)
    case Rainy => Seq(Happy -> 0.3, Sad -> 0.7)
  }

  /** two days, both observed Happy; the program answers day 2's state */
  def hmm: Weather ! Dist =
    for {
      s1 <- dist[Weather](Sunny -> 0.6, Rainy -> 0.4)
      o1 <- dist(emission(s1): _*)
      _ <- observe(o1 == Happy)
      s2 <- dist(transition(s1): _*)
      o2 <- dist(emission(s2): _*)
      _ <- observe(o2 == Happy)
    } yield s2

  /** the SAME question by hand: four explicit (s1, s2) branches */
  def handComputeHMM(): Map[Weather, Double] = {
    val prior = Seq[(Weather, Double)](Sunny -> 0.6, Rainy -> 0.4)
    var acc = Map.empty[Weather, Double]
    for {
      (s1, p1) <- prior
      (o1, pe1) <- emission(s1) if o1 == Happy
      (s2, p2) <- transition(s1)
      (o2, pe2) <- emission(s2) if o2 == Happy
    } {
      val w = p1 * pe1 * p2 * pe2
      acc = acc.updated(s2, acc.getOrElse(s2, 0.0) + w)
    }
    val total = acc.values.sum
    acc.map { case (s, w) => s -> w / total }
  }

  test("small HMM: two days observed Happy, day-2 state — exact inference agrees with the hand enumerator") {
    val post = !.run(runExact(hmm)).posterior
    val hand = handComputeHMM()
    assertEqualsDouble(post(Sunny), hand(Sunny), 1e-12)
    assertEqualsDouble(post(Rainy), hand(Rainy), 1e-12)
    assertEqualsDouble(post(Sunny), 0.3072 / 0.372, 1e-9)
    assertEqualsDouble(post(Rainy), 0.0648 / 0.372, 1e-9)
  }

  test("small HMM: rejection sampling agrees within statistical tolerance") {
    val sampled = runRejection(30000)(hmm)
    assert(math.abs(sampled.getOrElse(Sunny, 0.0) - handComputeHMM()(Sunny)) < 0.02)
  }

  test("observe(false) prunes: an impossible branch contributes nothing") {
    val p: Int ! Dist = dist(1 -> 0.5, 2 -> 0.5).flatMap(x => observe(x == 3).map(_ => x))
    assertEquals(!.run(runExact(p)), Map.empty[Int, Double])
  }

  test("uniform: every alternative equally weighted") {
    val post = !.run(runExact(uniform(1, 2, 3, 4))).posterior
    for (i <- 1 to 4) assertEqualsDouble(post(i), 0.25, 1e-12)
  }

  test("runExact forwards other effects: both branches' tells happen — multi-shot") {
    val p: Boolean ! (Dist + Writer[String]) =
      dist(true -> 0.5, false -> 0.5).flatMap(b => Writer.tell(if (b) "heads" else "tails").map(_ => b))
    val (told, post) = !.run(Writer.run(runExact(p)))
    assertEquals(told.sorted, Seq("heads", "tails"), "both branches ran, so both tells happened")
    assertEqualsDouble(post.posterior(true), 0.5, 1e-12)
  }

  test("a deep model: 12 coin flips, exact inference enumerates 4096 branches") {
    def flips(n: Int): Int ! Dist =
      if (n == 0) pure[Dist, Int](0) else uniform(0, 1).flatMap(b => flips(n - 1).map(_ + b))
    val post = !.run(runExact(flips(12))).posterior
    assertEqualsDouble(post(6), 924.0 / 4096.0, 1e-12)
  }
}
