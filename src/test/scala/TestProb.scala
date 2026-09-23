package okay

import Prob.*
import okay.RowLift.at

/**
 * specs/prob-effect-hansei.md: exact inference by multi-shot capture
 * (Kiselyov & Shan, "Embedded probabilistic programming", DSL 2009),
 * against hand-computed posteriors on two textbook models, plus
 * rejection sampling on the same programs and a quick timing
 * comparison against a hand-written enumerator (the price multi-shot
 * capture pays per branch, not a JMH lane).
 */
class TestProb extends munit.FunSuite:

  given rng: scala.util.Random = scala.util.Random(42)

  // ---------------------------------------------------------- wet grass

  /** the textbook Bayes net (Rain, Sprinkler independent priors;
   * WetGrass = Rain OR Sprinkler, no noise, so the posterior is exact
   * rational arithmetic — hand-computed below, not approximated) */
  def wetGrass: Boolean ! Dist =
    for
      rain <- dist(true -> 0.3, false -> 0.7)
      sprinkler <- dist(true -> 0.4, false -> 0.6)
      _ <- observe(rain || sprinkler)
    yield rain

  test("wet grass: P(Rain | WetGrass) by hand — 15/29, 14/29") {
    val post = !.run(runExact[Boolean, okay.Pure](wetGrass)).posterior
    assertEqualsDouble(post(true), 15.0 / 29.0, 1e-12)
    assertEqualsDouble(post(false), 14.0 / 29.0, 1e-12)
    assertEqualsDouble(post.values.sum, 1.0, 1e-12)
  }

  test("wet grass: the UNNORMALIZED joint before observe sums to less than 1 — mass was pruned") {
    val joint = !.run(runExact[Boolean, okay.Pure](wetGrass))
    assertEqualsDouble(joint.values.sum, 0.58, 1e-12, "0.3*0.4 + 0.3*0.6 + 0.7*0.4 survive; 0.7*0.6 (both false) was pruned")
  }

  test("wet grass: rejection sampling agrees with exact inference within statistical tolerance") {
    val sampled = runRejection(20000)(wetGrass)
    assert(sampled.nonEmpty, "every sample was rejected")
    assert(math.abs(sampled.getOrElse(true, 0.0) - 15.0 / 29.0) < 0.02,
      s"20000 samples should land within 2% of 15/29 ≈ 0.517: got ${sampled.getOrElse(true, 0.0)}")
  }

  test("guide: the pinned wet-grass example, verbatim") {
    val wetGrass: Boolean ! Dist =
      for
        rain <- dist(true -> 0.3, false -> 0.7)
        sprinkler <- dist(true -> 0.4, false -> 0.6)
        _ <- observe(rain || sprinkler)
      yield rain
    assertEqualsDouble(!.run(runExact[Boolean, okay.Pure](wetGrass)).posterior(true), 15.0 / 29.0, 1e-12)
  }

  // ---------------------------------------------------------- a small HMM

  enum Weather:
    case Sunny, Rainy
  enum Obs:
    case Happy, Sad

  def transition(s: Weather): Seq[(Weather, Double)] = s match
    case Weather.Sunny => Seq(Weather.Sunny -> 0.7, Weather.Rainy -> 0.3)
    case Weather.Rainy => Seq(Weather.Sunny -> 0.4, Weather.Rainy -> 0.6)

  def emission(s: Weather): Seq[(Obs, Double)] = s match
    case Weather.Sunny => Seq(Obs.Happy -> 0.8, Obs.Sad -> 0.2)
    case Weather.Rainy => Seq(Obs.Happy -> 0.3, Obs.Sad -> 0.7)

  /** two days, both observed Happy; the program answers day 2's state */
  def hmm: Weather ! Dist =
    for
      s1 <- dist(Weather.Sunny -> 0.6, Weather.Rainy -> 0.4)
      o1 <- dist(emission(s1)*)
      _ <- observe(o1 == Obs.Happy)
      s2 <- dist(transition(s1)*)
      o2 <- dist(emission(s2)*)
      _ <- observe(o2 == Obs.Happy)
    yield s2

  /** the SAME question, by hand: four explicit (s1, s2) branches, no
   * effect at all — the oracle AND the timing baseline */
  def handComputeHMM(): Map[Weather, Double] =
    val prior = Seq(Weather.Sunny -> 0.6, Weather.Rainy -> 0.4)
    var acc = Map.empty[Weather, Double]
    for
      (s1, p1) <- prior
      (o1, pe1) <- emission(s1) if o1 == Obs.Happy
      (s2, p2) <- transition(s1)
      (o2, pe2) <- emission(s2) if o2 == Obs.Happy
    do
      val w = p1 * pe1 * p2 * pe2
      acc = acc.updated(s2, acc.getOrElse(s2, 0.0) + w)
    val total = acc.values.sum
    acc.view.mapValues(_ / total).toMap

  test("small HMM: two days observed Happy, day-2 state — exact inference agrees with the hand enumerator") {
    val post = !.run(runExact[Weather, okay.Pure](hmm)).posterior
    val hand = handComputeHMM()
    assertEqualsDouble(post(Weather.Sunny), hand(Weather.Sunny), 1e-12)
    assertEqualsDouble(post(Weather.Rainy), hand(Weather.Rainy), 1e-12)
    // the numbers, so a reader need not run the test to see them
    assertEqualsDouble(post(Weather.Sunny), 0.3072 / 0.372, 1e-9)
    assertEqualsDouble(post(Weather.Rainy), 0.0648 / 0.372, 1e-9)
  }

  test("small HMM: rejection sampling agrees within statistical tolerance") {
    val sampled = runRejection(30000)(hmm)
    val hand = handComputeHMM()
    assert(math.abs(sampled.getOrElse(Weather.Sunny, 0.0) - hand(Weather.Sunny)) < 0.02)
  }

  // ---------------------------------------------------------- observe / uniform

  test("observe(false) prunes: an impossible branch contributes nothing") {
    val p: Int ! Dist = dist(1 -> 0.5, 2 -> 0.5).flatMap(x => observe(x == 3).map(_ => x))
    assertEquals(!.run(runExact[Int, okay.Pure](p)), Map.empty[Int, Double])
  }

  test("uniform: every alternative equally weighted") {
    val post = !.run(runExact[Int, okay.Pure](uniform(1, 2, 3, 4))).posterior
    for i <- 1 to 4 do assertEqualsDouble(post(i), 0.25, 1e-12)
  }

  // ---------------------------------------------------------- forwarding, and the price

  test("runExact forwards other effects: both branches' tells happen — multi-shot") {
    val p: Boolean ! (Dist + Writer % String) =
      dist(true -> 0.5, false -> 0.5).at[Dist + Writer % String].flatMap { b =>
        Writer.tell(if b then "heads" else "tails").at[Dist + Writer % String].map(_ => b)
      }
    val (told, post) = !.run(Writer.run[String, Map[Boolean, Double], okay.Pure](
      runExact[Boolean, Writer % String](p)))
    assertEquals(told.sorted, Seq("heads", "tails"), "both branches ran, so both tells happened")
    assertEqualsDouble(post.posterior(true), 0.5, 1e-12)
  }

  test("THE NUMBER: exact inference's price is one continuation-capture per branch") {
    // not a JMH lane (docs/benchmarks.md's own lane rules are for a
    // published comparison; this is a same-process order-of-magnitude
    // check) — the hand enumerator has NO effect machinery at all, so
    // the gap IS the price of multi-shot capture, per branch, on the
    // shipping runners
    (1 to 2000).foreach(_ => { (handComputeHMM(): Unit); (!.run(runExact[Weather, okay.Pure](hmm)): Unit) })
    val n = 20000
    val t0 = System.nanoTime()
    (1 to n).foreach(_ => handComputeHMM())
    val handNs = (System.nanoTime() - t0).toDouble / n
    val t1 = System.nanoTime()
    (1 to n).foreach(_ => !.run(runExact[Weather, okay.Pure](hmm)))
    val exactNs = (System.nanoTime() - t1).toDouble / n
    // print rather than assert a ratio: the point is the number exists
    // and is bounded, not a specific multiple that would flake on a
    // shared box (per-lane-gated-jmh's own lesson, at a smaller scale)
    println(f"okay-prob: hand enumerator ${handNs}%.0f ns/run, runExact ${exactNs}%.0f ns/run, ratio ${exactNs / handNs}%.1fx (4 branches: 2 states x 2 states)")
    assert(exactNs > 0 && handNs > 0)
    assert(exactNs < handNs * 200, s"runExact's per-branch capture cost blew up past 200x the hand enumerator: ${exactNs / handNs}x")
  }
