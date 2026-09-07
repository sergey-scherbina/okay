package okay.intent

/**
 * Combining weak labelers by their AGREEMENT, with no labelled data
 * (specs/intent-autonomy.md §2.2 and §4.3).
 *
 * Our offline tiers — hand-written cues, induced cues, the gram model
 * at one window or another, a word TF-IDF head — are labelling
 * functions in Snorkel's sense: each answers some messages and
 * abstains on the rest, at an accuracy nobody stated. The door
 * combines them as a CASCADE, taking the first that fires and
 * throwing away everything the others said. That is the weakest
 * combiner available: it cannot notice that three tiers agree, and it
 * cannot notice that the one that fired is the one usually wrong.
 *
 * This estimates each labeler's accuracy FROM AGREEMENT ALONE and
 * then votes with those weights. The estimator is the classical one
 * (Dawid & Skene 1979; the same idea Snorkel's generative model
 * scales up): start from the majority answer, score each labeler by
 * how often it matches the current consensus, re-weigh the consensus
 * by those scores, repeat. It converges in a handful of passes and
 * needs no gold labels — which is the point, because gold labels are
 * exactly what this programme is short of.
 *
 * DELIBERATELY SMALL AND EXPLICIT: no priors to tune, one convergence
 * rule, weights a caller can print and argue with. A labeler that
 * abstains contributes nothing (not a vote for "unknown"), and a
 * labeler that never agrees with anyone sinks to the floor weight
 * rather than being dropped, because a corpus that grows may prove it
 * right later.
 *
 * IT LIVES IN THE TEST SOURCES, AND THAT IS THE RESULT. Measured
 * against the shipped cascade over eight random splits
 * (`MeasureLabelModel`; specs/intent-classify.md, "Results —
 * intent-label-model"): 72.2% precision at 82.9% coverage against the
 * cascade's 77.5% at 89.6%, worst class F1 0.42 against 0.56, ahead
 * on one split of eight. So it does not ship — this is the apparatus
 * of a measurement, kept where measurements are kept. A later lane
 * with MANY independent labelers (this corpus offers four, one of
 * which dominates) can move it to `main` with a number beside it.
 */
object Agreement {

  /** what a labeler said about one message, or nothing */
  final case class Vote(labeler: String, intent: String, confidence: Double)

  /** the estimated accuracy of each labeler, by name */
  final case class Weights(byLabeler: Map[String, Double], passes: Int):
    def of(name: String): Double = byLabeler.getOrElse(name, floor)
    /** printable, because a weight nobody can read is a magic number */
    def show: String =
      byLabeler.toVector.sortBy(-_._2)
        .map((n, w) => f"$n%-18s $w%.3f").mkString("\n")

  /** a labeler this far below chance is still heard, quietly: the
   * corpus may prove it right later, and dropping it is a decision
   * this estimator has no evidence to make */
  val floor: Double = 0.05

  /**
   * Estimate each labeler's accuracy from how often it agrees with
   * the weighted consensus of the others, over UNLABELLED messages.
   *
   * `votes(i)` is what every labeler said about message `i`.
   */
  def estimate(votes: Seq[Vector[Vote]], passes: Int = 8): Weights =
    val names = votes.flatten.map(_.labeler).distinct
    if names.isEmpty then Weights(Map.empty, 0)
    else
      // pass 0: everyone equal, so the first consensus is a majority
      var w: Map[String, Double] = names.map(_ -> 1.0).toMap
      var pass = 0
      while pass < passes do
        // the consensus under the current weights
        val consensus: Seq[Option[String]] = votes.map(vs => best(vs, w))
        // each labeler's agreement with it, where BOTH spoke
        val next = names.map { n =>
          var seen = 0
          var hit = 0
          var i = 0
          while i < votes.length do
            val mine = votes(i).find(_.labeler == n)
            (mine, consensus(i)) match
              case (Some(v), Some(c)) => seen += 1; if v.intent == c then hit += 1
              case _ => ()
            i += 1
          val acc = if seen == 0 then floor else hit.toDouble / seen
          n -> math.max(floor, acc)
        }.toMap
        // stop early when nothing moves: the estimate is a fixed point
        val moved = names.map(n => math.abs(next(n) - w(n))).maxOption.getOrElse(0.0)
        w = next
        pass += 1
        if moved < 1e-6 then pass = passes
      Weights(w, pass)

  /** the weighted vote: each labeler's confidence times its estimated
   * accuracy, summed per intent; `None` when everyone abstained */
  def best(vs: Vector[Vote], weights: Map[String, Double]): Option[String] =
    if vs.isEmpty then None
    else
      val scores = scala.collection.mutable.Map.empty[String, Double]
      for v <- vs do
        val w = weights.getOrElse(v.labeler, floor)
        scores(v.intent) = scores.getOrElse(v.intent, 0.0) + w * v.confidence
      scores.toVector.sortBy(-_._2).headOption.map(_._1)

  /** the decision AND its margin, for a caller that wants to abstain
   * when the top two are close — the same knob every tier here has */
  def decide(vs: Vector[Vote], w: Weights): Option[(String, Double)] =
    if vs.isEmpty then None
    else
      val scores = scala.collection.mutable.Map.empty[String, Double]
      for v <- vs do scores(v.intent) = scores.getOrElse(v.intent, 0.0) + w.of(v.labeler) * v.confidence
      val ranked = scores.toVector.sortBy(-_._2)
      val total = ranked.map(_._2).sum
      ranked.headOption.map { (intent, top) =>
        val second = ranked.lift(1).map(_._2).getOrElse(0.0)
        (intent, if total <= 0.0 then 0.0 else (top - second) / total)
      }
}
