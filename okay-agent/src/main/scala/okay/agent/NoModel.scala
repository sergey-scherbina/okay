package okay.agent

import okay.rag.Embedding

/**
 * A classifier with no generation on the request path
 * (specs/intent-classify.md).
 *
 * This is the assembly the tier bake-off argued for, rather than
 * another table: the tiers that measured well, composed, with a
 * calibrated point at which the thing declines to answer.
 *
 * Two ideas do the work.
 *
 * STACKING. Each tier's opinion is a feature, not a vote: the pattern
 * cue that fired, the probe's probability for each class. Choosing one
 * tier throws away what the others knew, and a vote throws away how
 * sure they were. What is stacked here is deliberately shallow — a
 * weighted blend with one fitted weight — because with sixty training
 * examples a second-level model has less data than the first level
 * did.
 *
 * CONFORMAL ABSTENTION. A threshold picked on a CALIBRATION half, so
 * "I answer 70% of messages and I am right 95% of the time when I do"
 * is a measured promise rather than a hope. Without it an abstention
 * threshold is a number someone liked the look of.
 */
object NoModel {

  /**
   * `promise` is `None` when the calibration sample cannot SUPPORT a
   * bound at the requested error rate, which is a different statement
   * from a low bound and the one that was missing.
   *
   * A split-conformal threshold at error rate `alpha` needs at least
   * `(1 - alpha) / alpha` calibration MISTAKES to exist at all — 19 of
   * them at 95%. With fewer, the empirical accuracy above the
   * threshold is a description of the sample and not a prediction, and
   * quoting it is the exact overclaim this field exists to prevent.
   * Measured: the first version reported "96.2% over 65%" and
   * delivered 88.9% over 45% on unseen data.
   */
  final case class Trained(probe: Probe.Trained,
                           cues: Vector[Patterns.Cue],
                           patternWeight: Double,
                           threshold: Double,
                           promise: Option[Double],
                           observedCoverage: Double,
                           calibrationErrors: Int,
                           errorsNeeded: Int)

  /**
   * What the classifier concluded — INCLUDING what it nearly concluded
   * instead.
   *
   * `runnerUp` and the ranking are computed anyway, one layer down, and
   * the first version of this type discarded them: an abstention then
   * told a caller only THAT it declined. Two consumers need the rest.
   * An interface that abstains has to show a person the two candidates
   * it could not separate, and active learning selects the next
   * examples to label by uncertainty, which is a property of the
   * DISTRIBUTION and not of the winner.
   */
  final case class Verdict(best: String, confidence: Double, fromPattern: Boolean,
                           runnerUp: Option[String] = None,
                           ranked: Seq[(String, Double)] = Seq.empty)

  /**
   * Blend a pattern verdict into the probe's distribution.
   *
   * A cue that fires adds its weight to that class's score; everything
   * else is the probe. `patternWeight` is how much a cue is worth in
   * units of probability, and it is fitted rather than chosen — see
   * `fit`.
   */
  private def blend(t: Probe.Trained, cues: Vector[Patterns.Cue], w: Double,
                    text: String, v: Embedding): Seq[(String, Double)] =
    val base = Probe.ranked(t, v)
    val cue = Patterns.score(cues, text)
    base.map { (c, p) =>
      val bonus = cue.filter(_.best == c).map(_.margin * w).getOrElse(0.0)
      c -> (p + bonus)
    }.sortBy(-_._2)

  /**
   * Fit on a training half and CALIBRATE on a held-out half.
   *
   * The two halves do different jobs and must not be the same data:
   * the first fits the probe, the second chooses the pattern weight
   * and the abstention threshold. A threshold chosen on the data the
   * model was fitted to promises an accuracy nobody will see.
   */
  def fit(train: Seq[(String, Embedding, String)],
          calibrate: Seq[(String, Embedding, String)],
          cues: Vector[Patterns.Cue] = Patterns.meeting,
          targetAccuracy: Double = 0.95,
          weights: Seq[Double] = Seq(0.0)): Trained =
    val probe = Probe.train(train.map((_, v, c) => (v, c)))

    // The pattern weight, chosen on the calibration half. The DEFAULT
    // GRID IS A SINGLE ZERO, which is a measured decision and not
    // timidity: fitted over six candidates on forty calibration rows,
    // the search picked 0.8 and cost five points on held-out data (70%
    // against the probe's own 75%). Forty rows cannot support choosing
    // even one number. A caller with a larger corpus passes a real
    // grid, and `intent-ensemble-weights` is the lane that will.
    val best = weights.maxBy { w =>
      calibrate.count { (text, v, gold) =>
        blend(probe, cues, w, text, v).headOption.exists(_._1 == gold)
      }
    }

    val scoredCal = calibrate.map { (text, v, gold) =>
      val ranked = blend(probe, cues, best, text, v)
      val top = ranked.headOption
      val conf = top.map(_._2 - ranked.lift(1).map(_._2).getOrElse(0.0)).getOrElse(0.0)
      (conf, top.exists(_._1 == gold))
    }

    /*
     * The threshold, as a CONFORMAL quantile rather than as the point
     * where the calibration sample happened to look good.
     *
     * The first version of this picked the lowest confidence at which
     * calibration accuracy still met the target, and it did not hold:
     * it promised 96.2% over 65% of messages and delivered 88.9% over
     * 45% on data it had not seen. That is the classic failure —
     * choosing a threshold ON a sample and then quoting that sample's
     * accuracy as a prediction about the next one.
     *
     * The fix is the standard split-conformal construction. Look only
     * at the calibration examples the model got WRONG, and put the
     * threshold above all but an `alpha` fraction of their
     * confidences, using the finite-sample rank
     * `ceil((1 - alpha) * (m + 1))` rather than an empirical
     * percentile. With few errors that rank runs off the end of the
     * list, and then the honest answer is the maximum confidence seen
     * — refuse everything rather than promise from three data points.
     */
    val alpha = 1.0 - targetAccuracy
    val wrong = scoredCal.filter(!_._2).map(_._1).sorted(using Ordering[Double].reverse)
    val m = wrong.length
    // rounded before the ceiling: at targetAccuracy 0.8 the subtraction
    // leaves alpha at 0.19999999999999996, the quotient at
    // 4.000000000000001, and an unrounded ceiling demands a fifth error
    // that the arithmetic invented
    val needed = math.ceil(math.round((1.0 - alpha) / alpha * 1e9) / 1e9).toInt
    val rank = math.ceil((1.0 - alpha) * (m + 1)).toInt
    val threshold =
      if m == 0 then 0.0
      else if rank > m then wrong.head + 1e-9
      else wrong(rank - 1)

    val kept = scoredCal.filter(_._1 >= threshold)
    // the promise exists only when the sample could carry it
    val promise = if m >= needed then Some(targetAccuracy) else None
    Trained(probe, cues, best, threshold, promise,
      kept.length.toDouble / scoredCal.length, m, needed)

  /** the whole classifier: an answer, or `None` meaning "ask a person" */
  def classify(t: Trained, text: String, v: Embedding): Option[Verdict] =
    decide(t, text, v).answer

  /**
   * The full decision, whether or not it clears the threshold.
   *
   * `classify` answers or declines; this says what the declining LOOKED
   * like, which is what a person is shown and what an example-selector
   * ranks on. Returning both from one call keeps them from disagreeing.
   */
  final case class Decision(answer: Option[Verdict], considered: Verdict)

  def decide(t: Trained, text: String, v: Embedding): Decision =
    val ranked = blend(t.probe, t.cues, t.patternWeight, text, v)
    val (cls, s0) = ranked.headOption.getOrElse(("", 0.0))
    val conf = s0 - ranked.lift(1).map(_._2).getOrElse(0.0)
    val full = Verdict(cls, conf, Patterns.score(t.cues, text).exists(_.best == cls),
      ranked.lift(1).map(_._1), ranked)
    Decision(if ranked.nonEmpty && conf >= t.threshold then Some(full) else None, full)

  /** what it answers when it must answer — for measuring the tier at
   * full coverage beside the abstaining one */
  def force(t: Trained, text: String, v: Embedding): Option[String] =
    blend(t.probe, t.cues, t.patternWeight, text, v).headOption.map(_._1)
}
