package okay.dlm

/**
 * DOES OUR OWN NUMBER MEAN ANYTHING (specs/dlm.md, "Calibration").
 *
 * The router answers with a score and the caller decides by it. This
 * asks the only question that matters about such a number: **when it
 * said 0.9, how often was it right?**
 *
 * PER LAYER, AND NEVER ACROSS THEM. A rule matching is 1.0 because a
 * rule matched, not because we are probably right; compared anyway,
 * the first measurement over a live journal found the service
 * corrected thirty percent of the time at 1.0, against six percent
 * where it said `Unclear`. So a Brier score exists only where a
 * probability exists, and a rule owes a different number: its own
 * correction rate, per intent, with the people and the distinct
 * sentences beside it — because one person repeating one message is
 * one thing wrong and not twenty-six.
 *
 * The arithmetic is here; reading a journal into `Seen` rows, and
 * deciding what "wrong" means there, is the caller's.
 */
object Calibration:

  /** one decision, and whether the person went on to correct it */
  final case class Seen(layer: Layer, intent: String, score: Float, wrong: Boolean,
                        who: String = "", text: String = "")

  /** a decision as the journal recorded it, read into a row: a
   * `Fires` under its layer, an `Unclear` as the semantic layer's own
   * abstention, a `Missing` as nothing to score */
  def seen(route: Route, wrong: Boolean, who: String = "", text: String = ""): Option[Seen] = route match
    case Route.Fires(name, _, s) => Some(Seen(s.layer, name, s.score, wrong, who, text))
    case Route.Unclear(_, sc) => Some(Seen(Layer.Semantic, "unclear", sc, wrong, who, text))
    case Route.Missing(_, _) => None

  /** the reliability of one band: how often it was wrong, against
   * what it claimed */
  final case class Band(lo: Double, hi: Double, n: Int, wrong: Int):
    def rate: Double = if n == 0 then 0 else wrong * 100.0 / n

  val defaultCuts: Vector[(Double, Double)] =
    Vector(0.0 -> 0.3, 0.3 -> 0.45, 0.45 -> 0.6, 0.6 -> 0.8, 0.8 -> 1.01)

  def bands(rows: Vector[Seen], cuts: Vector[(Double, Double)] = defaultCuts): Vector[Band] =
    cuts.map((lo, hi) =>
      val in = rows.filter(r => r.score >= lo && r.score < hi)
      Band(lo, hi, in.length, in.count(_.wrong)))

  /**
   * THE BRIER SCORE — mean squared error of a probability against
   * what happened, the proper scoring rule a calibrated model is
   * trained on. Lower is better; 0.25 is what «0.5 to everything»
   * scores. ONLY WHERE A PROBABILITY EXISTS: a rule's 1.0 is not one,
   * so scoring it would be arithmetic about nothing — `None` says
   * that instead of printing a number.
   */
  def brier(rows: Vector[Seen]): Option[Double] =
    val real = rows.filter(_.layer == Layer.Semantic)
    Option.when(real.nonEmpty)(
      real.map(r => { val p = r.score.toDouble; val y = if r.wrong then 0.0 else 1.0; (p - y) * (p - y) }).sum / real.length)

  /** one layer's line: turns, corrected */
  final case class Reliability(layer: Layer, n: Int, wrong: Int):
    def rate: Double = if n == 0 then 0 else wrong * 100.0 / n

  def byLayer(rows: Vector[Seen]): Vector[Reliability] =
    Layer.values.toVector.map(l =>
      val in = rows.filter(_.layer == l)
      Reliability(l, in.length, in.count(_.wrong))).filter(_.n > 0)

  /** a rule's own number: fired, corrected, and how many PEOPLE and
   * distinct SENTENCES the corrections are — worst first */
  final case class PerIntent(intent: String, fired: Int, wrong: Int, people: Int, sentences: Int):
    def rate: Double = if fired == 0 then 0 else wrong * 100.0 / fired

  def perIntent(rows: Vector[Seen], layer: Layer = Layer.Rule, minFired: Int = 10): Vector[PerIntent] =
    rows.filter(_.layer == layer).groupBy(_.intent).toVector
      .map((name, in) =>
        val bad = in.filter(_.wrong)
        PerIntent(name, in.length, bad.length, bad.map(_.who).distinct.length,
          bad.map(_.text.trim.toLowerCase).distinct.length))
      .filter(_.fired >= minFired).sortBy(x => -x.rate)
