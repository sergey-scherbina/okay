package okay.agent

import okay.rag.Embedding

/**
 * A linear probe over frozen embeddings (specs/intent-classify.md).
 *
 * What the backlog originally proposed, and the one tier here that
 * LEARNS: a centroid and a kNN both take the embedding space as given,
 * while a probe moves the separating plane. The reference's fine-tuned
 * encoder adapts the representation itself and wants 1k-5k labelled
 * examples per class; this adapts only the decision and wants tens,
 * because it fits `classes x dim` weights instead of a hundred million.
 *
 * Multinomial logistic regression by plain gradient descent — no
 * dependency, no native library, no training pipeline, and it runs on
 * every platform. At 1024 dimensions and four classes that is 4096
 * weights, 16KB, and a fit measured in milliseconds.
 *
 * Deterministic: weights start at zero and the data is walked in
 * order, so two runs give the same model and a test can assert on it.
 */
object Probe {

  final case class Trained(classes: Vector[String], w: Array[Array[Double]], b: Array[Double])

  final case class Verdict(best: String, probability: Double,
                           margin: Double, runnerUp: Option[String])

  private def softmax(z: Array[Double]): Array[Double] =
    val m = z.max
    val e = z.map(x => math.exp(x - m))
    val s = e.sum
    if s == 0.0 then e else e.map(_ / s)

  private def logits(t: Trained, v: Embedding): Array[Double] =
    val u = Centroid.normalise(v)
    Array.tabulate(t.classes.length) { c =>
      var s = t.b(c)
      var i = 0
      val row = t.w(c)
      val n = math.min(row.length, u.length)
      while i < n do
        s += row(i) * u(i)
        i += 1
      s
    }

  /**
   * Fit by gradient descent on the cross-entropy.
   *
   * `epochs` and `rate` are the caller's, and their defaults are the
   * ones the bake-off measured with — stated rather than tuned per
   * fixture, since a probe tuned against the test half would be
   * measuring nothing.
   */
  def train(labelled: Seq[(Embedding, String)], epochs: Int = 300,
            rate: Double = 0.5): Trained =
    val classes = labelled.map(_._2).distinct.sorted.toVector
    val dim = labelled.headOption.map(_._1.length).getOrElse(0)
    val w = Array.fill(classes.length)(Array.fill(dim)(0.0))
    val b = Array.fill(classes.length)(0.0)
    val rows = labelled.map((v, c) => (Centroid.normalise(v), classes.indexOf(c))).toVector
    val model = Trained(classes, w, b)
    var epoch = 0
    while epoch < epochs do
      for (u, target) <- rows do
        val p = softmax(logits(model, u))
        var c = 0
        while c < classes.length do
          val err = p(c) - (if c == target then 1.0 else 0.0)
          val row = w(c)
          var i = 0
          while i < dim do
            row(i) -= rate * err * u(i)
            i += 1
          b(c) -= rate * err
          c += 1
      epoch += 1
    model

  /**
   * Every class with its probability, best first — the distribution
   * the verdict is drawn FROM.
   *
   * `score` answers what to do; this answers what was considered, and
   * the two are not the same question. A diagnostic that lists every
   * class, and an uncertainty-sampled choice of what to label next,
   * both need the whole ranking; without it a caller re-implements
   * this softmax outside, against internals it should not have to
   * know. Asked for by a consumer wiring the probe into a router, on
   * the day the seam was filed.
   */
  def ranked(t: Trained, v: Embedding): Vector[(String, Double)] =
    if t.classes.isEmpty then Vector.empty
    else
      val p = softmax(logits(t, v))
      t.classes.indices.map(i => (t.classes(i), p(i))).sortBy(-_._2).toVector

  /** the margin is the gap between the top two PROBABILITIES, which is
   * already on a fixed scale — unlike a logit gap */
  def score(t: Trained, v: Embedding): Option[Verdict] =
    ranked(t, v) match
      case Vector() => None
      case r =>
        val (best, p0) = r.head
        Some(Verdict(best, p0, p0 - r.lift(1).map(_._2).getOrElse(0.0),
          r.lift(1).map(_._1)))

  def classify(t: Trained, v: Embedding, floor: Double = 0.3): Option[String] =
    score(t, v).filter(_.margin >= floor).map(_.best)
}
