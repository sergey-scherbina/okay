package okay.intent

import okay.rag.Embedding

/**
 * The vector first pass (specs/intent-classify.md).
 *
 * The symbolic tier plateaued at 60-64% agreement no matter how tight
 * its margin, and BM25 matches WORDS — so the question this tier
 * exists to answer is whether the binding constraint was the
 * REPRESENTATION. If it was, the agreement curve should rise with the
 * margin where the symbolic one flat-lined.
 *
 * A class centroid is the mean of its examples' embeddings, which is
 * the cheapest classifier that uses meaning rather than spelling: no
 * training loop, no dependency, 4KB per class at 1024 dimensions, and
 * a handful of dot products per message. The reference's fine-tuned
 * encoder wants 1k-5k labelled examples per class; this wants tens.
 *
 * Embedding is the caller's job — this file never calls a gateway, so
 * it stays testable and stays on every platform.
 *
 * Named `Centroid` rather than `Vectors` because `okay.rag.Vectors`
 * already exists and callers import both packages: the newcomer gives
 * way. It also reads better beside `Symbolic`, since the two tiers now
 * share a shape — `train`, `score`, `classify`, and a `Trained`.
 */
object Centroid {

  /**
   * The class means, and THE TAXONOMY THEY WERE FITTED AGAINST.
   *
   * A tier used to infer its classes from whatever labels its rows
   * happened to carry, so a caller checked agreement by hand
   * afterwards or not at all. `train` still infers — it is the
   * ordinary case — but the answer is recorded, and `against`
   * DECLARES a taxonomy and refuses rows that do not fit it.
   */
  final case class Trained(byClass: Map[String, Embedding], taxon: Taxon):
    /** classes the taxonomy declares that this tier can never produce
     * — none when the taxonomy was inferred, and the interesting case
     * when it was declared */
    def silent: Vector[String] = taxon.classes.filterNot(byClass.contains)

  final case class Verdict(best: String, similarity: Double,
                           margin: Double, runnerUp: Option[String])

  /** unit length, so a dot product IS cosine similarity */
  def normalise(v: Embedding): Embedding =
    var sum = 0.0
    var i = 0
    while i < v.length do
      sum += v(i).toDouble * v(i).toDouble
      i += 1
    val n = math.sqrt(sum)
    if n == 0.0 then v else v.map(x => (x / n).toFloat)

  def dot(a: Embedding, b: Embedding): Double =
    var s = 0.0
    var i = 0
    val n = math.min(a.length, b.length)
    while i < n do
      s += a(i).toDouble * b(i).toDouble
      i += 1
    s

  /**
   * The mean of each class's examples, normalised.
   *
   * Normalising the MEAN rather than averaging normalised vectors is
   * the usual choice and the wrong one when example lengths vary
   * wildly — so each example is normalised first and the mean of those
   * is normalised again, which makes a long example count the same as
   * a short one.
   */
  def train(labelled: Seq[(Embedding, String)]): Trained =
    val byClass = labelled.groupBy(_._2).map { (cls, rows) =>
      val dim = rows.head._1.length
      val acc = Array.fill(dim)(0.0f)
      for (v, _) <- rows do
        val u = normalise(v)
        var i = 0
        while i < dim do
          acc(i) += u(i)
          i += 1
      cls -> normalise(okay.rag.embedding(acc))
    }
    Trained(byClass, Taxon.parsed(byClass.keys.toVector.sorted))

  /** the same fit, against a taxonomy the caller DECLARES — a label
   * outside it is an error here rather than an invented class later */
  def against(taxon: Taxon, labelled: Seq[(Embedding, String)])
  : Either[String, Trained] =
    taxon.check(labelled.map(_._2)).map(_ => train(labelled).copy(taxon = taxon))

  /**
   * Nearest centroid, with the gap to the runner-up as the margin.
   *
   * The margin is a DIFFERENCE of cosines rather than a ratio: cosines
   * against normalised centroids already live on a fixed scale, and a
   * ratio would exaggerate the gap wherever the absolute similarity is
   * small — which is exactly where the tier should be least sure.
   */
  def score(c: Trained, v: Embedding): Option[Verdict] =
    if c.byClass.isEmpty then None
    else
      val u = normalise(v)
      val ranked = c.byClass.toSeq.map((cls, cv) => (cls, dot(u, cv))).sortBy(-_._2)
      val (best, s0) = ranked.head
      val second = ranked.lift(1)
      Some(Verdict(best, s0, second.map(s0 - _._2).getOrElse(1.0), second.map(_._1)))

  /** answer above the margin, defer below it — the same contract the
   * symbolic tier has, so the two are comparable on one axis */
  def classify(c: Trained, v: Embedding, floor: Double = 0.02): Option[String] =
    score(c, v).filter(_.margin >= floor).map(_.best)
}
