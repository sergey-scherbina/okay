package okay.intent

import okay.rag.Embedding

/**
 * kNN over example embeddings (specs/intent-classify.md).
 *
 * A centroid averages a class into one point, which assumes the class
 * is a ball. These are not: `Other` is a deliberate grab-bag — a
 * birthday wish, a double charge, a weather forecast — and its mean is
 * a point that resembles none of them. Nearest neighbours never form
 * that mean, so this is the cheapest way to ask whether the centroid's
 * shape assumption was costing anything.
 *
 * Same contract as the other tiers, so the rows compare: `train`,
 * `score`, `classify`, and a margin that is a share of the vote rather
 * than a distance, so it does not move with the absolute similarity.
 */
object Nearest {

  final case class Trained(examples: Vector[(Embedding, String)], taxon: Taxon):
    def silent: Vector[String] =
      val seen = examples.map(_._2).toSet
      taxon.classes.filterNot(seen)

  final case class Verdict(best: String, margin: Double, runnerUp: Option[String])

  def train(labelled: Seq[(Embedding, String)]): Trained =
    Trained(labelled.map((v, c) => (Centroid.normalise(v), c)).toVector,
      Taxon.parsed(labelled.map(_._2).distinct.sorted))

  /** fitted against a taxonomy the caller declares */
  def against(taxon: Taxon, labelled: Seq[(Embedding, String)]): Either[String, Trained] =
    taxon.check(labelled.map(_._2)).map(_ => train(labelled).copy(taxon = taxon))

  /**
   * Vote among the k nearest, each neighbour weighted by its
   * similarity — an unweighted vote throws away exactly the
   * information that distinguishes a close neighbour from a distant
   * one, which is what this tier is for.
   */
  def score(t: Trained, v: Embedding, k: Int = 5): Option[Verdict] =
    if t.examples.isEmpty then None
    else
      val u = Centroid.normalise(v)
      val near = t.examples.map((e, c) => (c, Centroid.dot(u, e))).sortBy(-_._2).take(k)
      val byClass = near.groupBy(_._1).map((c, xs) => c -> xs.map(_._2).sum)
      val ranked = byClass.toSeq.sortBy(-_._2)
      val total = ranked.map(_._2).sum
      val (best, s0) = ranked.head
      val margin =
        if total <= 0.0 then 0.0
        else (s0 - ranked.lift(1).map(_._2).getOrElse(0.0)) / total
      Some(Verdict(best, margin, ranked.lift(1).map(_._1)))

  def classify(t: Trained, v: Embedding, floor: Double = 0.2, k: Int = 5): Option[String] =
    score(t, v, k).filter(_.margin >= floor).map(_.best)
}
