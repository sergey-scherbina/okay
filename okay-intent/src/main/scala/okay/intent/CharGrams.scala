package okay.intent

/**
 * Character n-grams with a linear model (specs/intent-classify.md).
 *
 * The one candidate that needs NOTHING at request time — no tokenizer,
 * no embedding server, no network, no model — and the only one whose
 * features are language-agnostic by construction. That matters here
 * for a measured reason: the Russian arm scores 0.741 against English's
 * 0.929, and every tier above 80% so far has gone through an embedding
 * model whose multilingual quality is exactly what that gap is made of.
 * A 4-character window does not know what language it is in.
 *
 * Hashed features rather than a vocabulary: a dictionary of n-grams
 * grows with the corpus and has to be shipped beside the weights,
 * while a hash of fixed width is a constant that never needs
 * distribution. Collisions cost a little accuracy and buy a model that
 * is just an array.
 *
 * Trained with the same plain gradient descent as `Probe`, on the same
 * contract as every other tier — `train`, `score`, `classify`, a
 * `Trained` — so it goes into the same table.
 */
object CharGrams {

  final case class Trained(classes: Vector[String], dim: Int,
                           w: Array[Array[Double]], b: Array[Double],
                           low: Int, high: Int, taxon: Taxon):
    def silent: Vector[String] = taxon.classes.filterNot(classes.contains)

  final case class Verdict(best: String, probability: Double,
                           margin: Double, runnerUp: Option[String])

  /** the text as it is hashed: lowercased, whitespace flattened, and
   * padded so that a first and last n-gram exist at all */
  private def prepared(text: String): String =
    " " + text.toLowerCase.replaceAll("\\s+", " ").trim + " "

  /**
   * A term-frequency vector over hashed n-grams, L2-normalised so a
   * long message does not simply outweigh a short one.
   *
   * No IDF: it would have to be fitted on the training half and
   * carried with the model, and at this corpus size the estimate is
   * noisier than the thing it corrects.
   */
  def features(t: String, dim: Int, low: Int, high: Int): Array[Double] =
    val s = prepared(t)
    val v = Array.fill(dim)(0.0)
    var n = low
    while n <= high do
      var i = 0
      while i + n <= s.length do
        val g = s.substring(i, i + n)
        // a stable, cheap hash: String.hashCode is specified by the
        // language, so a model trained on one JVM scores the same on
        // the next
        val h = math.abs(g.hashCode % dim)
        v(h) += 1.0
        i += 1
      n += 1
    val norm = math.sqrt(v.map(x => x * x).sum)
    if norm > 0.0 then
      var i = 0
      while i < dim do
        v(i) /= norm
        i += 1
    v

  private def softmax(z: Array[Double]): Array[Double] =
    val m = z.max
    val e = z.map(x => math.exp(x - m))
    val s = e.sum
    if s == 0.0 then e else e.map(_ / s)

  private def logits(t: Trained, f: Array[Double]): Array[Double] =
    Array.tabulate(t.classes.length) { c =>
      var s = t.b(c)
      val row = t.w(c)
      var i = 0
      while i < t.dim do
        s += row(i) * f(i)
        i += 1
      s
    }

  def train(labelled: Seq[(String, String)], dim: Int = 4096,
            low: Int = 3, high: Int = 5,
            epochs: Int = 400, rate: Double = 0.5): Trained =
    val classes = labelled.map(_._2).distinct.sorted.toVector
    val w = Array.fill(classes.length)(Array.fill(dim)(0.0))
    val b = Array.fill(classes.length)(0.0)
    val model = Trained(classes, dim, w, b, low, high, Taxon.parsed(classes))
    val rows = labelled.map((text, c) => (features(text, dim, low, high), classes.indexOf(c))).toVector
    var epoch = 0
    while epoch < epochs do
      for (f, target) <- rows do
        val p = softmax(logits(model, f))
        var c = 0
        while c < classes.length do
          val err = p(c) - (if c == target then 1.0 else 0.0)
          val row = w(c)
          var i = 0
          while i < dim do
            if f(i) != 0.0 then row(i) -= rate * err * f(i)
            i += 1
          b(c) -= rate * err
          c += 1
      epoch += 1
    model

  /** fitted against a taxonomy the caller declares */
  def against(taxon: Taxon, labelled: Seq[(String, String)], dim: Int = 4096,
              low: Int = 3, high: Int = 5, epochs: Int = 400,
              rate: Double = 0.5): Either[String, Trained] =
    taxon.check(labelled.map(_._2))
      .map(_ => train(labelled, dim, low, high, epochs, rate).copy(taxon = taxon))

  def score(t: Trained, text: String): Option[Verdict] =
    if t.classes.isEmpty then None
    else
      val p = softmax(logits(t, features(text, t.dim, t.low, t.high)))
      val ranked = t.classes.indices.map(i => (t.classes(i), p(i))).sortBy(-_._2)
      val (best, p0) = ranked.head
      Some(Verdict(best, p0, p0 - ranked.lift(1).map(_._2).getOrElse(0.0),
        ranked.lift(1).map(_._1)))

  def classify(t: Trained, text: String, floor: Double = 0.3): Option[String] =
    score(t, text).filter(_.margin >= floor).map(_.best)

  /**
   * The same classifier under another taxonomy's names.
   *
   * A fitted model speaks the names its rows carried, and a caller
   * whose taxonomy is domain-bearing ("MeetingProposal", not
   * "Proposal") otherwise translates by hand — the exact defect
   * `Cues.renamed` was built to end, one tier over. So this obeys the
   * same rule and is TOTAL IN BOTH DIRECTIONS: every class the model
   * knows must be named, and every name given must be a class the
   * destination holds. A `Map` with a fallback would be the silent
   * `case _ =>` again in different syntax.
   *
   * The weights are untouched: only the labels on the rows change.
   */
  def renamed(t: Trained, onto: Taxon, mapping: Map[String, String])
  : Either[String, Trained] =
    val unmapped = t.classes.filterNot(mapping.contains)
    val offTaxon = mapping.values.toVector.distinct.filterNot(onto.has)
    if unmapped.nonEmpty then
      Left(s"no name given for: ${unmapped.sorted.mkString(", ")}")
    else if offTaxon.nonEmpty then
      Left(s"renamed onto classes the taxonomy does not hold: ${offTaxon.sorted.mkString(", ")}")
    else Right(t.copy(classes = t.classes.map(mapping), taxon = onto))
}

