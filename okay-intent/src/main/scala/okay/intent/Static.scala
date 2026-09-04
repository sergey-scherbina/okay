package okay.intent

import okay.rag.{Embedding, embedding}

/**
 * Static embeddings: a transformer's semantics, distilled into a table
 * (specs/intent-classify.md).
 *
 * Every tier above 80% in this programme goes through an embedding
 * server, which is one network round trip per message and a piece of
 * infrastructure to keep alive. This one does not. A vector per token
 * is computed ONCE, offline, by whatever teacher is available; at
 * request time there is no inference at all — tokenize, look up, pool.
 * Pure array arithmetic, so it crosses to JS and Native where a native
 * runtime could not follow.
 *
 * That is `model2vec`'s method rather than its model: distilling from
 * the teacher already in use means the table inherits the semantics
 * the probe was fitted to, and no foreign tokenizer has to be matched.
 *
 * The honest trade: a static table cannot represent a word differently
 * in two contexts, which is most of what a transformer is for. It is a
 * COMPROMISE whose size is measured here, not a free lunch.
 */
object Static {

  /**
   * A distilled table.
   *
   * `weights` is the Zipf-style down-weighting `model2vec` applies:
   * frequent tokens carry less, because a word that appears
   * everywhere distinguishes nothing. Absent, "the" would dominate
   * every short message.
   */
  final case class Table(dim: Int, vectors: Map[String, Embedding],
                         weights: Map[String, Double],
                         split: String => Vector[String] = tokens):
    def size: Int = vectors.size

  /** the tokenizer, deliberately plain: lowercase, split on
   * non-letters, keep what has content. A BPE would carry more, and
   * `okay.lex.Bpe` is there for a lane that wants to match a foreign
   * vocabulary — but a table distilled from OUR teacher can be
   * distilled over whatever units we choose. */
  def tokens(text: String): Vector[String] =
    text.toLowerCase.split("[^\\p{L}\\p{N}']+").iterator
      .filter(_.nonEmpty).toVector

  /**
   * Words AND adjacent pairs.
   *
   * Distilling words alone makes a bag of words, and this task's
   * signal is word ORDER: "could you" requests where "we could"
   * proposes, and a bag cannot tell them apart. That is the same
   * mechanism that sank the BM25 tier, arriving a second time by a
   * different road — measured, a word-only table caps at 51.7% with
   * complete vocabulary coverage, against a teacher's 86.7%.
   *
   * A pair is a unit the teacher can embed exactly like a word, so
   * this costs one longer distillation and nothing at request time.
   */
  def units(text: String): Vector[String] =
    val ws = tokens(text)
    ws ++ ws.sliding(2).collect { case Vector(a, b) => s"$a $b" }.toVector

  /**
   * Build the table from token vectors and the corpus they came from.
   *
   * Frequencies come from the corpus rather than from a language-wide
   * table, because the weighting only has to be right RELATIVE to the
   * messages being classified.
   */
  def table(vectors: Map[String, Embedding], corpus: Seq[String],
            a: Double = 1e-3,
            split: String => Vector[String] = tokens): Table =
    val counts = corpus.flatMap(split).groupBy(identity).view.mapValues(_.size.toDouble).toMap
    val total = math.max(counts.values.sum, 1.0)
    val weights = vectors.keys.map { t =>
      val f = counts.getOrElse(t, 1.0) / total
      // the SIF weighting: a / (a + f), so a rare token keeps its
      // weight and a ubiquitous one is discounted smoothly
      t -> (a / (a + f))
    }.toMap
    val dim = vectors.values.headOption.map(_.length).getOrElse(0)
    Table(dim, vectors, weights, split)

  /**
   * Encode a message: the weighted mean of the vectors of the tokens
   * that are IN the table, normalised.
   *
   * A message whose every token is unknown gets `None` rather than a
   * zero vector — a zero vector is a point in the space and would be
   * classified as confidently as any other, which is the quiet failure
   * this returns `None` to avoid.
   */
  def encode(t: Table, text: String): Option[Embedding] =
    val ts = t.split(text).filter(t.vectors.contains)
    if ts.isEmpty then None
    else
      val acc = Array.fill(t.dim)(0.0f)
      var wsum = 0.0
      for tok <- ts do
        val w = t.weights.getOrElse(tok, 1.0)
        val v = t.vectors(tok)
        var i = 0
        while i < t.dim do
          acc(i) += (v(i) * w).toFloat
          i += 1
        wsum += w
      if wsum <= 0.0 then None
      else Some(Centroid.normalise(embedding(acc)))

  /** how much of a message the table can see — the number that says
   * whether a table is big enough for a corpus */
  def coverage(t: Table, text: String): Double =
    val ts = t.split(text)
    if ts.isEmpty then 0.0 else ts.count(t.vectors.contains).toDouble / ts.length
}
