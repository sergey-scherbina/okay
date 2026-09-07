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
   * Words, adjacent pairs AND adjacent triples
   * (intent-static-trigrams-and-pca): the same argument once more —
   * measured, +5.0 to the probe and +11.7 to the centroid over pairs
   * on the same split, for a table 1.65x the units. The best
   * no-network table is this splitter's, cut by `pca` to 256.
   */
  def units3(text: String): Vector[String] =
    val ws = tokens(text)
    ws ++ ws.sliding(2).collect { case Vector(a, b) => s"$a $b" }.toVector ++
      ws.sliding(3).collect { case Vector(a, b, c) => s"$a $b $c" }.toVector

  /**
   * model2vec's PCA step (intent-static-trigrams-and-pca): the
   * table's own unit vectors, centred, projected onto their top-k
   * principal subspace, so a 1024-dimensional table ships at 256 —
   * a quarter of the bytes — and, measured, loses nothing (the
   * probe gained 5 points: the cut is a denoising). Fitted once at
   * distillation by subspace iteration over the covariance; the
   * PROJECTED table is what ships, and request time is still lookup
   * and pool — the `Pca` itself is not needed after `projected`.
   * `basis(i)` is a unit vector of the original dimension.
   */
  final case class Pca(mean: Array[Double], basis: Array[Array[Double]]):
    def k: Int = basis.length
    /** the first `n` components, for a narrower cut from one fit */
    def take(n: Int): Pca = Pca(mean, basis.take(n))

  def fitPca(vectors: Iterable[Embedding], k: Int, sweeps: Int = 40): Pca =
    val rows = vectors.toArray
    require(rows.nonEmpty, "a PCA over no vectors")
    val n = rows.length; val d = rows(0).length
    val mean = Array.tabulate(d)(j => rows.iterator.map(_(j).toDouble).sum / n)
    // the covariance, d x d, built once
    val cov = Array.ofDim[Double](d, d)
    val centred = Array.ofDim[Double](d)
    for r <- rows do
      var j = 0
      while j < d do { centred(j) = r(j) - mean(j); j += 1 }
      var a = 0
      while a < d do
        val ca = centred(a)
        if ca != 0.0 then
          val row = cov(a); var b = 0
          while b < d do { row(b) += ca * centred(b); b += 1 }
        a += 1
    // subspace iteration, Q <- orth(C Q), seeded so a fit is reproducible
    val rnd = new scala.util.Random(7)
    def orthonormalise(m: Array[Array[Double]]): Array[Array[Double]] =
      val out = Array.ofDim[Array[Double]](m.length)
      for i <- m.indices do
        val v = m(i).clone()
        for p <- 0 until i do
          val u = out(p); var dot = 0.0; var j = 0
          while j < d do { dot += v(j) * u(j); j += 1 }
          j = 0
          while j < d do { v(j) -= dot * u(j); j += 1 }
        var norm = 0.0; var j = 0
        while j < d do { norm += v(j) * v(j); j += 1 }
        norm = math.sqrt(norm)
        j = 0
        while j < d do { v(j) = if norm > 0 then v(j) / norm else 0.0; j += 1 }
        out(i) = v
      out
    var q = orthonormalise(Array.fill(math.min(k, d))(Array.fill(d)(rnd.nextGaussian())))
    for _ <- 1 to sweeps do
      q = orthonormalise(q.map { v =>
        val w = Array.ofDim[Double](d)
        var a = 0
        while a < d do
          val row = cov(a); var s = 0.0; var b = 0
          while b < d do { s += row(b) * v(b); b += 1 }
          w(a) = s; a += 1
        w
      })
    Pca(mean, q)

  /** one vector, centred and projected */
  def project(p: Pca, v: Embedding): Embedding =
    val d = p.mean.length
    val out = Array.ofDim[Float](p.k)
    var i = 0
    while i < p.k do
      val u = p.basis(i); var s = 0.0; var j = 0
      while j < d do { s += (v(j) - p.mean(j)) * u(j); j += 1 }
      out(i) = s.toFloat
      i += 1
    embedding(out)

  /** the table that ships: every unit projected, weights and splitter kept */
  def projected(t: Table, p: Pca): Table =
    Table(p.k, t.vectors.map((k, v) => k -> project(p, v)), t.weights, t.split)

  /** the share of the vectors' variance the subspace keeps — the
   * honest size of the cut (91.5% at 256 of 1024, 78.5% at 128) */
  def variance(p: Pca, vectors: Iterable[Embedding]): Double =
    val total = vectors.iterator.map(v => v.indices.map(j => { val c = v(j) - p.mean(j); c * c }).sum).sum
    val inSub = vectors.iterator.map(v => { val z = project(p, v); z.indices.map(i => z(i).toDouble * z(i)).sum }).sum
    if total > 0 then inSub / total else 0.0

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
