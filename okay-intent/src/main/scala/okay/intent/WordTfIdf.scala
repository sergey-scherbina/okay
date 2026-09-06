package okay.intent

import okay.rag.Embedding

/**
 * The classical baseline: word-level TF-IDF into the same linear
 * model as every other tier (specs/intent-classify.md,
 * intent-tfidf-word-linear).
 *
 * It sits between BM25 and character n-grams in what it sees — words,
 * like BM25, but weighed into one vector a linear model can separate,
 * like the n-grams — and it exists to answer one question about the
 * n-gram tier's number: is it about CHARACTERS, or about having a
 * linear model at all?
 *
 * A vocabulary rather than a hash, because that is what the classical
 * method is: the vocabulary and the IDF are fitted on the training
 * half and carried with the model, and a word never seen in training
 * contributes nothing. Tokens are runs of letters or digits in any
 * script (`\p{L}`/`\p{N}`), lowercased, so the Russian arm is words
 * too. TF is the count, IDF is the smoothed `log((N + 1) / (df + 1)) +
 * 1`, the vector is L2-normalised, and the descent is `Probe`'s —
 * literally: the features are handed to `Probe.train` as an embedding.
 */
object WordTfIdf {

  final case class Vocab(index: Map[String, Int], idf: Array[Double]):
    def size: Int = idf.length

  final case class Trained(vocab: Vocab, probe: Probe.Trained):
    def classes: Vector[String] = probe.classes
    def silent: Vector[String] = probe.silent

  private val token = "[\\p{L}\\p{N}]+".r

  def tokens(text: String): Vector[String] =
    token.findAllIn(text.toLowerCase).toVector

  /** the vocabulary and IDF of a training corpus, in first-seen order
   * so a fit is deterministic */
  def fit(texts: Seq[String]): Vocab =
    val df = scala.collection.mutable.LinkedHashMap.empty[String, Int]
    for t <- texts do
      for w <- tokens(t).distinct do df.update(w, df.getOrElse(w, 0) + 1)
    val n = texts.length.toDouble
    val words = df.keys.toVector
    val idf = words.map(w => math.log((n + 1.0) / (df(w) + 1.0)) + 1.0).toArray
    Vocab(words.zipWithIndex.toMap, idf)

  /** the TF-IDF vector of one text over a fitted vocabulary, L2-normalised */
  def features(vocab: Vocab, text: String): Embedding =
    val v = new Array[Float](vocab.size)
    for w <- tokens(text) do
      vocab.index.get(w) match
        case Some(i) => v(i) += vocab.idf(i).toFloat
        case None => ()
    var norm = 0.0
    var i = 0
    while i < v.length do { norm += v(i).toDouble * v(i); i += 1 }
    if norm > 0.0 then
      val s = math.sqrt(norm).toFloat
      i = 0
      while i < v.length do { v(i) /= s; i += 1 }
    okay.rag.embedding(v)

  def train(labelled: Seq[(String, String)], epochs: Int = 300, rate: Double = 0.5): Trained =
    val vocab = fit(labelled.map(_._1))
    val rows = labelled.map((text, c) => (features(vocab, text), c))
    Trained(vocab, Probe.train(rows, epochs, rate))

  /** fitted against a taxonomy the caller declares */
  def against(taxon: Taxon, labelled: Seq[(String, String)], epochs: Int = 300,
              rate: Double = 0.5): Either[String, Trained] =
    taxon.check(labelled.map(_._2)).map: _ =>
      val t = train(labelled, epochs, rate)
      t.copy(probe = t.probe.copy(taxon = taxon))

  def score(t: Trained, text: String): Option[Probe.Verdict] =
    Probe.score(t.probe, features(t.vocab, text))

  def classify(t: Trained, text: String, floor: Double = 0.3): Option[String] =
    Probe.classify(t.probe, features(t.vocab, text), floor)
}
