package okay.intent

import okay.rag.{Embedding, Token, Tokens, Vectors, embedding}

/**
 * Typed spans from token vectors (specs/intent-spans.md).
 *
 * The tiers beside this one — `Centroid`, `Probe`, `CharGrams` — say
 * what KIND of thing a message is, from one vector for the sentence.
 * This one says WHICH WORDS of it are the place, the time, the thing:
 * the slot layer, from the vectors mean pooling throws away.
 *
 * A slot is described the way an intent is, by phrases. Each phrase is
 * embedded IN CONTEXT (`inContext`) and pooled to one vector; a slot's
 * centroid is the mean of its phrases. A message's candidate spans
 * are its windows of words (`windows`), each pooled the same way, and
 * the best window per slot is the answer when it clears a threshold.
 *
 * Pure: vectors in, spans out, no encoder in sight — `Centroid.train`
 * takes embeddings for the same reason. Measured by a consumer before
 * it was built: okay-chat, specs/meaning.md, 317 live turns.
 */
object Spans {

  /** what was found: the words, where, and how sure by two measures —
   * the centroid score the threshold is set on, and the score of the
   * nearest single phrase, which is what a reader looks at to see WHY */
  final case class Span(slot: String, text: String, start: Int, end: Int,
                        score: Double, nearest: Double)

  /** the phrase vectors per slot, unit length, and a centroid each */
  final case class Trained(phrases: Map[String, Vector[Embedding]],
                           centroids: Map[String, Embedding]):
    def slots: Vector[String] = centroids.keys.toVector.sorted

  /**
   * The phrase's OWN token vectors, from inside a carrier sentence.
   *
   * The lesson this function exists for: a phrase embedded alone is
   * the wrong reference for a span inside a sentence, by 0.2–0.3 of
   * cosine, because the encoder is contextual. So the phrase is put
   * where `{}` stands in the carrier, the whole sentence is encoded,
   * and only the tokens overlapping the phrase's characters come back
   * — none of the carrier's own.
   */
  def inContext(tokens: Tokens, carrier: String, phrase: String): Vector[Embedding] =
    val at = carrier.indexOf("{}")
    if at < 0 then tokens(phrase).map(_.vector)
    else
      val sentence = carrier.substring(0, at) + phrase + carrier.substring(at + 2)
      tokens(sentence).filter(t => t.start < at + phrase.length && t.end > at).map(_.vector)

  /** the mean of some vectors, unit length; `None` for none */
  def pooled(vs: Seq[Embedding]): Option[Embedding] =
    vs.headOption.map { h =>
      val acc = Array.fill(h.length)(0.0f)
      for v <- vs do
        var i = 0
        while i < acc.length && i < v.length do
          acc(i) += v(i)
          i += 1
      Vectors.normalize(embedding(acc.map(_ / vs.length)))
    }

  /**
   * Fit: each prototype is a slot and the token vectors of one phrase
   * in context; the phrase becomes one unit vector, the slot's
   * centroid the unit mean of its phrases. A slot whose phrases all
   * pooled to nothing is absent — not a zero vector, which would
   * score every window alike.
   */
  def train(protos: Seq[(String, Vector[Embedding])]): Trained =
    val phrases = protos.groupBy(_._1).map { (slot, rows) =>
      slot -> rows.toVector.flatMap((_, toks) => pooled(toks))
    }.filter(_._2.nonEmpty)
    Trained(phrases, phrases.flatMap((slot, vs) => pooled(vs).map(slot -> _)))

  /**
   * The function words a window may OPEN on and may not CLOSE on or
   * consist of — the measurement's second lesson: a contextual
   * encoder pours a phrase's meaning into its preposition, and a bare
   * «в» is then the nearest thing to «во Вроцлаве» there is. Surface
   * forms for ru, uk, pl and en, where the measurement was made; a
   * consumer in another language passes its own.
   */
  val functionWords: Set[String] = Set(
    "в", "во", "на", "у", "з", "із", "из", "по", "за", "до", "от", "від", "и", "а", "но", "не", "с", "со", "к", "о", "об", "при", "для", "про",
    "w", "we", "na", "z", "ze", "do", "od", "za", "o", "i", "a", "nie", "po", "u", "dla", "przy",
    "in", "on", "at", "for", "to", "the", "a", "an", "and", "of", "from", "by", "with", "or", "near")

  private def bare(w: String): String =
    w.toLowerCase.filter(c => c.isLetterOrDigit)

  /** a word: its characters in the text */
  final case class Word(text: String, start: Int, end: Int)

  def words(text: String): Vector[Word] =
    val out = Vector.newBuilder[Word]
    var i = 0
    while i < text.length do
      while i < text.length && text(i).isWhitespace do i += 1
      val s = i
      while i < text.length && !text(i).isWhitespace do i += 1
      if i > s then out += Word(text.substring(s, i), s, i)
    out.result()

  /** every window of one to `most` words that does not close on a
   * function word and is not made only of them */
  def windows(text: String, most: Int = 5,
              function: Set[String] = functionWords): Vector[Word] =
    val ws = words(text)
    for
      len <- (1 to most).toVector
      i <- 0 to ws.length - len
      slice = ws.slice(i, i + len)
      if !function(bare(slice.last.text)) && slice.exists(w => !function(bare(w.text)))
    yield Word(text.substring(slice.head.start, slice.last.end), slice.head.start, slice.last.end)

  /**
   * The best window per slot, at or above the threshold.
   *
   * A window's vector is the mean of every token overlapping its
   * characters — so a window that ends mid-word cannot happen, and a
   * word the tokenizer split into three pieces is one thing here, as
   * it is to the person who typed it.
   */
  def find(tokens: Vector[Token], text: String, model: Trained,
           threshold: Double = 0.5, most: Int = 5,
           function: Set[String] = functionWords): Vector[Span] =
    val candidates = windows(text, most, function).flatMap { w =>
      pooled(tokens.filter(t => t.start < w.end && t.end > w.start).map(_.vector)).map(w -> _)
    }
    model.slots.flatMap { slot =>
      val c = model.centroids(slot)
      val scored = candidates.map { (w, v) =>
        val nearest = model.phrases(slot).map(p => Vectors.cosine(v, p).toDouble).max
        Span(slot, w.text, w.start, w.end, Vectors.cosine(v, c).toDouble, nearest)
      }
      scored.maxByOption(_.score).filter(_.score >= threshold)
    }
}
