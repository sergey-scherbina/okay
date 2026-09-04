package okay.intent

import okay.codec.{Json, Schema}

/**
 * The path from "I have messages" to "I have a model"
 * (specs/intent-classify.md).
 *
 * Twenty lanes measured these tiers and every fitted one existed only
 * inside the test that fitted it. A caller reading this module got the
 * types, the accuracy tables, and no route: `Centroid.train` wants
 * embeddings it has no door for, `Fitted` turns a model into data and
 * nothing ever wrote the data anywhere. This is the missing half —
 * fit, write, read — and `Models` is what it produced.
 *
 * Deliberately thin. It adds no cleverness to the tiers; it is a
 * DOOR, and its value is that a caller does not have to assemble
 * `Fitted.save`, a summoned `Schema` and `Json.parseValue` correctly
 * to keep a model between two processes.
 */
object Fit {

  /**
   * The tier that needs nothing: hashed character n-grams over the
   * text itself. No embedder, no network, no gateway — which is what
   * makes it the one a library can SHIP.
   *
   * `dim` defaults to 1024 rather than `CharGrams`'s own 4096 because
   * of what the measurement said: at this corpus size 1024 scores
   * within two points of 4096 (61.7% against 63.3% on held-out
   * English) and the serialised model is a quarter of the size. A
   * caller with a real corpus should raise it.
   */
  def grams(rows: Seq[(String, String)], dim: Int = 1024): CharGrams.Trained =
    CharGrams.train(rows, dim = dim)

  // There is deliberately no `Fit.centroid(rows)` or `Fit.probe(rows)`
  // for FITTING: `Centroid.train` and `Probe.train` already take
  // exactly `(Embedding, String)` and a wrapper that renames a call
  // adds a name to learn and nothing else. What was missing for those
  // two is below — keeping the result.

  // ---------------------------------------------------------------
  // written down, and read back

  private def write[A: Schema](a: A): String = Json.write(a)

  private def read[A](s: String)(using sc: Schema[A]): Either[String, A] =
    Json.decode(sc)(Json.parseValue(s))

  def save(t: CharGrams.Trained): String = write(Fitted.save(t))
  def save(t: Probe.Trained): String = write(Fitted.save(t))
  def save(t: Centroid.Trained): String = write(Fitted.save(t))

  def grams(json: String): Either[String, CharGrams.Trained] =
    read[Fitted.GramsModel](json).map(Fitted.load)
  def probe(json: String): Either[String, Probe.Trained] =
    read[Fitted.ProbeModel](json).map(Fitted.load)
  def centroid(json: String): Either[String, Centroid.Trained] =
    read[Fitted.CentroidModel](json).map(Fitted.load)
}
