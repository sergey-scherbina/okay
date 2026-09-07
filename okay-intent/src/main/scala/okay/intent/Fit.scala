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
   * The defaults are the shipped model's: 4096 buckets and a 2–3
   * window. They were 1024 and 3–5 — chosen when 1024 scored within
   * two points of 4096 (61.7% against 63.3%) for a quarter of the
   * size — until intent-window-by-dim measured (2,3) @4096 at +5.0
   * behind the cues (80.0 vs 75.0), +8.4 under a typo (71.7 vs 63.3)
   * with `Other` above the per-class floor, and the operator took
   * the size (intent-shipped-model-4096, 2026-09-07). The artifact is
   * what these defaults produce; move them together or not at all.
   */
  def grams(rows: Seq[(String, String)], dim: Int = 4096, low: Int = 2, high: Int = 3): CharGrams.Trained =
    CharGrams.train(rows, dim = dim, low = low, high = high)

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
