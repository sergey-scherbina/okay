package okay.intent

/**
 * The conditions a measurement was taken under, printed with it
 * (specs/intent-classify.md).
 *
 * This exists because of a specific failure. Two measurements an hour
 * apart disagreed by ten points; the disagreement was read as a
 * finding and published; a re-read then showed the two runs had
 * embedded their messages differently — one bare, one with a classify
 * instruction — and neither printed which. The number was retracted.
 *
 * A convention would not have helped: a convention is exactly what
 * there was. What is needed is that a row CANNOT be written without
 * its terms, so `line` takes the conditions and there is no other way
 * to format one.
 *
 * Live-scope on purpose. A deterministic test carries its conditions
 * in its own source; only a measurement against a moving world needs
 * to say what the world was.
 */
final case class Conditions(embedder: String,
                            framing: String,
                            train: Int,
                            test: Int,
                            corpus: String = "IntentFixture.labelled",
                            extra: String = ""):
  /** one line, and the same line every time — a header a reader can
   * compare across lanes and across days */
  def header: String =
    val bits = Seq(
      s"embedder=$embedder",
      s"framing=$framing",
      s"train=$train test=$test",
      s"corpus=$corpus") ++ Option.when(extra.nonEmpty)(extra)
    bits.mkString("[", "  ", "]")

object Conditions:
  /** the two framings this programme has actually used, named rather
   * than described, so a row says which without a sentence */
  val Bare = "bare"
  val Classify = "classify-instruction"

  val SmallEmbedder = "Qwen3-Embedding-0.6B"
  val LargeEmbedder = "Qwen3-Embedding-4B"

  /** a measured row: the number, what it measures, and the terms.
   * There is deliberately no overload without `c`. */
  def line(c: Conditions, label: String, value: String): String =
    f"  $label%-30s $value%-24s ${c.header}"
