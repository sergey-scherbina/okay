package okay.agent

import okay.rag.Embedding

/**
 * A training row knows what language it is in
 * (specs/intent-classify.md).
 *
 * A row was `(text, embedding, class)`, so the language it was written
 * in had nowhere to live and a multilingual corpus pooled every
 * language into one boundary. That is not a hypothetical cost: the
 * per-language arms of the embedding bake-off came out unreadable, and
 * a consumer had written this down BEFORE that lane ran.
 *
 * A centroid averaged across languages is a worse centroid for the
 * same reason the language gap exists at all — the vectors of two
 * languages are not interchangeable, so their mean is a point in
 * neither.
 *
 * This is a grouping key, not new mathematics, and the fallback is the
 * part that makes it usable: a language with too few rows to fit its
 * own model borrows the pooled one rather than getting a model built
 * from four examples.
 */
final case class Row(text: String, vector: Embedding, cls: String,
                     lang: String = Row.Any)

object Row:
  /** the language of a row nobody tagged, and the key of the pooled
   * model — one value doing both jobs, so a corpus with no languages
   * at all behaves exactly as it did before this existed */
  val Any: String = "*"

/**
 * A model per language, with a pooled fallback.
 *
 * `minRows` is the threshold below which a language is not fitted on
 * its own. It is a policy rather than a discovery: the learning curve
 * put the probe's stabilisation at about 32 examples, so anything
 * under that is a model whose numbers cannot be defended, and
 * borrowing the pooled one is the honest default.
 */
final case class ByLanguage[M](pooled: M, byLang: Map[String, M],
                               fittedFor: Set[String]):
  def apply(lang: String): M = byLang.getOrElse(lang, pooled)
  def isOwn(lang: String): Boolean = fittedFor.contains(lang)

object ByLanguage:
  def fit[M](rows: Seq[Row], minRows: Int = 32)(train: Seq[Row] => M): ByLanguage[M] =
    val pooled = train(rows)
    val groups = rows.groupBy(_.lang).filter((l, rs) => l != Row.Any && rs.length >= minRows)
    ByLanguage(pooled, groups.view.mapValues(train).toMap, groups.keySet)
