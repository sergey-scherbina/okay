package okay.agent

import okay.codec.Schema

/**
 * One taxonomy both tiers read (specs/intent-classify.md).
 *
 * The model tier takes its classes from `Schema[I]`; a fitted tier
 * infers them from whatever labels its training rows happen to carry.
 * Nothing connected the two, so pointing both at the same taxonomy
 * meant aligning it by hand — and the sharper problem a consumer
 * named: a taxonomy that arrives as DATA could not reach the model
 * tier at all. That is exactly what `intent-label-distillation`
 * produces, a corpus that can define examples but never a class.
 *
 * So the taxonomy becomes a VALUE with two constructors: `of[I]` reads
 * it out of a `Schema`, `parsed` builds it from strings. Everything
 * downstream takes the value and does not care which door it came
 * through.
 *
 * Named `Taxon` rather than `Taxonomy` because the precedence lane
 * shipped and then withdrew a `Taxonomy[I]` typeclass, and a name that
 * means one thing in the history and another in the code is worse than
 * a slightly odd name.
 */
/** one authored example: the message, and the class it illustrates */
final case class Phrasing(text: String, cls: String) derives Schema

final case class Taxon(classes: Vector[String],
                       examples: Vector[Phrasing] = Vector.empty) derives Schema:

  def has(cls: String): Boolean = classes.contains(cls)

  /** the examples as `prompt` wants them: message first, class second.
   * A `Vector` rather than a `Map[String, Vector[String]]` because a
   * map's key order is unspecified and this rides on a wire — and
   * because the codec has no map schema, which turned out to be the
   * same argument arriving from the other side. */
  def phrasings: Vector[(String, String)] =
    examples.map(p => p.text -> p.cls)

  def examplesFor(cls: String): Vector[String] =
    examples.filter(_.cls == cls).map(_.text)

  /**
   * Every label in `rows` must be one of ours.
   *
   * A fit that silently accepts an unknown label invents a class, and
   * the invented one then appears in a confusion matrix as a column
   * nobody declared — which is how `Eval`'s "an invented label is
   * still a class" rule, correct for a classifier, becomes wrong for a
   * typo.
   */
  def check(rows: Seq[String]): Either[String, Unit] =
    val unknown = rows.filterNot(has).distinct
    if unknown.isEmpty then Right(())
    else Left(s"labels not in the taxonomy: ${unknown.mkString(", ")}")

object Taxon {

  /** from a sum type: the case names, in declaration order, with the
   * hierarchy flattened to its leaves the way `Classify.label` reads
   * them */
  def of[I](using s: Schema[I]): Taxon = s match
    case su: Schema.SSum[I] => Taxon(su.cases.map(_._1))
    case _ => Taxon(Vector.empty)

  /** from data: what a distilled corpus or an edited file can carry */
  def parsed(classes: Seq[String],
             examples: Seq[(String, String)] = Nil): Taxon =
    Taxon(classes.toVector.distinct,
      examples.map((t, c) => Phrasing(t, c)).toVector)
}
