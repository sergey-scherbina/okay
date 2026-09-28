package okay.dlm

import okay.rag.Embedding

/**
 * ONE QUESTION, ANSWERED BY VECTORS OR NOT AT ALL.
 *
 * A head is a probe over a table of exemplars, with the bar it will
 * not answer below. What kind of move a message is (an answer, a
 * correction, a question about us, a pleasantry); what was answered
 * to a yes/no question (yes, no, tell me more); whether work needs
 * somebody to be there; which frame a sentence belongs to — each is a
 * head over the SAME frozen encoder, the same artifact format and the
 * same compile step. Only the corpus differs.
 *
 * `margin` is the gap between the top two probabilities, not the
 * winner's own probability: a message equally close to two classes is
 * ambiguous however confident the winner looks. Below it, `None` —
 * and `None` means whatever the caller's own default is: "an answer"
 * inside an intake, "the rules' default" for a frame. A head is used
 * the way an abstention is: it must be CONFIDENT to divert a decision
 * from its default, and silence costs nothing.
 *
 * `quiet` names a class whose winning is not worth saying — the
 * widest class by far, the one silence already means. An act head
 * whose default is "answer" answers `None` for it, so a caller reads
 * one signal and not two.
 */
final class Head(exemplars: Option[Exemplars],
                 embed: Option[String => Embedding],
                 val margin: Float,
                 val quiet: Option[String] = None):

  private lazy val fitted: Option[okay.intent.Probe.Trained] =
    exemplars.filter(_.rows.nonEmpty).map(e => okay.intent.Probe.train(e.labelled))

  /** the head is live only when BOTH halves are present */
  val live: Boolean = exemplars.exists(_.rows.nonEmpty) && embed.isDefined

  /** the classes this head can name */
  def classes: Vector[String] = exemplars.map(_.labels).getOrElse(Vector.empty)

  /** the winner above the margin, or `None` — the caller's default */
  def of(text: String): Option[String] = of(text, margin)

  /**
   * The same question at a DIFFERENT bar, for callers where being
   * wrong costs something else. Inside an intake a wrong divert asks a
   * person their city twice, so the bar is high; at the door nothing
   * is pending and a wrong "you're welcome" costs one sentence, so it
   * is low. Same classifier, same scores; what a mistake buys is the
   * caller's business rather than a threshold baked into the model.
   */
  def of(text: String, bar: Float): Option[String] =
    verdict(text).filter(_.margin >= bar).map(_.best).filterNot(quiet.contains)

  /** the whole verdict, for a caller that wants the runner-up too */
  def verdict(text: String): Option[okay.intent.Probe.Verdict] =
    (fitted, embed) match
      case (Some(t), Some(f)) => okay.intent.Probe.score(t, f(text))
      case _ => None

  /** every class and its probability, best first — the operator
   * surface: a threshold is tuned against numbers, not anecdote */
  def scores(text: String): Vector[(String, Float)] =
    (fitted, embed) match
      case (Some(t), Some(f)) =>
        okay.intent.Probe.ranked(t, f(text)).map((c, p) => c -> p.toFloat)
      case _ => Vector.empty

object Head:
  /** a head with nothing behind it: never answers, and says so */
  val off: Head = Head(None, None, 1f)
