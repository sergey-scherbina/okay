package okay.dlm

import okay.rag.Embedding

/**
 * ONE QUESTION, ANSWERED BY A JUDGE OR NOT AT ALL.
 *
 * A head is a `Judge` and the question it asks it, with the bar it
 * will not answer below. What kind of move a message is (an answer,
 * a correction, a question about us, a pleasantry); what was answered
 * to a yes/no question (yes, no, tell me more); whether work needs
 * somebody to be there; which frame a sentence belongs to — each is a
 * head with its own options. By default the judge is ours, a probe
 * over the head's exemplars; a remote judge answers the same question
 * over the wire (specs/dlm.md, "Backends").
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
final class Head(val judge: Judge,
                 val question: Judge.Question,
                 val margin: Float,
                 val quiet: Option[String] = None):

  /** a head is live when it has a judge and something to ask about */
  val live: Boolean = question.options.nonEmpty && (judge ne Judge.silent)

  /** the classes this head can name */
  def classes: Vector[String] = question.names

  /** the winner above the margin, or `None` — the caller's default */
  def of(text: String): Option[String] = of(text, margin)

  /**
   * The same question at a DIFFERENT bar, for callers where being
   * wrong costs something else. Inside an intake a wrong divert asks a
   * person their city twice, so the bar is high; at the door nothing
   * is pending and a wrong "you're welcome" costs one sentence, so it
   * is low. Same judge, same scores; what a mistake buys is the
   * caller's business rather than a threshold baked into the model.
   */
  def of(text: String, bar: Float): Option[String] =
    verdict(text).filter(_.margin >= bar).map(_.best).filterNot(quiet.contains)

  /** the whole answer, for a caller that wants the runner-up or the
   * judge's confidence too */
  def verdict(text: String): Option[Judge.Choice] =
    if question.options.isEmpty then None else judge.choose(text, question)

  /** every class and its probability, best first — the operator
   * surface: a threshold is tuned against numbers, not anecdote */
  def scores(text: String): Vector[(String, Float)] =
    verdict(text).map(_.probabilities.map((c, p) => c -> p.toFloat)).getOrElse(Vector.empty)

object Head:

  /** a head with nothing behind it: never answers, and says so */
  val off: Head = new Head(Judge.silent, Judge.Question(Vector.empty), 1f)

  /**
   * A head over a table of exemplars, judged as the scope says: ours
   * by default, a remote judge when a `given Judge.Fit` names one.
   * Absent exemplars is `off` — a supported deployment, the head then
   * holds its caller's default. `descriptions` and `instructions` are
   * what a judge that reads words is told; ours ignores them.
   */
  def of(exemplars: Option[Exemplars], margin: Float, quiet: Option[String] = None,
         instructions: String = "", descriptions: Map[String, String] = Map.empty)
        (using fit: Judge.Fit): Head =
    exemplars.filter(_.rows.nonEmpty) match
      case None => new Head(Judge.silent, Judge.Question(Vector.empty), margin, quiet)
      case Some(e) =>
        new Head(fit(e), Judge.Question(e.labels.map(l => l -> descriptions.getOrElse(l, "")), instructions), margin, quiet)

  /** the shape the first consumer wrote: a table and a plain encoder
   * function — ours, the probe, and nothing from scope */
  def apply(exemplars: Option[Exemplars], embed: Option[String => Embedding], margin: Float,
            quiet: Option[String] = None): Head =
    (exemplars.filter(_.rows.nonEmpty), embed) match
      case (Some(e), Some(f)) => new Head(Judge.probe(e, f), Judge.Question.of(e.labels), margin, quiet)
      case _ => new Head(Judge.silent, Judge.Question(Vector.empty), margin, quiet)
