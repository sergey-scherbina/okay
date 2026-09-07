package okay.intent

import okay.codec.Schema

/**
 * Demonstrations for a prompt, SELECTED FROM A LOG rather than
 * written beside it (tod-demonstrations-from-the-log; "Show, Don't
 * Tell", Zhao & Gupta 2022).
 *
 * SDT's finding is that one annotated example in the input does the
 * work that slot DESCRIPTIONS are supposed to do. The okay-shaped
 * half of it is where the example comes from: a recorded turn — the
 * message, and the typed `Reading` it produced — IS an annotated
 * example already, so a durable log stops being only an audit trail
 * and becomes prompt material. Nothing here reads a log itself: the
 * caller passes recorded pairs (okay-chat reads its ChatLog, a test
 * passes a list), which keeps this module free of a persistence
 * dependency and the selection testable without one.
 *
 * The selection rule is deliberately the dullest one that can be
 * stated in a sentence, because a clever one cannot be defended
 * without its own measurement: ONE demonstration per class, the
 * first the log offers, in the taxonomy's own case order. That gives
 * a prompt whose examples cover the classes exactly once, which is
 * the shape SDT reports, and it is stable — the same log yields the
 * same prompt, so a fingerprint over the prompt stays meaningful.
 *
 * WHAT IT DOES NOT DO: pick demonstrations near the message being
 * classified (a retrieval step, which needs the vector tier and its
 * own measurement), weight them, or rank them by confidence. Each of
 * those is a separate claim and a separate number.
 */
object Demonstrations {

  /**
   * One demonstration per case of the taxonomy, in the taxonomy's own
   * order, taking the first the log offers for each; at most `limit`
   * of them (0 or fewer means all the classes it found).
   *
   * A message that appears in `exclude` is never chosen — the caller
   * passes the set it is about to classify, because a demonstration
   * that is also a scored message measures the prompt against its own
   * answer key.
   */
  def perClass[I](recorded: Seq[(String, I)], limit: Int = 0,
                  exclude: Set[String] = Set.empty)(using s: Schema[I]): List[(String, I)] =
    val order = s match
      case su: Schema.SSum[I] => su.cases.map(_._1).toList
      case _ => Nil
    val seen = scala.collection.mutable.LinkedHashMap.empty[String, (String, I)]
    for (m, i) <- recorded if !exclude.contains(m) do
      val label = Classify.label(i)
      if !seen.contains(label) then seen += (label -> ((m, i)))
    val byOrder =
      if order.isEmpty then seen.values.toList
      else order.flatMap(seen.get)
    if limit > 0 then byOrder.take(limit) else byOrder

  /**
   * The bridge from a log of RAW REPLIES to demonstrations: each
   * recorded (message, reply) is decoded with the same reader the
   * live path uses, an undecodable or empty reply is dropped, and the
   * first alternative of the first span is the demonstration's
   * intent.
   *
   * Dropping rather than failing is the point: a log contains what a
   * model actually said, including what it said badly, and a prompt
   * built from it must not inherit that.
   */
  def fromReplies[I](recorded: Seq[(String, String)])
                    (using sr: Schema[Reading[I]]): List[(String, I)] =
    recorded.toList.flatMap { (m, reply) =>
      Classify.read[I](reply)(using sr).toOption
        .flatMap(_.spans.headOption)
        .flatMap(_.alts.headOption)
        .map(a => (m, a.intent))
    }

  /** what `prompt` will show, for a caller that wants to log or
   * fingerprint the demonstrations without building the whole prompt */
  def render[I](ds: List[(String, I)])(using s: Schema[I]): String =
    ds.map((m, i) => s"""  "$m" -> ${okay.codec.Json.write(i)(using s)}""").mkString("\n")
}
