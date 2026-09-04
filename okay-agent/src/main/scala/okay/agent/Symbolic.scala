package okay.agent

import okay.lex.Span
import okay.rag.{Keyword, Postings, Segment}

/**
 * The symbolic first pass (specs/intent-classify.md).
 *
 * FrameNet's "lexical units" — the words that evoke a class — are, in
 * this stack, already a thing that exists: `Postings` is a `Fold` and
 * a `Monoid`, and `Keyword.search` scores BM25 over it. So a symbolic
 * tier is a PROJECTION of the retrieval machinery onto the labelled
 * examples, not new infrastructure, and building it cost this file.
 *
 * What it is for: answering the easy majority without a model call.
 * What decides whether it earns that: coverage at a margin, agreement
 * with gold on what it answers, and latency — measured in
 * `TestSymbolic`, on a split, because an index scored against its own
 * training set measures nothing.
 *
 * The DEFER is the whole design. A tier that always answers is a bad
 * classifier; a tier that answers only when the top class beats the
 * second by a margin is a cheap filter with a stated confidence, and
 * everything it declines still costs exactly what it cost before.
 */
object Symbolic {

  /** one class's evidence: every labelled example that carried it */
  final case class Trained(index: Postings, classOf: Vector[String])

  /** what the pass concluded, and how sure the margin makes it */
  final case class Verdict(best: String, margin: Double, runnerUp: Option[String])

  /**
   * Build from labelled examples. One document per example, so a class
   * with more evidence has more documents rather than one long one —
   * BM25 length-normalises, and a single concatenated document per
   * class would make a well-attested class look diluted.
   */
  def train(labelled: Seq[(String, String)]): Trained =
    val segs = labelled.zipWithIndex.map { case ((text, _), i) =>
      Segment(s"ex-$i", Span(0, 0, 0, text.length), text, Seq("example"))
    }
    Trained(Keyword.index(segs), labelled.map(_._2).toVector)

  /**
   * Score a message. The class of the best-matching example wins, and
   * the margin is the gap to the best example of ANY other class —
   * relative, so it is comparable across messages whose absolute BM25
   * scores differ by an order of magnitude.
   */
  def score(t: Trained, message: String, k: Int = 8): Option[Verdict] =
    val hits = Keyword.search(t.index, message, k)
    if hits.isEmpty then None
    else
      // the search returns segments; their source carries the index
      // they were built from, which is how a hit finds its label
      def labelOf(s: Segment): Option[String] =
        s.source.stripPrefix("ex-").toIntOption.flatMap(t.classOf.lift)
      val byClass = hits.flatMap(h => labelOf(h.segment).map(_ -> h.score.toDouble))
      if byClass.isEmpty then None
      else
        val best = byClass.head
        val other = byClass.find(_._1 != best._1)
        val margin = other match
          case Some((_, s)) => if best._2 <= 0.0 then 0.0 else (best._2 - s) / best._2
          case None => 1.0   // nothing else scored at all
        Some(Verdict(best._1, margin, other.map(_._1)))

  /**
   * The tier: answer when the margin clears `floor`, defer otherwise.
   *
   * `None` means "ask the model", and it is not a failure — it is the
   * tier doing the one thing that makes it safe to put in front of
   * something expensive.
   */
  def classify(t: Trained, message: String, floor: Double = 0.3): Option[String] =
    score(t, message).filter(_.margin >= floor).map(_.best)
}
