package okay.agent

import okay.{!, ==>, Aggregator, Handler}
import okay.given
import okay.rag.{Retriever, Scored}

/**
 * Retrieval-augmented RECALL (specs/rag.md, P10f): the agent does not
 * ask for code, `recall` already contains it.
 *
 * That follows from the design rather than being bolted onto it —
 * retrieved segments ARE turns, so assembling "conversation plus
 * relevant code" is the SAME Aggregator with the SAME budget as
 * plain compaction. The trade-off between history and passages
 * becomes an explicit, testable policy instead of two subsystems
 * quietly fighting over one context window (which is what a
 * RetrievalAugmentor and a ChatMemory do when they do not know about
 * each other).
 *
 * The saving is a round trip per turn: the common case costs no tool
 * call and no tokens spent asking for one. An explicit search tool
 * stays available for when the agent wants to steer.
 */
object Grounded {

  /** a retrieved passage, as a turn the model can read */
  def turn(hit: Scored): Turn =
    Turn.System(s"[${hit.segment.source} ${hit.segment.path.mkString("/")}]\n" +
      hit.segment.text)

  /**
   * A context handler that grounds every recall: the conversation is
   * compacted by `policy`, the last user turn drives a retrieval, and
   * both go through ONE budget — passages first (they are context,
   * not dialogue), then as much conversation as still fits.
   *
   * `share` says how much of the budget retrieval may take; the rest
   * belongs to the conversation, and neither side can silently starve
   * the other.
   */
  def context[S](policy: Aggregator[Turn, S, Seq[Turn]],
                 retriever: Retriever[okay.Pure],
                 budget: Int, share: Double = 0.5, k: Int = 4,
                 // what the model ACTUALLY sees is assembled here and
                 // nowhere else, so without a seam it is invisible —
                 // which the first application built on this API
                 // noticed within a minute (it printed the
                 // conversation and wondered where the code went)
                 onRecall: Seq[Turn] => Unit = _ => ())
                (size: Turn => Int): (Handlers.ContextState[S], Handler[Context]) =
    val st = Handlers.ContextState(policy)
    val forRetrieval = (budget * share).toInt

    def lastQuestion(turns: Seq[Turn]): Option[String] =
      turns.reverse.collectFirst { case Turn.User(t) => t }

    /** as many passages as the retrieval share allows */
    def passages(query: String): Seq[Turn] =
      val hits = okay.!.run(retriever.retrieve(query, k))
      hits.map(turn).foldLeft((Vector.empty[Turn], 0)) { (acc, t) =>
        val (kept, used) = acc
        val cost = size(t)
        if used + cost <= forRetrieval then (kept :+ t, used + cost) else acc
      }._1

    (st, new Handler[Context]:
      def handle[A](e: Context[A]): A = e match
        case Context.Remember(t) => st.remember(t)
        case Context.Recall() =>
          val conversation = st.recall
          lastQuestion(conversation) match
            case None => onRecall(conversation); conversation
            case Some(q) =>
              val found = passages(q)
              // the conversation gets what retrieval did not take
              val left = budget - found.map(size).sum
              val kept = conversation.foldRight((Vector.empty[Turn], 0)) { (t, acc) =>
                val (keep, used) = acc
                val cost = size(t)
                if used + cost <= left then (t +: keep, used + cost) else acc
              }._1
              val view = found ++ kept
              onRecall(view)
              view
        case Context.Mark() => st.mark
        case Context.Restore(s) => st.restore(s))

  /**
   * The same grounding as a NATURAL TRANSFORMATION rather than a
   * comonadic handler:
   *
   *     Context ==> ([X] =>> X ! F)
   *
   * `Handler[Context]` is `Context ==> Id`, and `Id` is precisely
   * where a suspension cannot go: a comonadic handler must ANSWER, so
   * it must finish, so its retriever must already be pure. That is
   * the constraint `context` above carries, and it is a real one — it
   * is why a network-backed embedder could not be used for grounded
   * recall at all.
   *
   * Valued in a PROGRAM, the operation may answer with more
   * computation instead of with a value. So `Recall()` returns the
   * retrieval as a program in `F`, `!.translate` forwards `F` to the
   * surrounding row, and it is handled OUT THERE — where there is a
   * driver, and where parking or an event loop is available. The
   * retriever may now be `Retriever[Async]`, `Retriever[Embed]`, or
   * anything else; nothing about the grounding policy changes.
   *
   * This is the same three-point line the core already draws:
   * `F ==> Id` (comonadic), `F ==> ([X] =>> X ! G)` (this), and
   * `F !> S` (Cont-valued, adding abort and multi-shot).
   */
  def translating[S, F[+_]](policy: Aggregator[Turn, S, Seq[Turn]],
                            retriever: Retriever[F],
                            budget: Int, share: Double = 0.5, k: Int = 4,
                            onRecall: Seq[Turn] => Unit = _ => ())
                           (size: Turn => Int)
  : (Handlers.ContextState[S], Context ==> ([X] =>> X ! F)) =
    val st = Handlers.ContextState(policy)
    val forRetrieval = (budget * share).toInt

    def lastQuestion(turns: Seq[Turn]): Option[String] =
      turns.reverse.collectFirst { case Turn.User(t) => t }

    /** exactly the assembly `context` does — the policy is the same
     * function; only where the retrieval RUNS has moved */
    def assemble(conversation: Seq[Turn], hits: Seq[Scored]): Seq[Turn] =
      val found = hits.map(turn).foldLeft((Vector.empty[Turn], 0)) { (acc, t) =>
        val (kept, used) = acc
        val cost = size(t)
        if used + cost <= forRetrieval then (kept :+ t, used + cost) else acc
      }._1
      val left = budget - found.map(size).sum
      val kept = conversation.foldRight((Vector.empty[Turn], 0)) { (t, acc) =>
        val (keep, used) = acc
        val cost = size(t)
        if used + cost <= left then (t +: keep, used + cost) else acc
      }._1
      val view = found ++ kept
      onRecall(view)
      view

    val nt: Context ==> ([X] =>> X ! F) =
      [X] => (e: Context[X]) => (e match
        case Context.Remember(t) => okay.pure[F, Unit](st.remember(t))
        case Context.Recall() =>
          val conversation = st.recall
          lastQuestion(conversation) match
            case None => onRecall(conversation); okay.pure[F, Seq[Turn]](conversation)
            case Some(q) => retriever.retrieve(q, k).map(assemble(conversation, _))
        case Context.Mark() => okay.pure[F, Snapshot](st.mark)
        case Context.Restore(s) => okay.pure[F, Unit](st.restore(s))
      ).asInstanceOf[X ! F]

    (st, nt)
}
