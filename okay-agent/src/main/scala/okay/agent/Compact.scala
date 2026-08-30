package okay.agent

import okay.{Aggregator, Group}
import okay.given

/**
 * Context management as an ALGEBRA (specs/llm-agentic.md). A policy
 * is an `Aggregator[Turn, S, Seq[Turn]]` — our P1 type, unchanged —
 * and that single choice buys everything:
 *
 * - `add` folds ONE new turn: O(new tokens), not O(history), so
 *   applying the policy on every single turn is cheap. Compaction is
 *   the default path, not an emergency branch: being over budget only
 *   changes which view `present` returns.
 * - `merge` makes compaction HIERARCHICAL — summaries of two halves
 *   merge into a summary of the whole, so a long history compacts in
 *   parallel (a fiber per chunk) or across a cluster, same value.
 * - `zip` runs several policies in ONE pass (a token window, a
 *   running summary, a fact extractor).
 *
 * Token counts come from the BPE Scan in okay-llm — local, exact and
 * incremental, so a turn is counted once, as it arrives, and never
 * re-counted. The window is a `Group`: adding adds the count,
 * evicting subtracts it.
 */
object Compact {

  /** a turn with its measured cost — counted once, on the way in */
  final case class Sized(turn: Turn, tokens: Int)

  /** what a policy keeps: pinned turns, a running digest, a window */
  final case class Window(pinned: Vector[Sized], recent: Vector[Sized],
                          dropped: Int, droppedTokens: Int):
    def tokens: Int = pinned.map(_.tokens).sum + recent.map(_.tokens).sum

  /**
   * The token window: system turns are PINNED (never evicted), the
   * rest is a sliding window over the budget. Eviction subtracts —
   * the Group law is what keeps it O(1) per turn instead of a
   * re-count. What falls out is remembered as a count, so `present`
   * can tell the model that history was elided rather than pretend
   * the conversation began mid-sentence.
   */
  def window(budget: Int)(size: Turn => Int)
  : Aggregator[Turn, Window, Seq[Turn]] =
    val G = summon[Group[Int]]

    /** what the elision marker costs — it goes on the wire too, so
     * it must fit inside the budget, not beside it */
    def marker(w: Window): Option[Turn] =
      if w.dropped == 0 then None
      else Some(Turn.Summary(
        s"[${w.dropped} earlier turns elided, ${w.droppedTokens} tokens]", w.dropped))

    def cost(w: Window): Int = w.tokens + marker(w).fold(0)(size)

    def evict(w: Window): Window =
      if cost(w) <= budget || w.recent.isEmpty then w
      else
        val head = w.recent.head
        evict(Window(w.pinned, w.recent.tail,
          w.dropped + 1, G.combine(w.droppedTokens, head.tokens)))

    Aggregator[Turn, Window, Seq[Turn]](
      Window(Vector.empty, Vector.empty, 0, 0)) { (w, t) =>
      val s = Sized(t, size(t))
      t match
        case _: Turn.System => w.copy(pinned = w.pinned :+ s)
        case _ => evict(w.copy(recent = w.recent :+ s))
    } { (a, b) =>
      // merge is the distributed contract: two halves of a history
      // combine, then the window re-applies to the join
      evict(Window(a.pinned ++ b.pinned, a.recent ++ b.recent,
        a.dropped + b.dropped, G.combine(a.droppedTokens, b.droppedTokens)))
    } { w =>
      w.pinned.map(_.turn) ++ marker(w).toVector ++ w.recent.map(_.turn)
    }

  /** the simplest policy: keep everything (the baseline to compare) */
  def all: Aggregator[Turn, Vector[Turn], Seq[Turn]] =
    Aggregator[Turn, Vector[Turn], Seq[Turn]](Vector.empty)(_ :+ _)(_ ++ _)(identity)

  /** a crude size when no tokenizer is at hand (4 chars ~ 1 token) */
  def chars(t: Turn): Int = (text(t).length + 3) / 4

  /** the text a turn contributes to the context */
  def text(t: Turn): String = t match
    case Turn.System(s) => s
    case Turn.User(s) => s
    case Turn.Assistant(s, calls) => s + calls.map(_.name).mkString(" ")
    case Turn.Result(_, c) => c
    case Turn.Summary(s, _) => s
}
