package okay.agent

import okay.{!, %, +, Async, Writer, effect, pure}
import okay.given
import okay.codec.Json

/**
 * Agents as programs (specs/llm-agentic.md). Three effects and
 * nothing else: ask the model, call a tool, remember/recall the
 * conversation. The agent NEVER holds a message list — it performs
 * Context operations, and the HANDLER owns the policy: what stays in
 * context, who executes a tool, what a failure means. That inversion
 * is what makes compaction automatic instead of exceptional, tool
 * policy swappable per environment, and the whole agent testable
 * without a mocking framework — a different handler IS the mock.
 */

/** one turn of the conversation — what the model may see */
enum Turn:
  case System(text: String)
  case User(text: String)
  case Assistant(text: String, calls: Seq[ToolCall] = Nil)
  case Result(call: String, content: String)
  /** a compaction artifact: what a range of turns became */
  case Summary(text: String, covers: Int)

/** the model asked for a tool, by name, with JSON arguments */
final case class ToolCall(id: String, name: String, args: Json)

/** what one completion answered */
final case class Reply(text: String, calls: Seq[ToolCall])

/** the model effect: completion, and the local token count */
enum Model[+A]:
  case Complete(context: Seq[Turn], tools: Seq[ToolSpec]) extends Model[Reply]
  case Count(text: String) extends Model[Int]

/** one typed tool invocation — the handler decides what that means */
enum Tool[+A]:
  case Call(call: ToolCall) extends Tool[String]

/**
 * The conversation as effects. Recall answers a view that is ALREADY
 * within budget: compaction happens on every turn by construction,
 * not when someone remembers to trigger it. Mark/Restore make the
 * context backtrackable — the state is a value, so a rollback is a
 * pointer, not an undo log.
 */
enum Context[+A]:
  case Remember(turn: Turn) extends Context[Unit]
  case Recall() extends Context[Seq[Turn]]
  case Mark() extends Context[Snapshot]
  case Restore(mark: Snapshot) extends Context[Unit]

/** an opaque handle to a context state (a persistent value) */
final class Snapshot(private[agent] val state: Any)

/**
 * The row an agent lives in. The ORDER is deliberate: peeling from
 * the left is how a platform without threads runs the same program —
 * tools and context are program transformers, then the model becomes
 * Async by relay, and what is left is driven by the event loop.
 */
type Agent = Tool + (Context + (Model + Async))

object Agent {

  /** ask the model with the current (compacted) context */
  def complete(tools: Seq[ToolSpec] = Nil): Reply ! Agent =
    recall.flatMap(ctx => effect[Agent, Reply](Model.Complete(ctx, tools)))

  /** count tokens locally (the BPE Scan, no network) */
  def count(text: String): Int ! Agent = effect(Model.Count(text))

  def call(c: ToolCall): String ! Agent = effect(Tool.Call(c))

  def remember(t: Turn): Unit ! Agent = effect(Context.Remember(t))

  def recall: Seq[Turn] ! Agent = effect(Context.Recall())

  def mark: Snapshot ! Agent = effect(Context.Mark())

  def restore(s: Snapshot): Unit ! Agent = effect(Context.Restore(s))

  /** run every requested tool and remember each result */
  def runTools(calls: Seq[ToolCall]): Unit ! Agent =
    calls.foldLeft(pure[Agent, Unit](())) { (acc, c) =>
      acc.flatMap(_ => call(c).flatMap(r => remember(Turn.Result(c.id, r))))
    }

  /**
   * The loop: remember the message, ask (recall compacts on the way),
   * remember the reply, run the tools, repeat while the model keeps
   * calling tools. Note what is NOT here — no message list, no
   * truncation, no "if context too big" branch.
   */
  def converse(message: String, tools: Seq[ToolSpec] = Nil,
               maxSteps: Int = 8): String ! Agent =
    def step(n: Int): String ! Agent =
      complete(tools).flatMap { reply =>
        remember(Turn.Assistant(reply.text, reply.calls)).flatMap { _ =>
          if reply.calls.isEmpty || n <= 0 then pure(reply.text)
          else runTools(reply.calls).flatMap(_ => step(n - 1))
        }
      }

    remember(Turn.User(message)).flatMap(_ => step(maxSteps))
}
