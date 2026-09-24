package okay.scala2

import okay.{+, Handler}
import okay.agent.{Agent, Compact, Context, Durable, Handlers, Reply, ToolCall, Toolbox, Turn}
import okay.agent.{Model as ModelEffect, Tool as ToolEffect}
import okay.codec.Schema
import okay.given

/**
 * The agent layer for Scala 2.13 (specs/scala2-facade.md, stage 9).
 *
 * Probed first: okay-agent's data is readable from scalac 2.13 —
 * `Turn`, `Reply`, `ToolCall`, `ToolSpec`, `Toolbox`, `Handlers`,
 * `Provider` — as long as nothing touches a `Json` field (a tool call's
 * `args`, a spec's `schema`), because `okay.codec.Json` itself is
 * unreadable. What is not usable is the agent PROGRAM (`String ! Agent`,
 * a union row) and the assembly of its handlers. So this file gives:
 * a `Model` (scripted, or a real provider), `Tools` (a tool's arguments
 * reach Scala 2 as a case class, or as JSON text), a `Policy` for the
 * context, and `Chat`, the agent loop with a conversation that persists
 * across turns. Each is a call into okay-agent.
 */

/** a language model, as okay-agent's `Model` handler */
final class Model private (private[scala2] val handler: Handler[ModelEffect])

object Model {

  /** answers in order, one per completion, with no tool calls; then "done" */
  def scripted(replies: String*): Model =
    new Model(Handlers.scripted(replies.map(r => Reply(r, Nil))))

  /** answers in order, each with tool calls given as `(tool name, JSON arguments)` */
  def scriptedCalls(replies: (String, Seq[(String, String)])*): Model =
    new Model(Handlers.scripted(replies.zipWithIndex.map { case ((text, calls), i) =>
      Reply(text, calls.zipWithIndex.map { case ((name, args), j) =>
        ToolCall(s"c$i-$j", name, okay.codec.Json.parse(args)) })
    }))

  /** Anthropic's Messages API over okay-llm's HTTP transport */
  def anthropic(apiKey: String, model: String, maxTokens: Int = 1024): Model =
    new Model(okay.agent.Provider.anthropic(okay.llm.Transports.http(), apiKey, model, maxTokens = maxTokens))

  /** any OpenAI-compatible chat endpoint (`url` is the chat completions URL) */
  def openAi(apiKey: String, model: String, url: String = okay.llm.OpenAi.chatUrl): Model =
    new Model(okay.agent.Provider.openAi(okay.llm.Transports.http(), apiKey, model, url = url))
}

/** a tool call as Scala 2 can read it: the arguments as JSON text */
final case class Call(id: String, name: String, argsJson: String)

/** tools, declared and implemented in one place, as okay-agent's `Toolbox` */
final class Tools private (private[scala2] val box: Toolbox) {

  /** a tool whose arguments decode into `A` by the same `Schema` that
   * declares them to the model */
  def on[A](name: String, description: String)(run: A => String)(using s: Schema[A]): Tools =
    new Tools(box.on[A](name, description)(run))

  /** the declarations sent to the model: name, description, and the
   * JSON Schema of the arguments as text */
  def declarations: Seq[(String, String, String)] =
    box.specs.map(sp => (sp.name, sp.description, okay.codec.Json.print(sp.schema)))
}

object Tools {
  val empty: Tools = new Tools(Toolbox.empty)
}

/** how the conversation is kept within the model's context */
final class Policy private (private[scala2] val make: () => (() => Seq[Turn], Handler[Context]))

object Policy {

  /** everything, verbatim */
  val all: Policy = new Policy(() => { val (st, h) = Handlers.context(Compact.all); (() => st.recall, h) })

  /** system turns pinned, the oldest evicted past `budget` tokens (about
   * four characters each), the eviction reported to the model */
  def window(budget: Int): Policy =
    new Policy(() => { val (st, h) = Handlers.context(Compact.window(budget)(Compact.chars)); (() => st.recall, h) })
}

/**
 * A conversation with an agent. `say` runs okay-agent's loop — ask the
 * model, run the tools it calls, repeat until it answers — and the
 * conversation carries over to the next `say`, kept by the `Policy`.
 * `approve` decides each tool call before it runs; a denied call is
 * answered "denied", and the model sees that.
 *
 * With a `journal` the agent is DURABLE (okay-agent's `Durable.tools`):
 * every tool call is written down with its answer, and a new `Chat` over
 * the same journal, after a crash or a restart, replays the calls that
 * already happened instead of running them again. `onRepeat` says, per
 * tool name, what a call whose outcome the journal does not know
 * means: run it again (`Redo`), refuse (`Fail`, the default), and so on.
 */
final class Chat private (model: Model, tools: Tools, policy: Policy, maxSteps: Int, approve: Call => Boolean,
                          journal: Option[Durable.Journal], onRepeat: String => Durable.OnRepeat) {

  private val (recall, context) = policy.make()

  private val tool: Handler[ToolEffect] = {
    val gated = Handlers.gated(tools.box.table)(c => approve(Chat.call(c)))
    journal.fold(gated)(j => Durable.tools(gated, j)(onRepeat))
  }

  /** one user message; the agent's final answer */
  def say(message: String): Eff[Async, String] = Async(synchronized {
    // one handler per effect, assembled along the row, as okay-agent's
    // own tests do (TestAgent.run)
    given modelH: Handler[ModelEffect] = model.handler
    given toolH: Handler[ToolEffect] = tool
    given contextH: Handler[Context] = context
    given contextAsyncH: Handler[Context + okay.Async] = Handler.union[Context, okay.Async]
    given modelContextAsyncH: Handler[ModelEffect + (Context + okay.Async)] = Handler.union[ModelEffect, Context + okay.Async]
    given agentH: Handler[Agent] = Handler.union[ToolEffect, ModelEffect + (Context + okay.Async)]
    Agent.converse(message, tools.box.specs, maxSteps).runWith
  })

  /** the conversation so far, as the policy presents it to the model */
  def transcript: Seq[Turn] = recall()
}

object Chat {

  def apply(model: Model, tools: Tools = Tools.empty, policy: Policy = Policy.window(4000),
            maxSteps: Int = 8, approve: Call => Boolean = _ => true,
            journal: Option[Durable.Journal] = None,
            onRepeat: String => Durable.OnRepeat = _ => Durable.OnRepeat.Fail): Chat =
    new Chat(model, tools, policy, maxSteps, approve, journal, onRepeat)

  private def call(c: ToolCall): Call = Call(c.id, c.name, okay.codec.Json.print(c.args))
}
