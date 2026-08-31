package okay.mcp

import okay.*
import okay.given
import okay.agent.{ToolCall, ToolSpec, Turn}
import okay.codec.Json

/**
 * Our tools, served as an MCP server — and the server is a pure
 * `Stage[Rpc, Rpc, Unit]`: it awaits messages and tells messages,
 * which means the whole protocol is exercised in a test with no
 * process, no socket, no clock and no thread. `over` is the only part
 * that touches a wire, and it is six lines.
 *
 * The session state (has `initialize` happened) is the transducer's
 * parameter, like every other stage in this library.
 */
object Server {

  import Rpc.obj

  /**
   * Everything a server has, in one value.
   *
   * Each field is a type this library already used for the same idea:
   * tools are `ToolSpec` plus the table `Handlers.tools` takes,
   * resources are documents behind a `uri => text`, and a prompt is a
   * conversation opening — `Seq[Turn]`, which is what an agent's
   * context is made of. Nothing here is MCP's shape; the mapping to
   * it lives in `McpDocs` and is the only place that knows.
   */
  final case class Serving(info: Mcp.Info,
                           tools: Seq[ToolSpec] = Nil,
                           call: Map[String, ToolCall => String] = Map.empty,
                           resources: Seq[Mcp.Resource] = Nil,
                           read: String => Option[String] = _ => None,
                           prompts: Seq[Mcp.Prompt] = Nil,
                           prompt: (String, Map[String, String]) => Option[Seq[Turn]] =
                             (_, _) => None)

  /** the tools-only server, which is what most are */
  def serve(info: Mcp.Info, tools: Seq[ToolSpec],
            table: Map[String, ToolCall => String]): Stage[Rpc, Rpc, Unit] =
    serve(Serving(info, tools, table))

  /**
   * The protocol, as a stage.
   *
   * Capabilities are computed from what is actually there: a server
   * with no prompts does not advertise prompts. A client reads the
   * handshake and knows what to ask for, which is what makes an
   * unimplemented half of the protocol a scope decision rather than a
   * hole.
   */
  def serve(s: Serving): Stage[Rpc, Rpc, Unit] =
    val info = s.info
    // What a server DECLARES is exactly what it answers: a method of
    // a capability it does not have is `MethodNotFound`, not a polite
    // empty list. A client that read the handshake never asks; one
    // that asks anyway learns something true.
    val hasTools = s.tools.nonEmpty || s.call.nonEmpty
    val hasResources = s.resources.nonEmpty
    val hasPrompts = s.prompts.nonEmpty
    val stage: Stage[Rpc, Rpc, Boolean] =
      Stage.transduce(false)((ready, msg) => msg match {
        case Rpc.Request(id, Mcp.Initialize, _) =>
          answer(id, Mcp.initializeResult(info,
            tools = hasTools,
            resources = hasResources,
            prompts = hasPrompts)).map(_ => true)

        case Rpc.Notify(Mcp.Initialized, _) => pure(ready)

        // a notification is by definition unanswered — including the
        // ones we do not implement
        case Rpc.Notify(_, _) => pure(ready)

        case Rpc.Request(id, Mcp.Ping, _) => answer(id, obj()).map(_ => ready)

        case Rpc.Request(id, m, _) if !ready =>
          fail(id, Rpc.InvalidRequest,
            s"'$m' before initialize").map(_ => ready)

        case Rpc.Request(id, Mcp.ToolsList, _) =>
          answer(id, Mcp.toolsResult(s.tools)).map(_ => ready)

        case Rpc.Request(id, Mcp.ToolsCall, params) =>
          Mcp.callOf(params, Json.print(id)) match
            case None => fail(id, Rpc.InvalidParams, "no tool name").map(_ => ready)
            case Some(c) => answer(id, run(s.call, c)).map(_ => ready)

        case Rpc.Request(id, m, _)
          if (m == Mcp.ResourcesList || m == Mcp.ResourcesRead) && !hasResources =>
          fail(id, Rpc.MethodNotFound, m).map(_ => ready)

        case Rpc.Request(id, m, _)
          if (m == Mcp.PromptsList || m == Mcp.PromptsGet) && !hasPrompts =>
          fail(id, Rpc.MethodNotFound, m).map(_ => ready)

        case Rpc.Request(id, m, _)
          if (m == Mcp.ToolsList || m == Mcp.ToolsCall) && !hasTools =>
          fail(id, Rpc.MethodNotFound, m).map(_ => ready)

        case Rpc.Request(id, Mcp.ResourcesList, _) =>
          answer(id, McpDocs.resourcesResult(s.resources)).map(_ => ready)

        // an unknown URI is an ERROR, where an unknown TOOL is an
        // answer, and the difference is who asked: a model picks a
        // tool name and must be able to read its own mistake, while a
        // program asks for a uri it got from resources/list
        case Rpc.Request(id, Mcp.ResourcesRead, params) =>
          Rpc.str(params, "uri") match
            case None => fail(id, Rpc.InvalidParams, "no uri").map(_ => ready)
            case Some(uri) => s.read(uri) match
              case Some(text) =>
                answer(id, McpDocs.contentsResult(uri, text)).map(_ => ready)
              case None =>
                fail(id, Rpc.InvalidParams, s"no such resource '$uri'").map(_ => ready)

        case Rpc.Request(id, Mcp.PromptsList, _) =>
          answer(id, McpDocs.promptsResult(s.prompts)).map(_ => ready)

        case Rpc.Request(id, Mcp.PromptsGet, params) =>
          Rpc.str(params, "name") match
            case None => fail(id, Rpc.InvalidParams, "no prompt name").map(_ => ready)
            case Some(n) => s.prompt(n, McpDocs.argsOf(params)) match
              case Some(turns) =>
                val d = s.prompts.find(_.name == n).map(_.description).getOrElse("")
                answer(id, McpDocs.promptResult(d, turns)).map(_ => ready)
              case None =>
                fail(id, Rpc.InvalidParams, s"no such prompt '$n'").map(_ => ready)

        case Rpc.Request(id, m, _) =>
          fail(id, Rpc.MethodNotFound, m).map(_ => ready)

        // a damaged line arrives here as the Failed that decoding it
        // made, and JSON-RPC says the server owes exactly that error
        // back — so it is echoed, id and all (null, for a parse error)
        case f: Rpc.Failed => Stage.tell[Rpc, Rpc](f).map(_ => ready)

        // an answer arriving at a server answers a request it never
        // made: nothing to do with it, and nothing to say about it
        case Rpc.Answer(_, _) => pure(ready)
      }, pure)

    stage.map(_ => ())

  /**
   * Execute one call. An unknown tool and a throwing tool are both
   * ANSWERS with `isError`, never faults: the model asked for
   * something impossible and must be able to read that and try
   * again — the convention `Handlers.tools` already set, now on the
   * wire where the protocol has a field for it.
   */
  private def run(table: Map[String, ToolCall => String], c: ToolCall): Json =
    table.get(c.name) match
      case None => Mcp.contentResult(s"no such tool '${c.name}'", isError = true)
      case Some(f) =>
        try Mcp.contentResult(f(c))
        catch case e: Throwable =>
          Mcp.contentResult(Option(e.getMessage).getOrElse(e.toString), isError = true)

  private def answer(id: Json, result: Json): Stage[Rpc, Rpc, Unit] =
    Stage.tell[Rpc, Rpc](Rpc.Answer(id, result))

  private def fail(id: Json, code: Int, message: String): Stage[Rpc, Rpc, Unit] =
    Stage.tell[Rpc, Rpc](Rpc.Failed(id, code, message))

  /**
   * The only part that touches a wire: lines in through the framing,
   * messages through the stage, lines back out. Everything above is
   * testable without it.
   */
  def over(link: Link)(stage: Stage[Rpc, Rpc, Unit]): Unit ! Async =
    val framed: Unit ! (Writer % Rpc + Async) =
      through[String, Rpc, Async, Unit, Unit](link.lines)(
        !.widen[Unit, Take % String + Writer % Rpc, Async](Rpc.messages))
    val answered: Unit ! (Writer % Rpc + Async) =
      through[Rpc, Rpc, Async, Unit, Unit](framed)(
        !.widen[Unit, Take % Rpc + Writer % Rpc, Async](stage))

    def drain(p: Unit ! (Writer % Rpc + Async)): Unit ! Async =
      Writer.uncons[Rpc, Unit, Async](p).flatMap {
        case Left(_) => pure(())
        case Right((m, rest)) => link.send(Rpc.encode(m)).flatMap(_ => drain(rest))
      }

    drain(answered)

  /** the whole server, from a tool table: framing, protocol, wire */
  def run(link: Link, info: Mcp.Info, tools: Seq[ToolSpec],
          table: Map[String, ToolCall => String]): Unit ! Async =
    over(link)(serve(info, tools, table))

  /** the whole server, from everything it has */
  def run(link: Link, serving: Serving): Unit ! Async =
    over(link)(serve(serving))
}
