package okay.mcp

import okay.*
import okay.given
import okay.agent.{ToolCall, ToolSpec}
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
   * The protocol, as a stage.
   *
   * `table` is the same `Map[String, ToolCall => String]` that
   * `Handlers.tools` takes — a server and a local tool table are the
   * same thing seen from two sides, which is why serving costs
   * nothing to a program that already had tools.
   */
  def serve(info: Mcp.Info, tools: Seq[ToolSpec],
            table: Map[String, ToolCall => String]): Stage[Rpc, Rpc, Unit] =
    val stage: Stage[Rpc, Rpc, Boolean] =
      Stage.transduce(false)((ready, msg) => msg match {
        case Rpc.Request(id, Mcp.Initialize, _) =>
          answer(id, Mcp.initializeResult(info)).map(_ => true)

        case Rpc.Notify(Mcp.Initialized, _) => pure(ready)

        // a notification is by definition unanswered — including the
        // ones we do not implement
        case Rpc.Notify(_, _) => pure(ready)

        case Rpc.Request(id, Mcp.Ping, _) => answer(id, obj()).map(_ => ready)

        case Rpc.Request(id, m, _) if !ready =>
          fail(id, Rpc.InvalidRequest,
            s"'$m' before initialize").map(_ => ready)

        case Rpc.Request(id, Mcp.ToolsList, _) =>
          answer(id, Mcp.toolsResult(tools)).map(_ => ready)

        case Rpc.Request(id, Mcp.ToolsCall, params) =>
          Mcp.callOf(params, Json.print(id)) match
            case None => fail(id, Rpc.InvalidParams, "no tool name").map(_ => ready)
            case Some(c) => answer(id, run(table, c)).map(_ => ready)

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
}
