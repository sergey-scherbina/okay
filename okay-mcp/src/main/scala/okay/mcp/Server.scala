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
  /**
   * Who is watching what. The one mutable thing in the server, and it
   * is mutable for the same reason a Channel is: a subscription is
   * made by a message arriving and read by a push going out, and
   * those are two different threads of control.
   */
  final class Subscriptions:
    private val set = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    def add(uri: String): Unit = { set.add(uri); () }
    def remove(uri: String): Unit = { set.remove(uri); () }
    def has(uri: String): Boolean = set.contains(uri)
    def all: Set[String] = { import scala.jdk.CollectionConverters.*; set.asScala.toSet }

  final case class Serving(info: Mcp.Info,
                           tools: Seq[ToolSpec] = Nil,
                           call: Map[String, ToolCall => String] = Map.empty,
                           /** tools that are PROGRAMS in Async (`Toolbox.In[Async].table`):
                            * answered by `run(link, serving)` through `serveIn`; a pure
                            * `serve` cannot run them and does not see them */
                           callF: Map[String, ToolCall => String ! Async] = Map.empty,
                           resources: Seq[Mcp.Resource] = Nil,
                           read: String => Option[String] = _ => None,
                           prompts: Seq[Mcp.Prompt] = Nil,
                           prompt: (String, Map[String, String]) => Option[Seq[Turn]] =
                             (_, _) => None,
                           subscriptions: Subscriptions = Subscriptions(),
                           complete: Option[Mcp.Complete => Vector[String]] = None,
                           templates: Seq[Mcp.Template] = Nil):

    /**
     * The same server with only the tools this caller may use.
     *
     * ABSENT, NOT REFUSED. `tools` and `call` are narrowed TOGETHER,
     * so the list and the table cannot disagree by construction: a
     * name this caller may not use is one this server does not have
     * for them, and asking for it by name answers the same
     * "no such tool" a misspelling answers. The other design is a
     * check inside each handler, which has to be written once per
     * tool and remembered forever — and the one nobody wrote is the
     * hole (specs/security.md stage 7).
     *
     * Narrowed to nothing, the server declares no tools at all:
     * `serve` computes its capabilities from what is actually there,
     * so the handshake stays honest about a caller who has none.
     */
    def only(allowed: String => Boolean): Serving =
      copy(tools = tools.filter(t => allowed(t.name)),
        call = call.filter((n, _) => allowed(n)),
        callF = callF.filter((n, _) => allowed(n)))

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
    serveIn[Pure](s)(c => pure(run(s.call, c)))

  /** the row `serveIn` runs in: the protocol's two, and the tools' G */
  type Row[G[+_]] = Take % Rpc + (Writer % Rpc + G)

  /**
   * What stands AROUND every request (specs/x402.md stage 3): it sees a
   * request before the protocol does and either answers it itself —
   * `Left`, which is final — or passes it on with a `leave` that sees
   * the protocol's reply and says what actually goes out.
   *
   * A hook at the REQUEST, not at the tool, because what needs one
   * lives in the parts `ToolCall` drops: x402 reads its payment from
   * `params._meta` and writes its receipt into `result._meta`, and
   * its transport spec prices any request at all, `initialize` and
   * `resources/read` included. And one hook, not two: whatever
   * `before` learned (a verified payment) is what `after` must act
   * on (settle it), so `leave` is a closure over it rather than a
   * second callback that would need a table keyed by request id.
   */
  trait Around[G[+_]]:
    def apply(r: Rpc.Request): Either[Rpc, Around.Pass[G]] ! G

  object Around:
    /** go on with `request`; `leave` turns the reply into what is sent */
    final case class Pass[G[+_]](request: Rpc.Request, leave: Rpc => Rpc ! G)

    /** nothing around: every request passes, every reply goes out as is */
    def none[G[+_]]: Around[G] = r => pure(Right(Pass[G](r, out => pure(out))))

  /**
   * THE PROTOCOL, GENERIC IN THE TOOLS' EFFECT (specs/optics-outside.md
   * stage 8): one implementation of the stage, in a row that carries
   * `G` beside `Take` and `Writer`, with `runTool` the only place `G`
   * is performed — `serve` is this at `Pure`, which the union absorbs,
   * and `run(link, serving)` is this at `Async` with `Serving.callF`
   * answered. Written once so a tool that does I/O does not fork the
   * protocol into two copies that drift.
   */
  def serveIn[G[+_]](s: Serving, around: Around[G] = Around.none[G])
                     (runTool: ToolCall => Json ! G): Unit ! Row[G] =
    val info = s.info
    def tell(m: Rpc): Unit ! Row[G] = !.widen[Unit, Take % Rpc + Writer % Rpc, G](Stage.tell[Rpc, Rpc](m))
    def await: Option[Rpc] ! Row[G] = !.widen[Option[Rpc], Take % Rpc + Writer % Rpc, G](Stage.await[Rpc, Rpc])
    def lift[X](p: X ! G): X ! Row[G] = !.widen[X, G, Take % Rpc + Writer % Rpc](p)
    def tool(c: ToolCall): Json ! Row[G] = lift(runTool(c))
    // What a server DECLARES is exactly what it answers: a method of
    // a capability it does not have is `MethodNotFound`, not a polite
    // empty list. A client that read the handshake never asks; one
    // that asks anyway learns something true.
    val hasTools = s.tools.nonEmpty || s.call.nonEmpty
    val hasResources = s.resources.nonEmpty || s.templates.nonEmpty
    val hasPrompts = s.prompts.nonEmpty
    val hasCompletions = s.complete.isDefined
    // every reply to a request goes out through `reply`, which is
    // where the request's `leave` gets to see it
    def step(ready: Boolean, msg: Rpc, reply: Rpc => Unit ! Row[G]): Boolean ! Row[G] =
      def answer(id: Json, result: Json): Unit ! Row[G] = reply(Rpc.Answer(id, result))
      def fail(id: Json, code: Int, message: String): Unit ! Row[G] = reply(Rpc.Failed(id, code, message))
      msg match {
        case Rpc.Request(id, Mcp.Initialize, _) =>
          answer(id, Mcp.initializeResult(info,
            tools = hasTools,
            resources = hasResources,
            prompts = hasPrompts,
            completions = hasCompletions)).map(_ => true)

        case Rpc.Notify(Mcp.Initialized, _) => pure(ready)

        // a notification is by definition unanswered — including the
        // ones we do not implement
        case Rpc.Notify(_, _) => pure(ready)

        case Rpc.Request(id, Mcp.Ping, _) => answer(id, obj()).map(_ => ready)

        case Rpc.Request(id, m, _) if !ready =>
          fail(id, Rpc.InvalidRequest,
            s"'$m' before initialize").map(_ => ready)

        // BEFORE the two handlers below, and that order is the rule this
        // file states: "a method of a capability it does not have is
        // MethodNotFound, not a polite empty list". Written after them
        // it was DEAD — a tool-less server answered `tools/list` with
        // an empty list and `tools/call` with "no such tool", which is
        // the polite empty list the comment refuses, and the branch
        // could not be reached to say otherwise. Found 2026-09-19 by
        // the first caller for which a tool-less server is ORDINARY
        // rather than a curiosity: `Serving.only`, narrowing a server
        // to what one caller may use (specs/security.md stage 7).
        case Rpc.Request(id, m, _)
          if (m == Mcp.ToolsList || m == Mcp.ToolsCall) && !hasTools =>
          fail(id, Rpc.MethodNotFound, m).map(_ => ready)

        case Rpc.Request(id, Mcp.ToolsList, _) =>
          answer(id, Mcp.toolsResult(s.tools)).map(_ => ready)

        case Rpc.Request(id, Mcp.ToolsCall, params) =>
          Mcp.callOf(params, Json.print(id)) match
            case None => fail(id, Rpc.InvalidParams, "no tool name").map(_ => ready)
            case Some(c) => tool(c).flatMap(j => answer(id, j)).map(_ => ready)

        case Rpc.Request(id, m, _)
          if (m == Mcp.ResourcesList || m == Mcp.ResourcesRead ||
              m == Mcp.ResourcesTemplates ||
              m == Mcp.ResourcesSubscribe || m == Mcp.ResourcesUnsubscribe) && !hasResources =>
          fail(id, Rpc.MethodNotFound, m).map(_ => ready)

        case Rpc.Request(id, m, _)
          if (m == Mcp.PromptsList || m == Mcp.PromptsGet) && !hasPrompts =>
          fail(id, Rpc.MethodNotFound, m).map(_ => ready)

        case Rpc.Request(id, Mcp.CompletionComplete, params) => s.complete match
          case None => fail(id, Rpc.MethodNotFound, Mcp.CompletionComplete).map(_ => ready)
          case Some(f) => McpDocs.completeOf(params) match
            case None => fail(id, Rpc.InvalidParams, "no ref/argument").map(_ => ready)
            case Some(c) => answer(id, McpDocs.completionResult(f(c))).map(_ => ready)

        case Rpc.Request(id, Mcp.ResourcesList, _) =>
          answer(id, McpDocs.resourcesResult(s.resources)).map(_ => ready)

        case Rpc.Request(id, Mcp.ResourcesTemplates, _) =>
          answer(id, McpDocs.templatesResult(s.templates)).map(_ => ready)

        // subscribe/unsubscribe are the client asking to be TOLD, and
        // what it is told goes out through Pushes, beside the answers
        case Rpc.Request(id, Mcp.ResourcesSubscribe, params) =>
          Rpc.str(params, "uri") match
            case None => fail(id, Rpc.InvalidParams, "no uri").map(_ => ready)
            case Some(uri) =>
              s.subscriptions.add(uri)
              answer(id, Rpc.obj()).map(_ => ready)

        case Rpc.Request(id, Mcp.ResourcesUnsubscribe, params) =>
          Rpc.str(params, "uri") match
            case None => fail(id, Rpc.InvalidParams, "no uri").map(_ => ready)
            case Some(uri) =>
              s.subscriptions.remove(uri)
              answer(id, Rpc.obj()).map(_ => ready)

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
        case f: Rpc.Failed => tell(f).map(_ => ready)

        // an answer arriving at a server answers a request it never
        // made: nothing to do with it, and nothing to say about it
        case Rpc.Answer(_, _) => pure(ready)
      }

    def go(ready: Boolean): Unit ! Row[G] = await.flatMap {
      case Some(r: Rpc.Request) => lift(around(r)).flatMap {
        case Left(now) => tell(now).map(_ => ready)
        case Right(p) => step(ready, p.request, out => lift(p.leave(out)).flatMap(tell))
      }.flatMap(go)
      case Some(msg) => step(ready, msg, tell).flatMap(go)
      case None => pure(())
    }

    go(false)

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

  /**
   * What a server says without being asked.
   *
   * A push is a message that does not answer anything, so it cannot
   * come out of the stage — a stage only speaks when spoken to. It
   * goes on a channel instead, and the wire's outbound side is the
   * stage's answers MERGED with that channel: `mergeSources`, used
   * for exactly what it was built for, one fiber each and whoever is
   * ready goes first.
   */
  final class Pushes private[mcp] (out: Channel[Rpc], subs: Subscriptions) {
    /** tell every subscriber to this uri that it changed */
    def resourceUpdated(uri: String): Unit =
      if subs.has(uri) then out.offer(Duplex.updated(uri)): Unit

    /** the list itself changed (tools, resources or prompts) */
    def listChanged(what: String): Unit =
      out.offer(Rpc.Notify(what, Rpc.obj())): Unit

    /** anything at all, for a server with its own ideas */
    def push(m: Rpc): Unit = out.offer(m): Unit

    /** nothing more will be pushed */
    def close(): Unit = out.close()
  }

  /**
   * The only part that touches a wire: lines in through the framing,
   * messages through the stage, lines back out. Everything above is
   * testable without it.
   */
  def over(link: Link)(stage: Stage[Rpc, Rpc, Unit]): Unit ! Async =
    drain(link)(through[Rpc, Rpc, Async, Unit, Unit](framed(link))(
      !.widen[Unit, Take % Rpc + Writer % Rpc, Async](stage)))

  /** the incoming lines, framed as messages */
  private def framed(link: Link): Source[Rpc] =
    through[String, Rpc, Async, Unit, Unit](link.lines)(
      !.widen[Unit, Take % String + Writer % Rpc, Async](Rpc.messages))

  /** everything told, put on the wire */
  private def drain(link: Link)(p: Source[Rpc]): Unit ! Async =
    Writer.uncons[Rpc, Unit, Async](p).flatMap {
      case Left(_) => pure(())
      case Right((m, rest)) => link.send(Rpc.encode(m)).flatMap(_ => drain(link)(rest))
    }

  /** the whole server, from a tool table: framing, protocol, wire */
  def run(link: Link, info: Mcp.Info, tools: Seq[ToolSpec],
          table: Map[String, ToolCall => String]): Unit ! Async =
    over(link)(serve(info, tools, table))

  /** the whole server, from everything it has — including its
   * effectful tools (`callF`), which run in the wire's own Async */
  def run(link: Link, serving: Serving): Unit ! Async =
    overIn(link)(serveIn[Async](serving)(answering(serving)))

  /** `run`, with something around every request — a payment gate */
  def run(link: Link, serving: Serving, around: Around[Async]): Unit ! Async =
    overIn(link)(serveIn[Async](serving, around)(answering(serving)))

  /** how `run` answers a call: a pure tool as before (`isError` on a
   * throw), an effectful one as its program — whose own failure is
   * its own row's business, since a program cannot be `try`-caught
   * from outside and the server carries no Scheduler to `attempt` it */
  def answering(serving: Serving): ToolCall => Json ! Async = c =>
    serving.callF.get(c.name) match
      case Some(f) if !serving.call.contains(c.name) => f(c).map(Mcp.contentResult(_))
      case _ => pure(run(serving.call, c))

  /** `over`, for a stage that already carries Async — `serveIn[Async]` */
  def overIn(link: Link)(stage: Unit ! Row[Async]): Unit ! Async =
    drain(link)(through[Rpc, Rpc, Async, Unit, Unit](framed(link))(stage))

  /** the pushing half alone, for a transport that owns its own
   * outbound channel (the HTTP route fans one out to many sessions) */
  def pushesTo(out: Channel[Rpc], subs: Subscriptions): Pushes = Pushes(out, subs)

  /**
   * The server, plus the handle for what it says unasked. The
   * outbound side is the stage's answers merged with the pushes —
   * two sources, one wire, by readiness.
   */
  def duplex(link: Link, serving: Serving)
            (using Scheduler, CanBlock): (Unit ! Async, Pushes) =
    val out = Channel[Rpc]()
    val answers: Source[Rpc] =
      through[Rpc, Rpc, Async, Unit, Unit](framed(link))(
        !.widen[Unit, Take % Rpc + Writer % Rpc, Async](serve(serving)))
    (drain(link)(answers merge Writer.of(out)), Pushes(out, serving.subscriptions))
}
