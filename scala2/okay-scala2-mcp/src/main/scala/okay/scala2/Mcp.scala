package okay.scala2

import okay.{!, Channel}
import okay.given
import okay.agent.{ToolCall, Turn}
import okay.codec.Json
import okay.mcp.{Client, Link, Server, Session, Stdio}

/*
 * okay-mcp for Scala 2.13 (specs/scala2-facade.md, stage 15.5).
 *
 * Probed first. A `Link` is built from Scala 2 directly
 * (`Stdio.of(in, out)`, `Stdio.of(process)`, `Stdio.spawn(command)`), and
 * so are the protocol's plain values (`okay.mcp.Mcp.Info`, `.Resource`,
 * `.Prompt`). What Scala 2 cannot use: every session operation answers a
 * program, and a tool's call and declaration carry okay-codec's `Json`,
 * which the TASTy reader refuses outright. Here JSON crosses as TEXT, as
 * it does in okay-scala2-agent, and a server's tools are that module's
 * `Tools`, so one declaration serves a local agent and an MCP server.
 */

/** a tool as a server declares it; `schema` is its arguments' JSON Schema, as text */
final case class McpTool(name: String, description: String, schema: String)

/** an open MCP session */
final class McpClient private[scala2] (session: Session) {

  /** the server's name and version, from the handshake */
  def server: Option[(String, String)] = session.server.map(i => (i.name, i.version))

  def tools: Eff[Async, Seq[McpTool]] =
    Async.lift(session.tools.map(_.map(t => McpTool(t.name, t.description, Json.print(t.schema)))))

  /** call a tool with its arguments as a JSON object, as text; the answer is its text */
  def call(name: String, argsJson: String): Eff[Async, String] =
    Async.lift(session.call(ToolCall(java.util.UUID.randomUUID().toString, name, Json.parse(argsJson))))

  def resources: Eff[Async, Seq[okay.mcp.Mcp.Resource]] = Async.lift(session.resources)

  /** a resource's text, or None if the server has no such uri */
  def read(uri: String): Eff[Async, Option[String]] = Async.lift(session.read(uri))

  def prompts: Eff[Async, Seq[okay.mcp.Mcp.Prompt]] = Async.lift(session.prompts)

  /** a prompt, filled in: the conversation opening it stands for */
  def prompt(name: String, args: Map[String, String] = Map.empty): Eff[Async, Seq[Turn]] =
    Async.lift(session.prompt(name, args))
}

object McpClient {
  /** open a session over `link`: the reader, the `initialize`
   * handshake, and the notification the protocol requires after it */
  def connect(link: Link, name: String, version: String): Eff[Async, McpClient] =
    Async.lift(Client.connect(link, okay.mcp.Mcp.Info(name, version)).map(new McpClient(_)))

  /** start `command` as a process and open a session over its stdio */
  def spawn(command: Seq[String], name: String, version: String): Eff[Async, McpClient] =
    Async(Stdio.of(Stdio.spawn(command))).flatMap(link => connect(link, name, version))
}

object McpServer {
  /**
   * Serve `tools` (okay-scala2-agent's `Tools`) and `resources` (uri ->
   * text) over `link` until the link closes. Run it on its own fiber:
   * `Async.fork(McpServer.run(...))`.
   */
  def run(link: Link, name: String, version: String, tools: Tools,
          resources: Map[String, String] = Map.empty): Eff[Async, Unit] =
    Async.lift(Server.run(link, Server.Serving(
      okay.mcp.Mcp.Info(name, version),
      tools = tools.box.specs,
      call = tools.box.table,
      resources = resources.keys.toSeq.sorted.map(u => okay.mcp.Mcp.Resource(u, u)),
      read = resources.get)))
}

object McpLink {
  /** two ends of an in-process link: a server on one, a client on the
   * other. Lines travel through channels, not a byte stream. */
  def pair(): (Link, Link) = {
    val up = Channel[String]()
    val down = Channel[String]()
    (end(up, down), end(down, up))
  }

  private def end(out: Channel[String], in: Channel[String]): Link = new Link {
    def send(line: String): Unit ! okay.Async = out.send(line).map(_ => ())
    def lines: okay.Source[String] = okay.Writer.of(in)
  }

  /** a link over a process's stdin and stdout, or any pair of streams */
  def of(in: java.io.InputStream, out: java.io.OutputStream): Link = Stdio.of(in, out)
}
