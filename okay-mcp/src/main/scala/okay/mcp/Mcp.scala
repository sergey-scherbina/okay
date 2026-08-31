package okay.mcp

import okay.agent.{ToolCall, ToolSpec}
import okay.codec.Json

/**
 * The protocol's own vocabulary, and nothing else: method names, the
 * handshake, and the two shapes that carry a tool across the wire.
 * Pure — no transport, no session, no I/O. What a provider's dialect
 * is to okay-llm, this is to MCP: the one place that knows the
 * protocol calls a tool's schema `inputSchema` (Anthropic says
 * `input_schema`, OpenAI says `parameters`, and none of that reaches
 * the agent).
 */
object Mcp {

  import Rpc.{field, obj, str}

  /** the revision this implementation speaks; a client asking for
   * another is answered with ours, which is what the handshake is for */
  val Version = "2025-06-18"

  /** who is at each end — both sides send one */
  final case class Info(name: String, version: String)

  // the methods, by name
  val Initialize = "initialize"
  val Initialized = "notifications/initialized"
  val ToolsList = "tools/list"
  val ToolsCall = "tools/call"
  val Ping = "ping"

  def infoJson(i: Info): Json =
    obj("name" -> Json.JStr(i.name), "version" -> Json.JStr(i.version))

  def infoOf(j: Json): Option[Info] =
    for n <- str(j, "name"); v <- str(j, "version") yield Info(n, v)

  /** what a client sends to open a session */
  def initializeParams(client: Info): Json = obj(
    "protocolVersion" -> Json.JStr(Version),
    "capabilities" -> obj(),
    "clientInfo" -> infoJson(client))

  /**
   * What a server answers. `capabilities.tools` is the honest
   * declaration that this server serves tools and nothing else — a
   * client reading it knows not to ask for resources or prompts,
   * which is precisely why those being out of scope is a scope
   * decision rather than a hole.
   */
  def initializeResult(server: Info): Json = obj(
    "protocolVersion" -> Json.JStr(Version),
    "capabilities" -> obj("tools" -> obj("listChanged" -> Json.JBool(false))),
    "serverInfo" -> infoJson(server))

  /** a tool declaration on the wire — the schema is the DERIVED one */
  def toolJson(t: ToolSpec): Json = obj(
    "name" -> Json.JStr(t.name),
    "description" -> Json.JStr(t.description),
    "inputSchema" -> t.schema)

  /** and back: an entry of tools/list as our own ToolSpec */
  def toolSpec(j: Json): Option[ToolSpec] =
    str(j, "name").map(n => ToolSpec(n,
      str(j, "description").getOrElse(""),
      field(j, "inputSchema").getOrElse(obj("type" -> Json.JStr("object")))))

  /** the tools of a tools/list result, and the cursor to continue */
  def toolsOf(result: Json): (Seq[ToolSpec], Option[String]) =
    val tools = field(result, "tools") match
      case Some(Json.JArr(vs)) => vs.flatMap(toolSpec).toSeq
      case _ => Nil
    (tools, str(result, "nextCursor"))

  /** the page of a tools/list answer */
  def toolsResult(tools: Seq[ToolSpec], nextCursor: Option[String] = None): Json =
    val fs = Vector("tools" -> Json.JArr(tools.map(toolJson).toVector))
    Json.JObj(nextCursor.fold(fs)(c => fs :+ ("nextCursor" -> Json.JStr(c))))

  /** what a client sends to call one */
  def callParams(c: ToolCall): Json =
    obj("name" -> Json.JStr(c.name), "arguments" -> c.args)

  /** and back, on the server side; the id is the request's, which the
   * caller supplies — the wire carries no call id of its own */
  def callOf(params: Json, id: String): Option[ToolCall] =
    str(params, "name").map(n =>
      ToolCall(id, n, field(params, "arguments").getOrElse(obj())))

  /**
   * A tool's answer, as MCP carries it: a list of content blocks plus
   * an error flag. Our `Tool.Call` answers a String, so this is the
   * whole mapping — and the error flag becomes the `error: ...`
   * prefix that `Handlers.tools` already uses, because a failing tool
   * is an ANSWER the model must be able to recover from, not a fault
   * that ends the conversation.
   */
  def contentResult(text: String, isError: Boolean = false): Json = obj(
    "content" -> Json.JArr(Vector(
      obj("type" -> Json.JStr("text"), "text" -> Json.JStr(text)))),
    "isError" -> Json.JBool(isError))

  /** the text of a result, whatever blocks it came in */
  def textOf(result: Json): String =
    val text = field(result, "content") match
      case Some(Json.JArr(vs)) => vs.flatMap(b => str(b, "text")).mkString("\n")
      case _ => ""
    field(result, "isError") match
      case Some(Json.JBool(true)) => if text.startsWith("error:") then text else "error: " + text
      case _ => text
}
