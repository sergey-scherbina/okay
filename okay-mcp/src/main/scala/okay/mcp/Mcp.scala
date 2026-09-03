package okay.mcp

import okay.agent.{ToolCall, ToolSpec, Turn}
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
  val ResourcesList = "resources/list"
  val ResourcesRead = "resources/read"
  val PromptsList = "prompts/list"
  val PromptsGet = "prompts/get"
  val Ping = "ping"

  // the duplex half: what a SERVER asks a client, and what either
  // side says without being asked
  val ResourcesSubscribe = "resources/subscribe"
  val ResourcesUnsubscribe = "resources/unsubscribe"
  val RootsList = "roots/list"
  val SamplingCreate = "sampling/createMessage"
  val ElicitationCreate = "elicitation/create"
  val CompletionComplete = "completion/complete"

  /** what a completion is about: a prompt's argument, or a resource
   * template's (the template uri travels either way) */
  enum Ref:
    case Prompt(name: String)
    case Resource(uri: String)

  /** one completion request, as data: which ref, which argument, the
   * partial value typed, and the arguments already resolved */
  final case class Complete(ref: Ref, argument: String, value: String,
                            context: Map[String, String] = Map.empty)
  val ResourceUpdated = "notifications/resources/updated"
  val RootsChanged = "notifications/roots/list_changed"
  val ResourcesChanged = "notifications/resources/list_changed"
  val ToolsChanged = "notifications/tools/list_changed"
  val PromptsChanged = "notifications/prompts/list_changed"
  val Progress = "notifications/progress"
  val Cancelled = "notifications/cancelled"

  /** a directory or document tree the CLIENT offers the server */
  final case class Root(uri: String, name: String = "")

  /** a document a server offers, by uri — okay-rag's `Source` with
   * the protocol's metadata around it */
  final case class Resource(uri: String, name: String,
                            description: String = "",
                            mimeType: Option[String] = None)

  val ResourcesTemplates = "resources/templates/list"

  /** a resource TEMPLATE: one declaration standing for unbounded
   * uris — RFC 6570, level 1 in this implementation */
  final case class Template(uriTemplate: String, name: String,
                            description: String = "",
                            mimeType: Option[String] = None)

  object Template:
    /** {var} expansion with percent-encoding (RFC 6570 level 1) */
    def expand(template: String, vars: Map[String, String]): String =
      val out = StringBuilder()
      var i = 0
      while i < template.length do
        val c = template.charAt(i)
        if c == '{' then
          val end = template.indexOf('}', i)
          if end < 0 then { out.append(template.substring(i)); i = template.length }
          else
            val name = template.substring(i + 1, end)
            out.append(vars.get(name).fold("")(encode))
            i = end + 1
        else { out.append(c); i += 1 }
      out.toString

    /** the REVERSE: does this uri fit the template, and with which
     * variables? Never guesses — a leftover or a mismatch is None */
    def matches(template: String, uri: String): Option[Map[String, String]] =
      def go(ti: Int, ui: Int, acc: Map[String, String]): Option[Map[String, String]] =
        if ti >= template.length then
          if ui >= uri.length then Some(acc) else None
        else if template.charAt(ti) == '{' then
          val end = template.indexOf('}', ti)
          if end < 0 then None
          else
            val name = template.substring(ti + 1, end)
            // the variable runs to the next literal character (or the
            // end); level 1 variables do not cross '/' either way
            val next = if end + 1 < template.length then Some(template.charAt(end + 1)) else None
            val stop = next match
              case Some(ch) =>
                val at = uri.indexOf(ch, ui)
                if at < 0 then -1 else at
              case None => uri.length
            if stop < 0 then None
            else
              val raw = uri.substring(ui, stop)
              if raw.contains('/') then None
              else go(end + 1, stop, acc + (name -> decode(raw)))
        else if ui < uri.length && template.charAt(ti) == uri.charAt(ui) then
          go(ti + 1, ui + 1, acc)
        else None

      go(0, 0, Map.empty)

    private def encode(s: String): String =
      val keep = "-._~".toSet
      s.flatMap { ch =>
        if ch.isLetterOrDigit && ch < 128 || keep(ch) then ch.toString
        else ch.toString.getBytes("UTF-8").map(b => f"%%${b & 0xff}%02X").mkString
      }

    private def decode(s: String): String =
      try java.net.URLDecoder.decode(s.replace("+", "%2B"), "UTF-8")
      catch case _: IllegalArgumentException => s

  /** a conversation opening a server offers, by name */
  final case class Prompt(name: String, description: String = "",
                          arguments: Seq[Prompt.Arg] = Nil)

  object Prompt:
    final case class Arg(name: String, description: String = "",
                         required: Boolean = false)

  def infoJson(i: Info): Json =
    obj("name" -> Json.JStr(i.name), "version" -> Json.JStr(i.version))

  def infoOf(j: Json): Option[Info] =
    for n <- str(j, "name"); v <- str(j, "version") yield Info(n, v)

  /**
   * What a client sends to open a session — and its own capabilities,
   * because in a duplex protocol the client is a server too: a server
   * may only ask for roots or sampling if the client said it has
   * them.
   */
  def initializeParams(client: Info, roots: Boolean = false,
                       sampling: Boolean = false,
                       elicitation: Boolean = false): Json =
    val caps = Vector(
      Option.when(roots)("roots" -> obj("listChanged" -> Json.JBool(true))),
      Option.when(sampling)("sampling" -> obj()),
      Option.when(elicitation)("elicitation" -> obj())).flatten
    obj("protocolVersion" -> Json.JStr(Version),
      "capabilities" -> Json.JObj(caps),
      "clientInfo" -> infoJson(client))

  /**
   * What a server answers. `capabilities.tools` is the honest
   * declaration that this server serves tools and nothing else — a
   * client reading it knows not to ask for resources or prompts,
   * which is precisely why those being out of scope is a scope
   * decision rather than a hole.
   */
  def initializeResult(server: Info, tools: Boolean = true,
                       resources: Boolean = false,
                       prompts: Boolean = false,
                       completions: Boolean = false): Json =
    val caps = Vector(
      Option.when(tools)("tools" -> obj("listChanged" -> Json.JBool(false))),
      Option.when(resources)("resources" -> obj(
        "listChanged" -> Json.JBool(false), "subscribe" -> Json.JBool(false))),
      Option.when(prompts)("prompts" -> obj("listChanged" -> Json.JBool(false))),
      Option.when(completions)("completions" -> obj())
    ).flatten
    obj("protocolVersion" -> Json.JStr(Version),
      "capabilities" -> Json.JObj(caps),
      "serverInfo" -> infoJson(server))

  /** what the handshake said this server has */
  def capability(result: Json, name: String): Boolean =
    field(result, "capabilities").flatMap(field(_, name)).isDefined

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

// ------------------------------------------------------------------
// resources and prompts: the same list+fetch shape as tools, landing
// on the types this library already has — a resource is a document
// (okay-rag's Source), a prompt is a conversation opening (Seq[Turn])

object McpDocs {

  import Mcp.{Prompt, Resource}
  import Rpc.{field, obj, str}

  def resourceJson(r: Resource): Json =
    val fs = Vector(
      "uri" -> Json.JStr(r.uri),
      "name" -> Json.JStr(r.name),
      "description" -> Json.JStr(r.description))
    Json.JObj(r.mimeType.fold(fs)(m => fs :+ ("mimeType" -> Json.JStr(m))))

  def resourceOf(j: Json): Option[Resource] =
    str(j, "uri").map(u => Resource(u,
      str(j, "name").getOrElse(u),
      str(j, "description").getOrElse(""),
      str(j, "mimeType")))

  def resourcesResult(rs: Seq[Resource], nextCursor: Option[String] = None): Json =
    val fs = Vector("resources" -> Json.JArr(rs.map(resourceJson).toVector))
    Json.JObj(nextCursor.fold(fs)(c => fs :+ ("nextCursor" -> Json.JStr(c))))

  def resourcesOf(result: Json): (Seq[Resource], Option[String]) =
    val rs = field(result, "resources") match
      case Some(Json.JArr(vs)) => vs.flatMap(resourceOf).toSeq
      case _ => Nil
    (rs, str(result, "nextCursor"))

  /** the contents of one resource — one text block, which is what a
   * document is; binary blobs are a shape we do not serve */
  def contentsResult(uri: String, text: String, mimeType: String = "text/plain"): Json =
    obj("contents" -> Json.JArr(Vector(obj(
      "uri" -> Json.JStr(uri),
      "mimeType" -> Json.JStr(mimeType),
      "text" -> Json.JStr(text)))))

  /** and back: the text of whatever blocks came, joined */
  def contentsOf(result: Json): Option[String] =
    field(result, "contents") match
      case Some(Json.JArr(vs)) =>
        val texts = vs.flatMap(b => str(b, "text"))
        if texts.isEmpty then None else Some(texts.mkString("\n"))
      case _ => None

  def promptJson(p: Prompt): Json = obj(
    "name" -> Json.JStr(p.name),
    "description" -> Json.JStr(p.description),
    "arguments" -> Json.JArr(p.arguments.map(a => obj(
      "name" -> Json.JStr(a.name),
      "description" -> Json.JStr(a.description),
      "required" -> Json.JBool(a.required))).toVector))

  def promptOf(j: Json): Option[Prompt] =
    str(j, "name").map(n => Prompt(n, str(j, "description").getOrElse(""),
      field(j, "arguments") match
        case Some(Json.JArr(vs)) => vs.flatMap(a => str(a, "name").map(an =>
          Prompt.Arg(an, str(a, "description").getOrElse(""),
            field(a, "required").contains(Json.JBool(true))))).toSeq
        case _ => Nil))

  def promptsResult(ps: Seq[Prompt]): Json =
    obj("prompts" -> Json.JArr(ps.map(promptJson).toVector))

  def promptsOf(result: Json): Seq[Prompt] =
    field(result, "prompts") match
      case Some(Json.JArr(vs)) => vs.flatMap(promptOf).toSeq
      case _ => Nil

  /**
   * A conversation opening on the wire. MCP prompt messages carry one
   * of two roles, `user` and `assistant` — there is no system role
   * here — so a `Turn.System` travels as a user message and arrives
   * as one. The loss is the protocol's; dropping the turn would lose
   * more.
   */
  def messageJson(t: Turn): Json =
    val (role, text) = t match
      case Turn.Assistant(x, _) => ("assistant", x)
      case Turn.System(x) => ("user", x)
      case Turn.StatePatch(patch) => ("user", Json.print(patch))
      case Turn.User(x) => ("user", x)
      case Turn.Result(_, c) => ("user", c)
      case Turn.Summary(x, _) => ("user", x)
    obj("role" -> Json.JStr(role),
      "content" -> obj("type" -> Json.JStr("text"), "text" -> Json.JStr(text)))

  def turnOf(j: Json): Option[Turn] =
    val text = field(j, "content") match
      case Some(c) => str(c, "text")
      case None => None
    text.map(x => if str(j, "role").contains("assistant") then Turn.Assistant(x) else Turn.User(x))

  def promptResult(description: String, turns: Seq[Turn]): Json = obj(
    "description" -> Json.JStr(description),
    "messages" -> Json.JArr(turns.map(messageJson).toVector))

  def turnsOf(result: Json): Seq[Turn] =
    field(result, "messages") match
      case Some(Json.JArr(vs)) => vs.flatMap(turnOf).toSeq
      case _ => Nil

  /** resource templates on the wire, both directions */
  def templateJson(t: Mcp.Template): Json =
    val fs = Vector(
      "uriTemplate" -> Json.JStr(t.uriTemplate),
      "name" -> Json.JStr(t.name),
      "description" -> Json.JStr(t.description))
    Json.JObj(t.mimeType.fold(fs)(m => fs :+ ("mimeType" -> Json.JStr(m))))

  def templateOf(j: Json): Option[Mcp.Template] =
    str(j, "uriTemplate").map(u => Mcp.Template(u,
      str(j, "name").getOrElse(u), str(j, "description").getOrElse(""),
      str(j, "mimeType")))

  def templatesResult(ts: Seq[Mcp.Template]): Json =
    obj("resourceTemplates" -> Json.JArr(ts.map(templateJson).toVector))

  def templatesOf(result: Json): Seq[Mcp.Template] =
    field(result, "resourceTemplates") match
      case Some(Json.JArr(vs)) => vs.flatMap(templateOf).toSeq
      case _ => Nil

  /** completion/complete params, both directions */
  def completeParams(c: Mcp.Complete): Json =
    val ref = c.ref match
      case Mcp.Ref.Prompt(n) => obj("type" -> Json.JStr("ref/prompt"), "name" -> Json.JStr(n))
      case Mcp.Ref.Resource(u) => obj("type" -> Json.JStr("ref/resource"), "uri" -> Json.JStr(u))
    val base = Vector(
      "ref" -> ref,
      "argument" -> obj("name" -> Json.JStr(c.argument), "value" -> Json.JStr(c.value)))
    Json.JObj(if c.context.isEmpty then base else base :+ ("context" -> obj(
      "arguments" -> Json.JObj(c.context.toVector.map((k, v) => (k, Json.JStr(v)))))))

  def completeOf(params: Json): Option[Mcp.Complete] =
    val ref = field(params, "ref").flatMap { r =>
      str(r, "type") match
        case Some("ref/prompt") => str(r, "name").map(Mcp.Ref.Prompt(_))
        case Some("ref/resource") => str(r, "uri").map(Mcp.Ref.Resource(_))
        case _ => None
    }
    val arg = field(params, "argument")
    val context = field(params, "context").flatMap(field(_, "arguments")) match
      case Some(Json.JObj(fs)) => fs.collect { case (k, Json.JStr(v)) => (k, v) }.toMap
      case _ => Map.empty[String, String]
    for r <- ref; a <- arg; n <- str(a, "name")
    yield Mcp.Complete(r, n, str(a, "value").getOrElse(""), context)

  /** the answer: values capped at 100, hasMore telling the truth */
  def completionResult(values: Vector[String]): Json = obj(
    "completion" -> obj(
      "values" -> Json.JArr(values.take(100).map(Json.JStr(_))),
      "total" -> Json.JNum(values.length.toDouble),
      "hasMore" -> Json.JBool(values.length > 100)))

  def completionOf(result: Json): Vector[String] =
    field(result, "completion").flatMap(field(_, "values")) match
      case Some(Json.JArr(vs)) => vs.collect { case Json.JStr(s) => s }
      case _ => Vector.empty

  /** the arguments of a prompts/get, as a flat map of strings */
  def argsOf(params: Json): Map[String, String] =
    field(params, "arguments") match
      case Some(Json.JObj(fs)) => fs.collect { case (n, Json.JStr(v)) => (n, v) }.toMap
      case _ => Map.empty
}
