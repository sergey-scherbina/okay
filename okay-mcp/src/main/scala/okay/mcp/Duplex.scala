package okay.mcp

import okay.Handler
import okay.agent.{Model, Reply, Turn}
import okay.codec.Json

/**
 * The shapes of the duplex half: what a server asks a client for, and
 * what either side says unasked. Same rule as the rest of the module
 * — the protocol's concepts land on types this library already has,
 * and the mapping lives here, in one place.
 */
object Duplex {

  import Rpc.{field, obj, str}

  /**
   * What a client answers when a server asks. Both fields are things
   * the client already had lying around: a list of roots, and the
   * `Handler[Model]` an agent in this process is already using.
   */
  final case class Peer(roots: Seq[Mcp.Root] = Nil,
                        sample: Option[Handler[Model]] = None)

  def rootJson(r: Mcp.Root): Json =
    obj("uri" -> Json.JStr(r.uri), "name" -> Json.JStr(r.name))

  def rootsResult(rs: Seq[Mcp.Root]): Json =
    obj("roots" -> Json.JArr(rs.map(rootJson).toVector))

  def rootsOf(result: Json): Seq[Mcp.Root] =
    field(result, "roots") match
      case Some(Json.JArr(vs)) => vs.flatMap(v =>
        str(v, "uri").map(u => Mcp.Root(u, str(v, "name").getOrElse("")))).toSeq
      case _ => Nil

  /**
   * A sampling request, as our own conversation: the messages are
   * turns (the same decoding prompts use), and `systemPrompt` — which
   * MCP keeps beside the messages rather than in them — becomes the
   * `Turn.System` it is.
   */
  def samplingTurns(params: Json): Seq[Turn] =
    val system = str(params, "systemPrompt").map(Turn.System(_)).toSeq
    val messages = field(params, "messages") match
      case Some(Json.JArr(vs)) => vs.flatMap(McpDocs.turnOf).toSeq
      case _ => Nil
    system ++ messages

  /** and the request itself, for a server that wants to ask */
  def samplingParams(turns: Seq[Turn], maxTokens: Int = 1024): Json =
    val (system, rest) = turns.partition {
      case Turn.System(_) => true
      case _ => false
    }
    val fs = Vector(
      "messages" -> Json.JArr(rest.map(McpDocs.messageJson).toVector),
      "maxTokens" -> Json.JNum(maxTokens.toDouble))
    Json.JObj(system.headOption match
      case Some(Turn.System(t)) => fs :+ ("systemPrompt" -> Json.JStr(t))
      case _ => fs)

  /** what the client's model said, on the wire */
  def samplingResult(reply: Reply, model: String = "okay"): Json = obj(
    "role" -> Json.JStr("assistant"),
    "content" -> obj("type" -> Json.JStr("text"), "text" -> Json.JStr(reply.text)),
    "model" -> Json.JStr(model),
    "stopReason" -> Json.JStr("endTurn"))

  /** and back — a server reading what it asked for */
  def replyOf(result: Json): Reply =
    Reply(field(result, "content").flatMap(c => str(c, "text")).getOrElse(""), Nil)

  /** the uri a resources/updated notification is about */
  def updatedUri(n: Rpc.Notify): Option[String] = str(n.params, "uri")

  def updated(uri: String): Rpc.Notify =
    Rpc.Notify(Mcp.ResourceUpdated, obj("uri" -> Json.JStr(uri)))
}
