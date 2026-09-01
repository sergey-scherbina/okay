package okay.mcp

import okay.*
import okay.codec.Json

/**
 * JSON-RPC 2.0 as DATA. Four shapes, and which one a line is depends
 * only on the fields it carries: a `method` with an `id` is a call, a
 * `method` without one is a notification, a `result` is an answer and
 * an `error` is a failure.
 *
 * Decoding is TOTAL, and that is the whole point of doing it here
 * rather than with a decoder that throws: a stream that fails on one
 * damaged line loses every good line after it, and a subprocess that
 * writes one stray line to its stdout is not a reason to lose the
 * session. A line that is not JSON, or is JSON that is not a message,
 * decodes to `Failed` — which is also exactly the response JSON-RPC
 * says a server owes for a parse error, so the value is not a
 * consolation prize, it is the protocol's own answer.
 */
enum Rpc:
  /** a call that expects an answer */
  case Request(id: Json, method: String, params: Json)

  /** a call that does not */
  case Notify(method: String, params: Json)

  /** the answer to a request */
  case Answer(id: Json, result: Json)

  /** the failure of a request — or of the line that should have been one */
  case Failed(id: Json, code: Int, message: String)

object Rpc {

  /** the only version this protocol has ever had */
  val Version = "2.0"

  // the standard codes, by their standard names
  val ParseError = -32700
  val InvalidRequest = -32600
  val MethodNotFound = -32601
  val InvalidParams = -32602
  val InternalError = -32603

  /**
   * One line to one message; damage becomes a Failed, never a throw.
   *
   * The two JSON-RPC error codes map onto something this library
   * already has, and better than a throwing parser could: our JSON
   * parser is TOTAL, so a malformed line does not fail — it yields a
   * tree with `JErr` NODES where the damage is. So `ParseError`
   * (-32700, "not JSON") is "the tree contains damage", and
   * `InvalidRequest` (-32600, "JSON, but not a message") is "it
   * parsed clean and still has no method, result or error". A
   * throwing parser can only report the first of those, and only as
   * an exception.
   */
  def decode(line: String): Rpc = Json.parse(line) match
    case j if damaged(j) => Failed(Json.JNull, ParseError, describe(j))
    case j @ Json.JObj(_) =>
      val params = field(j, "params").getOrElse(Json.JObj(Vector.empty))
      (field(j, "method"), field(j, "id"), field(j, "result"), field(j, "error")) match
        case (Some(Json.JStr(m)), Some(id), _, _) => Request(id, m, params)
        case (Some(Json.JStr(m)), None, _, _) => Notify(m, params)
        case (None, id, Some(r), _) => Answer(id.getOrElse(Json.JNull), r)
        case (None, id, _, Some(e)) =>
          Failed(id.getOrElse(Json.JNull),
            field(e, "code").collect { case Json.JNum(n) => n.toInt }.getOrElse(InternalError),
            field(e, "message").collect { case Json.JStr(s) => s }.getOrElse(""))
        case _ => Failed(field(j, "id").getOrElse(Json.JNull), InvalidRequest,
          "not a JSON-RPC message: " + line)
    case _ => Failed(Json.JNull, InvalidRequest, "not a JSON-RPC object: " + line)

  /** a message to its line */
  def encode(m: Rpc): String = Json.print(json(m))

  /** a message as a Json value (the encoder's own algebra) */
  def json(m: Rpc): Json = m match
    case Request(id, method, params) => obj(
      "jsonrpc" -> Json.JStr(Version), "id" -> id,
      "method" -> Json.JStr(method), "params" -> params)
    case Notify(method, params) => obj(
      "jsonrpc" -> Json.JStr(Version),
      "method" -> Json.JStr(method), "params" -> params)
    case Answer(id, result) => obj(
      "jsonrpc" -> Json.JStr(Version), "id" -> id, "result" -> result)
    case Failed(id, code, message) => obj(
      "jsonrpc" -> Json.JStr(Version), "id" -> id,
      "error" -> obj("code" -> Json.JNum(code.toDouble), "message" -> Json.JStr(message)))

  /**
   * The framing, as a Stage: lines in, messages out. Blank lines are
   * framing, not damage — they are skipped; everything else answers a
   * message, which for a damaged line is `Failed`.
   */
  def messages: Stage[String, Rpc, Unit] =
    Stage.transduce(())((_, line: String) =>
      if line.trim.isEmpty then pure(())
      else Stage.tell[String, Rpc](decode(line)), pure)

  /** is there damage anywhere in this tree — the total parser's way
   * of saying what a throwing one says by throwing */
  def damaged(j: Json): Boolean = j match
    case Json.JErr(_) => true
    case Json.JArr(vs) => vs.exists(damaged)
    case Json.JObj(fs) => fs.exists((n, v) => n.startsWith("<unexpected") || damaged(v))
    case _ => false

  /** the first damage found, as a message */
  private def describe(j: Json): String = j match
    case Json.JErr(m) => m
    case Json.JArr(vs) => vs.collectFirst { case v if damaged(v) => describe(v) }.getOrElse("damaged")
    case Json.JObj(fs) => fs.collectFirst {
      case (n, _) if n.startsWith("<unexpected") => n
      case (_, v) if damaged(v) => describe(v)
    }.getOrElse("damaged")
    case _ => "damaged"

  /** a field of a JSON object, if it is one and has it */
  def field(j: Json, name: String): Option[Json] = j match
    case Json.JObj(fs) => fs.collectFirst { case (n, v) if n == name => v }
    case _ => None

  /** a string field, if it is one */
  def str(j: Json, name: String): Option[String] =
    field(j, name).collect { case Json.JStr(s) => s }

  /** a JSON object, spelled shortly — public because a transport or a
   * caller outside this package builds params too */
  def obj(fs: (String, Json)*): Json = Json.JObj(fs.toVector)
}
