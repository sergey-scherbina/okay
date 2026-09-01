package okay.langchain4j

import okay.Handler
import okay.agent.{Model, Reply, ToolCall, ToolSpec, Turn}
import okay.codec.Json
import dev.langchain4j.data.message as M
import dev.langchain4j.agent.tool.{ToolExecutionRequest, ToolSpecification}
import dev.langchain4j.model.chat.ChatModel
import dev.langchain4j.model.chat.request.ChatRequest
import dev.langchain4j.model.chat.request.json.*
import dev.langchain4j.model.chat.response.ChatResponse
import scala.jdk.CollectionConverters.*

/**
 * The interop sentence's Model half (specs/llm-agentic.md): their
 * `ChatModel` becomes a `Handler[Model]`, so every program written
 * against the effect — compaction, search, grounding, the durable
 * journal — runs over langchain4j's provider breadth unchanged. The
 * mappings are pure and tested against a SCRIPTED ChatModel; the
 * handler is comonadic like Provider.openAi, for the same reason: a
 * virtual thread parks in their blocking client while the row above
 * stays a program.
 */
object Langchain4j {

  /** one turn as their message — mirroring Provider.message, choice
   * for choice (a Summary is context, and context is a system turn) */
  def message(t: Turn): M.ChatMessage = t match
    case Turn.System(s) => M.SystemMessage.from(s)
    case Turn.User(s) => M.UserMessage.from(s)
    case Turn.Assistant(s, Nil) => M.AiMessage.from(s)
    case Turn.Assistant(s, calls) =>
      val reqs = calls.map(c => ToolExecutionRequest.builder()
        .id(c.id).name(c.name).arguments(Json.print(c.args)).build()).asJava
      if s.isEmpty then M.AiMessage.from(reqs) else M.AiMessage.from(s, reqs)
    case Turn.Result(id, content) =>
      // our Result carries the call id, not the tool name; providers
      // match by id, and their type tolerates the absent name
      M.ToolExecutionResultMessage(id, "", content)
    case Turn.Summary(s, _) => M.SystemMessage.from(s)

  /** the FOURTH algebra's JSON schema, translated node for node into
   * their element tree; a key we do not map (like codec-defaults'
   * `default`) is dropped, never a failure */
  def element(j: Json): JsonSchemaElement = j match
    case Json.JObj(fs) =>
      val m = fs.toMap
      def str(k: String) = m.get(k).collect { case Json.JStr(s) => s }
      (str("type"), m.get("oneOf")) match
        case (_, Some(Json.JArr(cases))) =>
          JsonAnyOfSchema.builder().anyOf(cases.map(element).asJava).build()
        case (Some("object"), _) =>
          val b = JsonObjectSchema.builder()
          m.get("properties") match
            case Some(Json.JObj(props)) => props.foreach((n, p) => b.addProperty(n, element(p)))
            case _ => ()
          m.get("required") match
            case Some(Json.JArr(rs)) =>
              b.required(rs.collect { case Json.JStr(s) => s }.asJava)
            case _ => ()
          b.build()
        case (Some("array"), _) =>
          JsonArraySchema.builder()
            .items(m.get("items").map(element).getOrElse(JsonStringSchema.builder().build()))
            .build()
        case (Some("integer"), _) => JsonIntegerSchema.builder().build()
        case (Some("number"), _) => JsonNumberSchema.builder().build()
        case (Some("boolean"), _) => JsonBooleanSchema.builder().build()
        case _ => JsonStringSchema.builder().build()   // string, base64 bytes, chars
    case _ => JsonStringSchema.builder().build()

  /** a ToolSpec as their ToolSpecification — properties and required
   * intact, so a defaulted field stays omittable across the interop */
  def declaration(spec: ToolSpec): ToolSpecification =
    val params = element(spec.schema) match
      case o: JsonObjectSchema => o
      case _ => JsonObjectSchema.builder().build()   // a tool takes an object
    ToolSpecification.builder()
      .name(spec.name).description(spec.description).parameters(params)
      .build()

  /** what one of their responses means to the agent */
  def reply(r: ChatResponse): Reply =
    val ai = r.aiMessage()
    val calls = ai.toolExecutionRequests().asScala.toSeq.map { t =>
      ToolCall(Option(t.id()).getOrElse(""), t.name(),
        Json.parse(Option(t.arguments()).filter(_.nonEmpty).getOrElse("{}")))
    }
    Reply(Option(ai.text()).getOrElse(""), calls)

  /**
   * Their model as our handler. `count` stays LOCAL (chars/4, or
   * Provider.counting(bpe)) — the compaction budget never costs a
   * round trip, and their Tokenizer is deliberately not consulted.
   */
  def model(chat: ChatModel, count: String => Int = _.length / 4): Handler[Model] = new:
    def handle[A](e: Model[A]): A = e match
      case Model.Complete(context, tools) =>
        val b = ChatRequest.builder().messages(context.map(message).asJava)
        val req = if tools.isEmpty then b else b.toolSpecifications(tools.map(declaration).asJava)
        reply(chat.chat(req.build()))
      case Model.Count(text) => count(text)

  /** the wiring form (ctx-everywhere): the handler awaiting its
   * environment — store it, ship it, provide(chatModel){ use it } */
  def wired(count: String => Int = _.length / 4): dev.langchain4j.model.chat.ChatModel ?=> Handler[Model] =
    model(summon[dev.langchain4j.model.chat.ChatModel], count)
}
