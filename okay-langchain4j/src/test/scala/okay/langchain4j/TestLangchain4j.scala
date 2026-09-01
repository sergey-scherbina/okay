package okay.langchain4j

import okay.agent.{Model, ToolCall, ToolSpec, Turn}
import okay.codec.{Json, Schema}
import dev.langchain4j.data.message as M
import dev.langchain4j.agent.tool.ToolExecutionRequest
import dev.langchain4j.data.message.AiMessage
import dev.langchain4j.model.chat.ChatModel
import dev.langchain4j.model.chat.request.ChatRequest
import dev.langchain4j.model.chat.request.json.JsonObjectSchema
import dev.langchain4j.model.chat.response.ChatResponse
import scala.jdk.CollectionConverters.*

/** a scripted ChatModel: answers from a queue, records every request
 * it saw — no network, which is the interop's own testing claim */
class FakeChat(script: AiMessage*) extends ChatModel {
  var seen = Vector.empty[ChatRequest]
  private var rest = script.toList
  override def doChat(req: ChatRequest): ChatResponse =
    seen = seen :+ req
    val head :: tail = rest: @unchecked
    rest = tail
    ChatResponse.builder().aiMessage(head).build()
}

class TestLangchain4j extends munit.FunSuite {

  final case class SearchArgs(query: String, limit: Int = 10, deep: Option[Boolean] = None)
  given Schema[SearchArgs] = Schema.derived
  val search = ToolSpec[SearchArgs]("search", "look something up")

  test("every Turn kind maps to the right ChatMessage; Summary rides as system") {
    assertEquals(Langchain4j.message(Turn.System("be brief")).asInstanceOf[M.SystemMessage].text(), "be brief")
    assertEquals(Langchain4j.message(Turn.User("hi")).asInstanceOf[M.UserMessage].singleText(), "hi")
    assertEquals(Langchain4j.message(Turn.Summary("earlier: greetings", 4)).asInstanceOf[M.SystemMessage].text(), "earlier: greetings")
    val ai = Langchain4j.message(Turn.Assistant("on it",
      Seq(ToolCall("c1", "search", Json.parse("""{"query":"q"}"""))))).asInstanceOf[M.AiMessage]
    assertEquals(ai.text(), "on it")
    assertEquals(ai.toolExecutionRequests().get(0).name(), "search")
    assertEquals(ai.toolExecutionRequests().get(0).arguments(), """{"query":"q"}""")
    val res = Langchain4j.message(Turn.Result("c1", "42")).asInstanceOf[M.ToolExecutionResultMessage]
    assertEquals(res.id(), "c1")
    assertEquals(res.text(), "42")
  }

  test("a derived ToolSpec arrives as theirs: properties and required intact, defaults unrequired") {
    val ts = Langchain4j.declaration(search)
    assertEquals(ts.name(), "search")
    val params = ts.parameters()
    assertEquals(params.properties().keySet().asScala.toSet, Set("query", "limit", "deep"))
    // limit has a default, deep is optional — only query is required,
    // on their side exactly as on ours (codec-defaults holds across)
    assertEquals(params.required().asScala.toSet, Set("query"))
  }

  test("the round trip against a scripted model: complete, tool request, result, complete") {
    val fake = FakeChat(
      AiMessage.from(ToolExecutionRequest.builder()
        .id("c1").name("search").arguments("""{"query":"okay"}""").build()),
      AiMessage.from("found it"))
    val h = Langchain4j.model(fake)

    val turns = Vector[Turn](Turn.System("use tools"), Turn.User("find okay"))
    val first = h.handle(Model.Complete(turns, Seq(search)))
    assertEquals(first.calls.map(c => (c.name, c.args)),
      Seq(("search", Json.parse("""{"query":"okay"}"""))))

    val second = h.handle(Model.Complete(
      turns :+ Turn.Assistant("", first.calls) :+ Turn.Result("c1", "one result"),
      Seq(search)))
    assertEquals(second, okay.agent.Reply("found it", Nil))

    // what THEY saw: our turns as their messages, our spec as theirs
    assertEquals(fake.seen.length, 2)
    val sent = fake.seen(1).messages().asScala.toVector
    assertEquals(sent.map(_.getClass.getSimpleName),
      Vector("SystemMessage", "UserMessage", "AiMessage", "ToolExecutionResultMessage"))
    assertEquals(fake.seen(0).toolSpecifications().get(0).name(), "search")
  }

  test("no tool calls is a plain Reply; null text is empty, not null") {
    val h = Langchain4j.model(FakeChat(AiMessage.from(
      ToolExecutionRequest.builder().id("x").name("t").arguments("").build())))
    val r = h.handle(Model.Complete(Vector(Turn.User("hi")), Nil))
    assertEquals(r.text, "")                          // AiMessage of only tool calls has null text
    assertEquals(r.calls.head.args, Json.parse("{}")) // empty arguments parse as the empty object
  }

  test("Count stays local — the fake is never consulted") {
    val fake = FakeChat()
    assertEquals(Langchain4j.model(fake).handle(Model.Count("12345678")), 2)
    assertEquals(fake.seen, Vector.empty)
  }
}
