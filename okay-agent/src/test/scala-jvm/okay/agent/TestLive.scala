package okay.agent

import okay.{!, +, Async, Handler}
import okay.given
import okay.codec.{Json, Schema}
import okay.llm.{OpenAi, Transport, Transports}
import scala.collection.mutable

/**
 * The one thing every layer above the effect had never done: talk to
 * a real model. This runs against an OpenAI-compatible endpoint if
 * one is reachable (a local gateway, Ollama, vLLM, or the cloud with
 * a key) and is SKIPPED otherwise, so CI stays honest without needing
 * a model.
 *
 *   OKAY_LLM_URL   the chat-completions endpoint
 *   OKAY_LLM_MODEL the model id
 *   OKAY_LLM_KEY   optional bearer token
 *
 * What is being tested is not the model's intelligence — that is not
 * ours to assert — but that OUR assumptions about the protocol hold
 * against a real server: the request we build is accepted, the
 * response we decode carries what we expect, tool calls come back in
 * the shape the agent loop needs, and the loop closes.
 */
class TestLive extends munit.FunSuite {

  override val munitTimeout = scala.concurrent.duration.Duration(180, "s")

  val url = sys.env.getOrElse("OKAY_LLM_URL", "http://127.0.0.1:8089/v1/chat/completions")
  val model = sys.env.getOrElse("OKAY_LLM_MODEL",
    "claude-rozum-mlx-community-Qwen3-5-4B-MLX-4bit")
  val key = sys.env.getOrElse("OKAY_LLM_KEY", "none")

  /** is anything listening that speaks this protocol? */
  lazy val reachable: Boolean =
    try
      val u = java.net.URI.create(url.replace("/chat/completions", "/models"))
      val c = u.toURL.openConnection().asInstanceOf[java.net.HttpURLConnection]
      c.setConnectTimeout(1500)
      c.setReadTimeout(1500)
      c.getResponseCode == 200
    catch case _: Throwable => false

  def transport = Transports.http()

  def run[A](prog: A ! Agent)(model: Handler[Model], tool: Handler[Tool],
                              ctx: Handler[Context]): A =
    given Handler[Model] = model
    given Handler[Tool] = tool
    given Handler[Context] = ctx
    given rowMA: Handler[Model + Async] = okay.Handler.union[Model, Async]
    given rowCMA: Handler[Context + (Model + Async)] =
      okay.Handler.union[Context, Model + Async]
    given rowAll: Handler[Agent] = okay.Handler.union[Tool, Context + (Model + Async)]
    prog.runWith

  test("live: a completion comes back through our own decoder") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    val body = OpenAi.request(model,
      Seq(OpenAi.message("user", "Reply with exactly: ok")),
      maxTokens = Some(20))
    val r = OpenAi.complete(transport, key, body, url).runWith
    val text = r.choices.headOption.flatMap(_.message).flatMap(_.content).getOrElse("")
    assert(text.nonEmpty, s"the model answered nothing: $r")
  }

  test("live: the agent loop runs against a real model") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    val provider = Provider.openAi(transport, key, model, url, maxTokens = Some(64))
    val (state, ctx) = Handlers.context(Compact.all)
    val answer = run(Agent.converse(
      "Answer in one short sentence: what colour is the sky on a clear day?"))(
      provider, Handlers.tools(Map.empty), ctx)

    assert(answer.nonEmpty, "no answer came back")
    // and the conversation is what the compactor recorded
    assert(state.recall.exists {
      case Turn.Assistant(t, _) => t == answer
      case _ => false
    })
  }

  /** the real transport, with a tap on what actually went out */
  def watched(sent: mutable.Buffer[String]): Transport = new Transport:
    private val inner = Transports.http()
    def post(url: String, headers: Map[String, String], body: String) =
      sent += body
      inner.post(url, headers, body)

  test("live: compaction keeps a long conversation inside its budget") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    val sent = mutable.Buffer[String]()
    val provider = Provider.openAi(watched(sent), key, model, url, maxTokens = Some(32))
    val budget = 400
    val (_, ctx) = Handlers.context(Compact.window(budget)(Compact.chars))

    val prog = (1 to 3).foldLeft(okay.pure[Agent, String]("")) { (acc, i) =>
      acc.flatMap(_ => Agent.converse(s"Question $i: name one colour. " + "x" * 300))
    }
    run(prog)(provider, Handlers.tools(Map.empty), ctx)

    // the policy held against a REAL exchange: every body that went
    // out carried a compacted context, not the whole history
    assertEquals(sent.length, 3)
    for body <- sent do
      val messages = Json.parse(body) match
        case Json.JObj(fs) =>
          val m: Map[String, Json] = fs.toMap
          Json.print(m("messages"))
        case other => fail(s"the request was not an object: $other")
      // budget is in Compact.chars units (~4 chars each), plus the
      // JSON envelope around each message
      assert(messages.length < budget * 4 + 400,
        s"an uncompacted history reached the wire: ${messages.length} chars")
    // and the last request is not simply the first one repeated
    assert(sent.last != sent.head)
  }

  test("live: how close is the local token estimate to the provider's count?") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    // a prompt long enough that a constant overhead does not dominate
    val prompt = ("The quick brown fox jumps over the lazy dog. " * 12).trim
    val body = OpenAi.request(model,
      Seq(OpenAi.message("user", prompt)), maxTokens = Some(8))
    val r = OpenAi.complete(transport, key, body, url).runWith
    val reported = r.usage.flatMap(_.prompt_tokens)
    assume(reported.isDefined, "this endpoint reports no usage")

    val ours = Compact.chars(Turn.User(prompt))    // the chars/4 estimate
    val actual = reported.get
    val ratio = ours.toDouble / actual
    // The estimate is a heuristic, not a tokenizer: what is asserted
    // is that it is the right ORDER, so a budget built on it cannot
    // be wrong by a factor that matters. An exact count needs the
    // model's own merges table in `Bpe` — the honest remaining gap.
    assert(ratio > 0.4 && ratio < 2.0,
      s"the local estimate is off by more than 2x: ours=$ours provider=$actual")
    println(s"[live] prompt tokens: ours=$ours provider=$actual ratio=${"%.2f".format(ratio)}")
  }

  // ---- the same programs over the OTHER protocol

  val anthropicUrl = sys.env.getOrElse("OKAY_LLM_ANTHROPIC_URL",
    url.replace("/chat/completions", "/messages"))

  lazy val anthropicReachable: Boolean =
    try
      val body = okay.llm.Anthropic.messagesBody(model, None,
        Seq(okay.llm.Anthropic.blocks("user",
          Vector(okay.llm.Anthropic.textBlock("hi")))), Nil, 4)
      val c = java.net.URI.create(anthropicUrl).toURL.openConnection()
        .asInstanceOf[java.net.HttpURLConnection]
      c.setRequestMethod("POST")
      c.setConnectTimeout(2000)
      c.setReadTimeout(30000)
      c.setDoOutput(true)
      c.setRequestProperty("content-type", "application/json")
      c.setRequestProperty("anthropic-version", "2023-06-01")
      c.getOutputStream.write(body.getBytes("UTF-8"))
      c.getResponseCode == 200
    catch case _: Throwable => false

  test("live: the same agent runs over the Anthropic protocol too") {
    assume(anthropicReachable, s"no Anthropic-shaped endpoint at $anthropicUrl")
    val provider = Provider.anthropic(transport, key, model, anthropicUrl, maxTokens = 64)
    val (state, ctx) = Handlers.context(Compact.all)
    val prog = Agent.remember(Turn.System("Answer in one short sentence."))
      .flatMap(_ => Agent.converse("What colour is the sky on a clear day?"))
    val answer = run(prog)(provider, Handlers.tools(Map.empty), ctx)

    assert(answer.nonEmpty, "no answer came back over the Messages API")
    println(s"[live/anthropic] $answer")
  }

  case class Weather(city: String)
  given Schema[Weather] = Schema.derived

  test("live: a tool call round-trips, if the model supports tools") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    val spec = ToolSpec[Weather]("get_weather", "the current weather in a city")
    val provider = Provider.openAi(transport, key, model, url, maxTokens = Some(128))
    val calls = mutable.Buffer[ToolCall]()
    val tools = Handlers.recording(Handlers.tools(Map(
      "get_weather" -> { c =>
        ToolSpec.args[Weather](c).fold(e => s"bad args: $e", w => s"${w.city}: 20C, clear")
      })))(calls)
    val (_, ctx) = Handlers.context(Compact.all)

    val answer = run(Agent.converse(
      "Use the get_weather tool for Kyiv, then tell me the temperature.",
      Seq(spec)))(provider, tools, ctx)

    // the gateway was checked to emit tool_calls for this prompt, so
    // this is asserted outright rather than made conditional — a
    // conditional here would pass whatever happened, which is not a
    // test
    assertEquals(calls.map(_.name).toList, List("get_weather"))
    assertEquals(ToolSpec.args[Weather](calls.head), Right(Weather("Kyiv")),
      s"the arguments did not decode: ${Json.print(calls.head.args)}")
    assert(answer.nonEmpty, "the loop did not come back with an answer")
  }
}
