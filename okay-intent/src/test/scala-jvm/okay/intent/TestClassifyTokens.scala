package okay.intent

import okay.given
import okay.{Async, Handler, Writer}
import okay.codec.Schema
import okay.llm.{OpenAi, Structured, Transports}

/**
 * What the early stop actually saves (specs/intent-classify.md).
 *
 * The spec has been claiming since the first lane that `Structured.cut`
 * stops generation at the closing brace, "so a classification costs the
 * answer" — and saying, honestly but unsatisfyingly, that the saving
 * was reasoned about rather than measured. This measures it.
 *
 * Two streamed calls per message: one walked with `cut`, which stops
 * pulling the moment the value decodes, and one drained to the end.
 * Temperature is whatever the server defaults it to and the same
 * request goes out twice, so the comparison rests on the server being
 * deterministic — which it has been across every run in this lane, and
 * which the `stopped` column would expose if it were not.
 */
class TestClassifyTokens extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(900, "s")

  import IntentFixture.Meeting
  private given sMeeting: Schema[Meeting] = summon[Schema[Meeting]]
  private val mReading = Classify.reading[Meeting]

  val url = sys.env.getOrElse("OKAY_LLM_URL", "http://127.0.0.1:8089/v1/chat/completions")
  val model = sys.env.getOrElse("OKAY_LLM_MODEL",
    "claude-rozum-mlx-community-Qwen3-5-4B-MLX-4bit")
  val key = sys.env.getOrElse("OKAY_LLM_KEY", "none")

  lazy val reachable: Boolean =
    try
      val c = java.net.URI.create(url.replace("/chat/completions", "/models")).toURL.openConnection()
      c.setConnectTimeout(1500); c.setReadTimeout(1500); c.getInputStream.close(); true
    catch case _: Throwable => false

  private def body(message: String) =
    OpenAi.request(model,
      Seq(OpenAi.message("user",
        Classify.prompt[Meeting](message, IntentFixture.meetingExamples))),
      stream = true, maxTokens = Some(1200))

  /** the whole stream: how many tokens, and how much text. The text
   * length is the decisive column — if it equals what the cut had
   * already accumulated, the model wrote nothing after the closing
   * brace and there was never anything to save. */
  private def drain(b: String): (Int, Int) =
    Writer.run[String, Unit, Async](
      OpenAi.stream(Transports.http(), key, b, url))
      .map((toks, _) => (toks.length, toks.mkString.length)).runWith

  /** the same question asked WITHOUT "and nothing else" — the shape
   * the early stop was designed for, and the one our own prompt talks
   * the model out of */
  private def chattyBody(message: String) =
    OpenAi.request(model,
      Seq(OpenAi.message("user",
        s"Classify this message. Give the answer as a JSON object of this shape, " +
        s"and explain your reasoning around it:\n${Classify.example(using mReading)}\n\n" +
        s"Message: $message")),
      stream = true, maxTokens = Some(1200))

  test("live: what the early stop saves, in tokens") {
    assume(reachable, s"no OpenAI-compatible endpoint at $url")
    given Handler[Async] = summon[Handler[Async]]

    // twenty messages is a mean, not a census: each one costs two
    // streamed completions and the box is shared
    val sample = IntentFixture.labelled.grouped(6).map(_.head)
      .take(sys.env.get("OKAY_TOKENS_N").map(_.toInt).getOrElse(20)).toList

    var cutTotal = 0
    var fullTotal = 0
    var stoppedCount = 0
    var decoded = 0
    var chattyCut = 0
    var chattyFull = 0
    for (message, _) <- sample do
      val b = body(message)
      val c = Structured.cut[Reading[Meeting]](
        OpenAi.stream(Transports.http(), key, b, url))(using mReading, summon[Handler[Async]])
      val (full, fullChars) = drain(b)
      cutTotal += c.tokens
      fullTotal += full
      if c.stopped then stoppedCount += 1
      if c.value.isDefined then decoded += 1
      println(f"  strict  cut=${c.tokens}%4d full=$full%4d  chars ${c.text.length}%5d vs $fullChars%5d  stopped=${c.stopped}")

      // and the same message asked in a way that invites prose
      val cb = chattyBody(message)
      val cc = Structured.cut[Reading[Meeting]](
        OpenAi.stream(Transports.http(), key, cb, url))(using mReading, summon[Handler[Async]])
      val (cfull, cfullChars) = drain(cb)
      chattyCut += cc.tokens
      chattyFull += cfull
      println(f"  chatty  cut=${cc.tokens}%4d full=$cfull%4d  chars ${cc.text.length}%5d vs $cfullChars%5d  decoded=${cc.value.isDefined}")

    def pct(cut: Int, full: Int) = if full == 0 then 0.0 else (1.0 - cut.toDouble / full) * 100
    println(f"\n[early stop] messages ${sample.length}   decoded $decoded   stopped early $stoppedCount")
    println(f"  strict prompt: cut $cutTotal vs generated $fullTotal  -> saved ${pct(cutTotal, fullTotal)}%.1f%%")
    println(f"  chatty prompt: cut $chattyCut vs generated $chattyFull  -> saved ${pct(chattyCut, chattyFull)}%.1f%%")

    // ours to assert: the walk ran and the stream was really streamed
    assert(fullTotal > 0, "no tokens arrived — the endpoint is not streaming")
    assert(cutTotal <= fullTotal, "the cut cannot cost more than the whole stream")
  }
}
