package okay.agent

import okay.{!, +, Handler, given}
import okay.codec.Json
import okay.lex.{Bpe, Scan}
import okay.llm.{Anthropic, OpenAi, Transport}

/**
 * A live model behind the `Model` effect. Everything in this library
 * above the effect — the compaction algebra, the search strategies,
 * grounded recall, the durable journal — was written against a
 * scripted handler; this is the handler that makes the same programs
 * talk to a real provider, and nothing above it changes.
 *
 * The protocol is OpenAI-compatible, which is what OpenAI, Groq,
 * Together, OpenRouter, Fireworks and the local runtimes (Ollama,
 * vLLM, llama.cpp) all serve — one handler, most of the market. Only
 * the socket is mocked in the tests; the request and response shapes
 * are the real ones.
 *
 * Loom is why this can be a comonadic handler at all: `handle` must
 * answer a value, so the async program runs to completion inside it
 * and a virtual thread parks on the wire. That is the same trade the
 * interop modules make, and it is stated rather than hidden.
 */
object Provider {

  /** turns as the wire wants them */
  def message(t: Turn): Json = t match
    case Turn.System(s) => OpenAi.message("system", s)
    case Turn.User(s) => OpenAi.message("user", s)
    case Turn.Assistant(s, calls) =>
      OpenAi.message("assistant", s, calls = calls.map(c =>
        (c.id, c.name, Json.print(c.args))))
    case Turn.Result(id, c) => OpenAi.message("tool", c, toolCallId = Some(id))
    // a compaction marker is context, and context is a system turn
    case Turn.Summary(s, _) => OpenAi.message("system", s)
    // Compact.skillState folds this into a rendered Turn.User before
    // `present` ever hands the context here; a raw one reaching the
    // wire is a caller bypassing that, so it goes as context, plainly
    case Turn.StatePatch(patch) => OpenAi.message("system", Json.print(patch))

  def declaration(spec: ToolSpec): Json =
    OpenAi.tool(spec.name, spec.description, spec.schema)

  /** what one response means to the agent: the text and the calls */
  def reply(r: OpenAi.Response): Reply =
    r.choices.headOption.flatMap(_.message) match
      case None => Reply("", Nil)
      case Some(m) =>
        val calls = m.tool_calls.getOrElse(Nil).map { c =>
          // OpenAI passes arguments as a STRING of JSON; our parser is
          // total, so a truncated one becomes a value with holes
          // rather than a failure here
          ToolCall(c.id, c.function.name, Json.parse(c.function.arguments))
        }
        Reply(m.content.getOrElse(""), calls)

  /**
   * The handler. `count` is local (a BPE Scan, no provider call) so
   * the compaction budget never costs a round trip.
   */
  def openAi(transport: Transport, apiKey: String, model: String,
             url: String = OpenAi.chatUrl,
             maxTokens: Option[Int] = Some(1024),
             count: String => Int = _.length / 4)
            (using okay.Handler[okay.Async]): Handler[Model] = new:
    def handle[A](e: Model[A]): A = e match
      case Model.Complete(context, tools) =>
        val body = OpenAi.request(model,
          context.map(message), tools.map(declaration),
          stream = false, maxTokens)
        // the virtual thread parks here; the row above stays a program
        reply(OpenAi.complete(transport, apiKey, body, url).runWith)
      case Model.Count(text) => count(text)

  // ------------------------------------------------------------------
  // the SAME effect, a genuinely different wire
  //
  // This is the seam's test rather than a convenience: Anthropic's
  // Messages API puts `system` at the top level instead of in the
  // message list, carries content as typed BLOCKS instead of a
  // string, names a tool's schema `input_schema` instead of
  // `parameters`, delivers tool arguments as a JSON object instead of
  // a string of JSON, and expects tool results back as a USER message
  // of tool_result blocks — several of them merged into one when
  // calls were parallel. None of that reached `Model`: the whole
  // mapping lives in this handler, which is what "policy lives in the
  // handler" is supposed to mean.

  /** turns to the Messages API's shape: the system prompt is lifted
   * out, and consecutive tool results merge into one user message */
  def anthropicMessages(context: Seq[Turn]): (Option[String], Seq[Json]) =
    val system = context.collect {
      case Turn.System(s) => s
      case Turn.Summary(s, _) => s
    }
    // a StatePatch normally never reaches here — Compact.skillState's
    // `present` already folded it into a Turn.User — so this treats
    // one the same as the summary marker just above: context, not
    // conversation
    val out = Vector.newBuilder[Json]
    var pendingResults = Vector.empty[Json]

    def flushResults(): Unit =
      if pendingResults.nonEmpty then
        out += Anthropic.blocks("user", pendingResults)
        pendingResults = Vector.empty

    for t <- context do t match
      case Turn.System(_) | Turn.Summary(_, _) | Turn.StatePatch(_) => ()
      case Turn.User(s) =>
        flushResults()
        out += Anthropic.blocks("user", Vector(Anthropic.textBlock(s)))
      case Turn.Assistant(s, calls) =>
        flushResults()
        val bs = (if s.nonEmpty then Vector(Anthropic.textBlock(s)) else Vector.empty) ++
          calls.map(c => Anthropic.toolUseBlock(c.id, c.name, c.args))
        if bs.nonEmpty then out += Anthropic.blocks("assistant", bs)
      case Turn.Result(id, c) =>
        pendingResults = pendingResults :+ Anthropic.toolResultBlock(id, c)

    flushResults()
    (Option(system.mkString("\n")).filter(_.nonEmpty), out.result())

  /** the handler: the same programs, the other protocol */
  def anthropic(transport: Transport, apiKey: String, model: String,
                url: String = Anthropic.messagesUrl,
                maxTokens: Int = 1024,
                count: String => Int = _.length / 4)
               (using okay.Handler[okay.Async]): Handler[Model] = new:
    def handle[A](e: Model[A]): A = e match
      case Model.Complete(context, tools) =>
        val (system, messages) = anthropicMessages(context)
        val body = Anthropic.messagesBody(model, system, messages,
          tools.map(t => Anthropic.toolDecl(t.name, t.description, t.schema)),
          maxTokens)
        val a = Anthropic.message(transport, apiKey, body, url).runWith
        // `input` is already an object here, so nothing to re-parse
        Reply(a.text, a.calls.map((id, name, input) => ToolCall(id, name, input)))
      case Model.Count(text) => count(text)

  // ------------------------------------------------------------------
  // the portable form
  //
  // A `Handler[Model]` must ANSWER with a value, so it has to run the
  // request to completion inside itself — which needs a thread that
  // can park, and JS has none. The portable shape therefore is not a
  // handler but a RELAY: the Model operations are translated into
  // Async ones and the program comes back as `A ! (Async + F)`, which
  // the JVM runs by parking (runWith) and JS runs by driving the
  // event loop (Async.runAsync). Same program, both platforms; the
  // comonadic handler above stays as the JVM convenience.

  def relay[A, F[+_]](complete: (Seq[Turn], Seq[ToolSpec]) => Reply ! okay.Async,
                                       count: String => Int = _.length / 4)
                                      (prog: A ! (Model + F)): A ! (okay.Async + F) =
    // the handler as a NATURAL TRANSFORMATION into another row: a
    // Model operation answers with a PROGRAM in Async, which is what
    // Handler[Model] = Model ==> Id could not express — Id has
    // nowhere to put the suspension
    // a row is a union, so (Model + F) + Async IS Model + (Async + F):
    // the ascription is the compiler's own equality, not a cast
    val widened: A ! (Model + (okay.Async + F)) = okay.!.widen[A, Model + F, okay.Async](prog)
    okay.!.translate[A, Model, okay.Async + F](widened) {
      [X] => (e: Model[X]) => e match
        // covariant row: X >: the case's answer, and `!` is invariant —
        // so the answer is lifted to X (a map, not a cast)
        case Model.Complete(ctx, tools) => okay.!.widen[Reply, okay.Async, F](complete(ctx, tools)).map[X](r => r)
        case Model.Count(text) => okay.pure[okay.Async + F, X](count(text))
    }

  /** the OpenAI-compatible provider as a relay — the cross-platform door */
  def openAiRelay[A, F[+_]](
      transport: Transport, apiKey: String, model: String,
      url: String = OpenAi.chatUrl, maxTokens: Option[Int] = Some(1024),
      count: String => Int = _.length / 4)(prog: A ! (Model + F)): A ! (okay.Async + F) =
    relay[A, F]((ctx, tools) =>
      OpenAi.complete(transport, apiKey,
        OpenAi.request(model, ctx.map(message), tools.map(declaration),
          stream = false, maxTokens), url).map(reply), count)(prog)

  /** the local tokenizer as the counter, when a dictionary is at hand */
  def counting(bpe: Bpe): String => Int = s =>
    Scan.all(bpe)(s).tokens.count(_.channel == okay.lex.Channel.Syntax)
}
