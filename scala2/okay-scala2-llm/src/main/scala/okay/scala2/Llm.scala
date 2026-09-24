package okay.scala2

import okay.{!, Writer, %, +}
import okay.given
import okay.codec.Schema
import okay.llm.{Anthropic, OpenAi, Structured, Transport, Transports}

/**
 * okay-llm for Scala 2.13 (specs/scala2-facade.md, stage 15.5).
 *
 * okay-scala2-agent already holds a model for a CONVERSATION (`Model`,
 * `Chat`). This is the layer under it: a completion as a stream of text
 * tokens, and a typed value pulled out of such a stream. Scala 2 cannot
 * use okay-llm's own doors because each answers a program (`Unit !
 * (Writer % String + Async)`), and it cannot IMPLEMENT a `Transport`
 * for the same reason; `Llm.transport` takes a function answering a
 * `Source` instead.
 */
object Llm {

  /** okay-llm's JVM transport: java.net.http, the response read line
   * by line as it arrives */
  def http: Transport = Transports.http()

  /** a transport from a function: each POST (url, headers, body)
   * answers the response's lines. A scripted server in a test, or any
   * HTTP client a Scala 2 program already has. */
  def transport(post: (String, Map[String, String], String) => Source[String]): Transport = {
    val f = post
    new Transport {
      def post(url: String, headers: Map[String, String], body: String): Unit ! (Writer % String + okay.Async) =
        f(url, headers, body).core
    }
  }

  /** an Anthropic Messages completion as text tokens, in order;
   * `messages` are (role, content) pairs, role "user" or "assistant" */
  def anthropic(transport: Transport, apiKey: String, model: String, messages: Seq[(String, String)],
                maxTokens: Int = 1024, url: String = "https://api.anthropic.com/v1/messages"): Source[String] =
    Source.of(Anthropic.stream(transport, apiKey,
      Anthropic.Request(model, maxTokens, messages.map(m => Anthropic.Message(m._1, m._2)).toList, stream = true), url))

  /** an OpenAI chat completion (or any server speaking its API) as text
   * tokens; roles "system", "user", "assistant" */
  def openAi(transport: Transport, apiKey: String, model: String, messages: Seq[(String, String)],
             maxTokens: Option[Int] = None, url: String = OpenAi.chatUrl): Source[String] =
    Source.of(OpenAi.stream(transport, apiKey,
      OpenAi.request(model, messages.map(m => OpenAi.message(m._1, m._2)), stream = true, maxTokens = maxTokens), url))

  /** pull `tokens` until the text so far decodes as an `A`, and STOP
   * there: the rest of the completion is never read (and never paid
   * for). None if the stream ended first. */
  def first[A](tokens: Source[String])(implicit schema: Schema[A]): Eff[Async, Option[A]] =
    Async(Structured.first[A](tokens.core))

  /** the same walk, with what it cost: the value, the text consumed,
   * the tokens read, and whether it stopped before the stream ended */
  def cut[A](tokens: Source[String])(implicit schema: Schema[A]): Eff[Async, Structured.Cut[A]] =
    Async(Structured.cut[A](tokens.core))
}
