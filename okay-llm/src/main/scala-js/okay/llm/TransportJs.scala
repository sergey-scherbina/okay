package okay.llm

import okay.{!, %, +, Async, Writer, await, effect, pure}
import scala.scalajs.js

/**
 * The JS transport: `fetch`, awaited through the Async effect's
 * callback form — which is why the same programs run here. Nothing
 * parks: the promise's completion re-enters the drive, and the body
 * is delivered as its lines, exactly as the JVM transport delivers
 * them.
 *
 * Streaming a response body incrementally is possible here too (the
 * reader of `response.body`), and is the stated next step; the whole
 * body first keeps the two platforms' semantics identical while the
 * seam is being proven.
 */
object Transports:

  def fetch: Transport = new Transport:
    def post(url: String, headers: Map[String, String], body: String)
    : Unit ! (Writer % String + Async) =
      type F = Writer % String + Async
      val request = await[String] { k =>
        val opts = js.Dynamic.literal(
          method = "POST",
          headers = js.Dictionary(headers.toSeq*).asInstanceOf[js.Any],
          body = body)
        js.Dynamic.global.fetch(url, opts)
          .asInstanceOf[js.Promise[js.Dynamic]]
          .`then`((r: js.Dynamic) => r.text().asInstanceOf[js.Promise[String]])
          .asInstanceOf[js.Promise[String]]
          .`then`((t: String) => { k(t); () }: Unit)
        ()
      }
      okay.!.widen[String, Async, Writer % String](request).flatMap { text =>
        text.split("\n").foldLeft(pure[F, Unit](()))((acc, line) =>
          acc.flatMap(_ => effect[F, String](Writer(line)).map(_ => ())))
      }
