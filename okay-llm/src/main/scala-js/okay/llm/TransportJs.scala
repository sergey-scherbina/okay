package okay.llm

import okay.{!, %, +, Async, Writer, effect, pure}
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
      // Async.await, not the success-only `await`: a rejected fetch
      // (an unreachable host, a DNS failure, a CORS refusal) used to
      // call nothing at all, and the program waited for a callback
      // that would never come. The Left is the row's error channel,
      // which is where a failed request belongs.
      val request = Async.await[String] { k =>
        val opts = js.Dynamic.literal(
          method = "POST",
          headers = js.Dictionary(headers.toSeq*).asInstanceOf[js.Any],
          body = body)
        js.Dynamic.global.fetch(url, opts)
          .asInstanceOf[js.Promise[js.Dynamic]]
          .`then`((r: js.Dynamic) => r.text().asInstanceOf[js.Promise[String]])
          .asInstanceOf[js.Promise[String]]
          .`then`((t: String) => { k(Right(t)); () }: Unit)
          .asInstanceOf[js.Promise[Unit]]
          .`catch`((e: Any) => {
            k(Left(js.JavaScriptException(e.asInstanceOf[js.Any]))); ()
          }: Unit | js.Thenable[Unit]): Unit
        () => ()      // fetch is not cancellable here; nothing to undo
      }
      okay.!.widen[String, Async, Writer % String](request).flatMap { text =>
        text.split("\n").foldLeft(pure[F, Unit](()))((acc, line) =>
          acc.flatMap(_ => effect[F, Unit](Writer(line)).map(_ => ())))
      }
