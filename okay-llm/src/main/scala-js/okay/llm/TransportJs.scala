package okay.llm

import okay.{Async, Web}

import okay.freer.{%, +}
import okay.freer.{!, Writer, effect}
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

  def fetch: Transport & Fetch = new Transport with Fetch:
    def post(url: String, headers: Map[String, String], body: String)
    : Unit ! Writer % String + Async = send("POST", url, headers, Some(body))
    def get(url: String, headers: Map[String, String]): Unit ! Writer % String + Async =
      send("GET", url, headers, None)
    def delete(url: String, headers: Map[String, String], body: String): Unit ! Writer % String + Async =
      send("DELETE", url, headers, Some(body))

    private def send(method: String, url: String, headers: Map[String, String], body: Option[String])
    : Unit ! Writer % String + Async =
      type F = Writer % String + Async
      // Async.await, not the success-only `await`: a rejected fetch
      // (an unreachable host, a DNS failure, a CORS refusal) used to
      // call nothing at all, and the program waited for a callback
      // that would never come. The Left is the row's error channel,
      // which is where a failed request belongs.
      val request = Async.await[String] { k =>
        val opts = new Web.RequestInit {}
        opts.method = method
        opts.headers = js.Dictionary(headers.toSeq*)
        body.foreach(b => opts.body = b)
        Web.Global.fetch(url, opts)
          .`then`[String]((r: Web.Response) => r.text())
          .`then`((t: String) => { k(Right(t)); () }: Unit)
          .`catch`((e: Any) => {
            k(Left(js.JavaScriptException(e))); ()
          }: Unit | js.Thenable[Unit]): Unit
        () => ()      // fetch is not cancellable here; nothing to undo
      }
      okay.freer.!.widen[String, Async, Writer % String](request).flatMap { text =>
        okay.freer.!.each(text.split("\n").toIndexedSeq)(line => effect[F, Unit](Writer(line)))
      }
