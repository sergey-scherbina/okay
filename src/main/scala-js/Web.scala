package okay

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Uint8Array

/**
 * The web-standard globals this stack uses, TYPED once (typed-js-
 * facades, 2026-09-02): `fetch` with its Response, headers and body
 * reader, and `WebSocket` with its events — `js.native` facades over
 * what Node (fetch since 18, WebSocket since 22) and browsers provide.
 * Still no scala-js-dom (the dependency rule in specs/http.md holds);
 * what changed is that the shape of the API is stated here, in types,
 * instead of cast at every use in okay-http and okay-llm.
 */
object Web {

  /** the init object `fetch` takes: a plain JS object built here */
  trait RequestInit extends js.Object:
    var method: js.UndefOr[String] = js.undefined
    var headers: js.UndefOr[js.Dictionary[String]] = js.undefined
    var body: js.UndefOr[js.Any] = js.undefined

  @js.native
  trait Headers extends js.Object:
    /** the callback receives (value, key) — the web order */
    def forEach(f: js.Function2[String, String, Unit]): Unit = js.native

  @js.native
  trait ReadResult extends js.Object:
    val done: Boolean = js.native
    val value: js.UndefOr[Uint8Array] = js.native

  @js.native
  trait Reader extends js.Object:
    def read(): js.Promise[ReadResult] = js.native

  @js.native
  trait ReadableStream extends js.Object:
    def getReader(): Reader = js.native

  @js.native
  trait Response extends js.Object:
    val status: Int = js.native
    val headers: Headers = js.native
    val body: ReadableStream = js.native
    def text(): js.Promise[String] = js.native

  @js.native
  trait MessageEvent extends js.Object:
    /** a string for a text frame, an ArrayBuffer for a binary one
     * (binaryType = "arraybuffer") — told apart by a type TEST, which
     * is why it is declared as Any: `case s: String` is a real test
     * on a JS value, and js.Any would make it "unreachable" */
    val data: Any = js.native

  @js.native
  trait CloseEvent extends js.Object:
    val code: Int = js.native
    val reason: String = js.native

  @js.native
  @JSGlobal("WebSocket")
  class WebSocket(url: String, protocols: js.UndefOr[js.Array[String]] = js.native) extends js.Object:
    var binaryType: String = js.native
    var onopen: js.Function1[js.Any, Unit] = js.native
    var onmessage: js.Function1[MessageEvent, Unit] = js.native
    var onclose: js.Function1[CloseEvent, Unit] = js.native
    var onerror: js.Function1[js.Any, Unit] = js.native
    def send(data: String): Unit = js.native
    def send(data: Uint8Array): Unit = js.native
    def close(): Unit = js.native
    def close(code: Int, reason: String): Unit = js.native

  @js.native
  @JSGlobalScope
  object Global extends js.Object:
    def fetch(url: String, init: RequestInit): js.Promise[Response] = js.native
}
