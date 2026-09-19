package okay.ui

/** The browser side of a live page: a dependency-free patch consumer
 * speaking `Protocol` -- the derived tree/patch/event shapes,
 * the same `React.elem` DOM structure (so a patch path walks the
 * same `childNodes`), the same delegated-listener event mapping the
 * Scala.js `Dom` backend uses, in ~100 lines of plain JavaScript
 * served at `Live.JsPath`. Hand-written rather than linked from
 * Scala.js so that a server needs no build step and no artifact: the
 * page IS the deployment.
 *
 * It lives here rather than in okay-script (specs/ui-html.md stage 2)
 * for the reason `Html` does: it is the browser's CLIENT of this
 * module's protocol, and a product with its own WebSocket route needs
 * it without the container. okay-script still serves it at
 * `Live.JsPath`.
 */
object LiveJs:

  /**
   * The client, printed from `Client`'s tree (specs/js.md).
   *
   * This used to be a 290-line JavaScript string literal. It is the
   * same program and the same deployment — one string in the jar, no
   * build step, no artifact — but it is now BUILT rather than typed,
   * so it is composable, diffable, and every literal in it is escaped
   * by okay-js's printer rather than by whoever wrote the line.
   *
   * `Client.program` is the value; this is the text. Nothing else
   * changed for a caller: `LiveJs.source` is still what a server
   * serves.
   */
  val source: String = Client.source
