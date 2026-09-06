package okay.script.api

import okay.*
import okay.given
import okay.ui.{Elem, Event, React, Ui, Wire}

/** A server-driven okay-ui app a page declares -- `Wire.serve`'s
 * three arguments, held as a value so the page can `mount` it and
 * the container can run a session for every WebSocket that connects.
 * See specs/okay-script.md "Live pages".
 *
 * Declare it once, at object level (a ```scala declare block), so the
 * app -- like a JSP declaration -- is built per compile, not per
 * request; mount it in prose with `${mount("counter", counter)}`.
 */
final class Live[S](val init: S, val view: S => Ui, val update: (S, Event) => S):
  /** the tree a fresh session shows first -- also the SSR content */
  def first: Ui = view(init)

  /** one session: event lines in, tree/patch lines out (okay-ui's
   * pure `Wire.serve`), the final state discarded */
  def session: Stage[String, String, Unit] =
    Wire.serve(init)(view)(update).map(_ => ())

object Live:
  def apply[S](init: S)(view: S => Ui)(update: (S, Event) => S): Live[S] =
    new Live(init, view, update)

  /** where the container serves the patch consumer */
  val JsPath = "/__okay/live.js"

  /** the tree as HTML, server-side -- the SAME structure `React.elem`
   * builds and the browser's patch consumer navigates, so a path into
   * one is a path into the other. A page is complete without any
   * script: what the browser shows first is this. */
  def html(ui: Ui): String =
    val sb = new StringBuilder
    render(React.elem(ui), sb)
    sb.toString

  private def render(e: Elem, sb: StringBuilder): Unit =
    sb ++= "<" ++= e.tag: Unit
    for (k, v) <- e.props do
      k match
        case "className" => attr(sb, "class", v)
        case "checked" => if v == "true" then sb ++= " checked": Unit
        case _ => attr(sb, k, v)
    sb ++= ">": Unit
    if e.tag != "input" then
      e.text.foreach(t => sb ++= escape(t): Unit)
      e.children.foreach(render(_, sb))
      sb ++= "</" ++= e.tag ++= ">": Unit

  private def attr(sb: StringBuilder, k: String, v: String): Unit =
    sb ++= " " ++= k ++= "=\"" ++= escape(v) ++= "\"": Unit

  def escape(s: String): String =
    val sb = new StringBuilder
    s.foreach {
      case '&' => sb ++= "&amp;": Unit
      case '<' => sb ++= "&lt;": Unit
      case '>' => sb ++= "&gt;": Unit
      case '"' => sb ++= "&quot;": Unit
      case c => sb += c: Unit
    }
    sb.toString

/** Mounts a Live app here: its first tree as HTML (so the page is
 * whole without JavaScript), and the script that opens this page's
 * own WebSocket (`?__live=<id>`) to receive patches and send events.
 * Outside a `Site` (a bare `render`) the HTML still renders; there is
 * just no container to answer the socket. */
def mount(id: String, app: Live[?]): String =
  Container.liveRegistrar.foreach(register => register(id, app))
  val safe = Live.escape(id)
  s"""<div id="okay-live-$safe" data-okay-live="$safe">${Live.html(app.first)}</div>""" +
    s"""<script src="${Live.JsPath}"></script><script>okayLive("$safe")</script>"""
